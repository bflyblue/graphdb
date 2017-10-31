{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
    ( Db(..)
    , View(..)
    , Connection(..)
    , Node(..)
    , Relation(..)
    , Properties
    , Value(..)
    , properties
    , propertyEq
    , connect
    , emptyDb
    , viewDb
    , getDb
    , diffDb
    , printDb
    , printView
    , printChangeSet
    , transaction
    , commit
    , addNode
    , addRelation
    , deleteNode
    , deleteRelation
    , updateNode
    , updateRelation
    ) where

import           Control.Concurrent.STM
import           Control.Monad
import           Data.Hashable
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HashMap
import           Data.IntMap.Strict     (IntMap)
import qualified Data.IntMap.Strict     as IntMap
import           Data.IntSet            (IntSet)
import qualified Data.IntSet            as IntSet
import           Data.List              (sort, sortOn)
import           Data.Maybe
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           GHC.Generics

type Properties = HashMap Field Value

properties :: [(Field, Value)] -> Properties
properties = HashMap.fromList

type Field = Text

data Value
  = S Text
  | I Int
  | B Bool
  deriving (Show, Eq, Ord, Generic, Hashable)

type PropertyIndexes = HashMap Field Index

type Index = HashMap Value IntSet

propertyEq :: PropertyIndexes -> Field -> Value -> IntSet
propertyEq pidx f v = fromMaybe IntSet.empty $ do
  i <- HashMap.lookup f pidx
  HashMap.lookup v i

--

newtype Node = Node
  { nodeProperties :: Properties
  } deriving (Show)

data Relation = Relation
  { nodeA              :: Id Node
  , nodeB              :: Id Node
  , relationProperties :: Properties
  } deriving (Show)

type Id a = Int

--

data Db = Db
  { nodes          :: IntMap Node      -- ^ map from node id to node
  , relations      :: IntMap Relation  -- ^ map from relation id to relation
  , versions       :: IntMap Version   -- ^ history of the database
  , nodeIndex      :: PropertyIndexes  -- ^ index for node properties
  , relationIndex  :: PropertyIndexes  -- ^ index for node properties
  , relationAIndex :: IntMap IntSet   -- ^ index for relations from `a`
  , relationBIndex :: IntMap IntSet   -- ^ index for relations to `b`
  , nodeseq        :: Int              -- ^ next available node id
  , relationseq    :: Int              -- ^ next available relation id
  , versionseq     :: Int              -- ^ next available version number
  } deriving (Show)

emptyDb :: Db
emptyDb = Db
          { nodes = mempty
          , relations = mempty
          , versions = IntMap.singleton 0 emptyVersion
          , nodeIndex = mempty
          , relationIndex = mempty
          , relationAIndex = mempty
          , relationBIndex = mempty
          , nodeseq = 0
          , relationseq = 0
          , versionseq = 1
          }

headDb :: Db -> Version
headDb db = versionDb db (pred $ versionseq db)

versionDb :: Db -> Int -> Version
versionDb Db{..} vid = IntMap.findWithDefault (error "No such version") vid versions

diffDb :: Db -> Int -> Int -> ChangeSet
diffDb db a b =
  let va = versionDb db a
      vb = versionDb db b
  in  ChangeSet
      { nodesAdded = IntSet.difference (selectedNodes vb) (selectedNodes va)
      , nodesDeleted = IntSet.difference (selectedNodes va) (selectedNodes vb)
      , relationsAdded = IntSet.difference (selectedRelations vb) (selectedRelations va)
      , relationsDeleted = IntSet.difference (selectedRelations va) (selectedRelations vb)
      }

data View = View
  { viewNodes     :: IntMap Node      -- ^ map from node id to node
  , viewRelations :: IntMap Relation  -- ^ map from relation id to relation
  } deriving (Show)

viewDb :: Db -> Int -> View
viewDb db vid =
  let ver = versionDb db vid
  in  View { viewNodes = restrictKeys (nodes db) (selectedNodes ver)
           , viewRelations = restrictKeys (relations db) (selectedRelations ver)
           }
  where
    restrictKeys m s = IntMap.filterWithKey (\k _ -> k `IntSet.member` s) m
{- requires containers 0.5.8+
    restrictKeys = IntMap.restrictKeys
-}


newtype Connection = Connection
  { connDb :: TVar Db
  }

getDb :: Connection -> IO Db
getDb = readTVarIO . connDb

printDb :: Db -> IO ()
printDb Db{..} = do
  putStrLn "Database:"
  putStrLn " ---- Nodes ----"
  forM_ (IntMap.toAscList nodes) $ \(nid, Node p) ->
    putStrLn $ unwords ["  n" ++ show nid ++ ":", props p]
  putStrLn " ---- Relations ----"
  forM_ (IntMap.toAscList relations) $ \(rid, Relation a b p) ->
    putStrLn $ unwords ["  r" ++ show rid ++ ":", "n" ++ show a ++ " -> n" ++ show b, props p]
  putStrLn " ---- Node Index ----"
  forM_ (sortOn fst $ HashMap.toList nodeIndex) $ \(field, idx) -> do
    putStrLn $ "  " ++ Text.unpack field ++ ":"
    forM_ (sortOn fst $ HashMap.toList idx) $ \(val, is) ->
      putStrLn $ "   " ++ showValue val ++ ": " ++
               unwords ( map (\i -> "n" ++ show i) (IntSet.toList is) )
  putStrLn " ---- Relation Index ----"
  forM_ (sortOn fst $ HashMap.toList relationIndex) $ \(field, idx) -> do
    putStrLn $ "  " ++ Text.unpack field ++ ":"
    forM_ (sortOn fst $ HashMap.toList idx) $ \(val, is) ->
      putStrLn $ "   " ++ showValue val ++ ": " ++
               unwords ( map (\i -> "r" ++ show i) (IntSet.toList is) )
  putStrLn " ---- Versions ----"
  forM_ (IntMap.toAscList versions) $ \(vid, Version nodeset relset) ->
    putStrLn $ "  v" ++ show vid ++ ": " ++
             unwords ( map (\i -> "n" ++ show i) (IntSet.toList nodeset)
                    ++ map (\i -> "r" ++ show i) (IntSet.toList relset) )
  putStrLn ""

printView :: View -> IO ()
printView View{..} = do
  putStrLn "View:"
  putStrLn " ---- Nodes ----"
  forM_ (IntMap.toAscList viewNodes) $ \(nid, Node p) ->
    putStrLn $ unwords ["  n" ++ show nid ++ ":", props p]
  putStrLn " ---- Relations ----"
  forM_ (IntMap.toAscList viewRelations) $ \(rid, Relation a b p) ->
    putStrLn $ unwords ["  r" ++ show rid ++ ":", "n" ++ show a ++ " -> n" ++ show b, props p]
  putStrLn ""

printChangeSet :: ChangeSet -> IO ()
printChangeSet ChangeSet{..} = do
  putStrLn "Diff:"
  putStrLn $ "  Added: " ++
           unwords ( map (\i -> "n" ++ show i) (IntSet.toList nodesAdded)
                  ++ map (\i -> "r" ++ show i) (IntSet.toList relationsAdded) )
  putStrLn $ "  Deleted: " ++
           unwords ( map (\i -> "n" ++ show i) (IntSet.toList nodesDeleted)
                  ++ map (\i -> "r" ++ show i) (IntSet.toList relationsDeleted) )
  putStrLn ""

props :: Properties -> String
props p =
  unwords $ ["{"]
          ++ concatMap pair (sort $ HashMap.toList p)
          ++ ["}"]

pair :: (Text, Value) -> [String]
pair (k, v) = [Text.unpack k, "=", showValue v]

showValue :: Value -> String
showValue (S a) = show a
showValue (I a) = show a
showValue (B a) = show a

data Transaction = Transaction
  { transConn    :: Connection
  , transChanges :: TVar ChangeSet
  }

data ChangeSet = ChangeSet
  { nodesAdded       :: IntSet
  , nodesDeleted     :: IntSet
  , relationsAdded   :: IntSet
  , relationsDeleted :: IntSet
  } deriving (Show)

emptyChangeSet :: ChangeSet
emptyChangeSet = ChangeSet
    { nodesAdded = mempty
    , nodesDeleted = mempty
    , relationsAdded = mempty
    , relationsDeleted = mempty
    }

nullChangeSet :: ChangeSet -> Bool
nullChangeSet ChangeSet{..} =
       IntSet.null nodesAdded
    && IntSet.null nodesDeleted
    && IntSet.null relationsAdded
    && IntSet.null relationsDeleted

instance Monoid ChangeSet where
  mempty = emptyChangeSet
  mappend a b = ChangeSet
    { nodesAdded = nodesAdded a <> nodesAdded b
    , nodesDeleted = nodesDeleted a <> nodesDeleted b
    , relationsAdded = relationsAdded a <> relationsAdded b
    , relationsDeleted = relationsDeleted a <> relationsDeleted b
    }

data Version = Version
  { selectedNodes     :: IntSet
  , selectedRelations :: IntSet
  } deriving (Show)

emptyVersion :: Version
emptyVersion = Version mempty mempty

--

connect :: Db -> IO Connection
connect db = Connection <$> newTVarIO db

transaction :: Connection -> (Transaction -> STM a) -> IO a
transaction conn act = do
  cs <- newTVarIO emptyChangeSet
  let t = Transaction { transConn = conn, transChanges = cs }
  atomically $ do
    a <- act t
    _ <- commit t
    return a

applyChanges :: ChangeSet -> Version -> Version
applyChanges ChangeSet{..} Version{..} =
  Version
  { selectedNodes = (selectedNodes `IntSet.union` nodesAdded) `IntSet.difference` nodesDeleted
  , selectedRelations = (selectedRelations `IntSet.union` relationsAdded) `IntSet.difference` relationsDeleted
  }

indexProperties :: Int -> Properties -> PropertyIndexes
indexProperties i = fmap (indexValue i)

indexValue :: Int -> Value -> Index
indexValue i v = HashMap.singleton v $ IntSet.singleton i

indexUnion :: PropertyIndexes -> PropertyIndexes -> PropertyIndexes
indexUnion = HashMap.unionWith (HashMap.unionWith IntSet.union)

{-
indexDifference :: PropertyIndexes -> PropertyIndexes -> PropertyIndexes
indexDifference = HashMap.differenceWith diffIndex
  where
    diffIndex a b =
      let is = HashMap.differenceWith diffValue a b
      in  if HashMap.null is
          then Nothing
          else Just is
    diffValue a b =
      let is = IntSet.difference a b
      in  if IntSet.null is
          then Nothing
          else Just is
-}

commit :: Transaction -> STM Int
commit Transaction{..} = do
  changes <- readTVar transChanges
  db@Db{..} <- readTVar (connDb transConn)
  if nullChangeSet changes
  then
    return $ pred versionseq
  else do
    let vid = versionseq
        ver = applyChanges changes (headDb db)
    writeTVar (connDb transConn)
      Db { versions = IntMap.insert vid ver versions
         , versionseq = succ versionseq
         , .. }
    writeTVar transChanges emptyChangeSet
    return vid

addNode :: Transaction -> Node -> STM (Id Node)
addNode Transaction{..} n = do
  Db{..} <- readTVar (connDb transConn)
  let nid = nodeseq
      idx = indexProperties nid (nodeProperties n)
  writeTVar (connDb transConn)
    Db { nodes = IntMap.insert nid n nodes
       , nodeseq = succ nodeseq
       , nodeIndex = indexUnion nodeIndex idx
       , .. }
  ChangeSet{..} <- readTVar transChanges
  writeTVar transChanges
    ChangeSet
    { nodesAdded = IntSet.insert nid nodesAdded
    , nodesDeleted = IntSet.delete nid nodesDeleted
    , .. }
  return nid

addRelation :: Transaction -> Relation -> STM (Id Relation)
addRelation Transaction{..} r@Relation{..} = do
  Db{..} <- readTVar (connDb transConn)
  let rid = relationseq
      idx = indexProperties rid relationProperties
  writeTVar (connDb transConn)
    Db { relations = IntMap.insert rid r relations
       , relationseq = succ relationseq
       , relationIndex = indexUnion relationIndex idx
       , relationAIndex = IntMap.insertWith IntSet.union nodeA (IntSet.singleton rid) relationAIndex
       , relationBIndex = IntMap.insertWith IntSet.union nodeB (IntSet.singleton rid) relationBIndex
       , .. }
  ChangeSet{..} <- readTVar transChanges
  writeTVar transChanges
    ChangeSet
    { relationsAdded = IntSet.insert rid relationsAdded
    , relationsDeleted = IntSet.delete rid relationsDeleted
    , .. }
  return rid

deleteNode :: Transaction -> Id Node -> STM ()
deleteNode Transaction{..} nid = do
  ChangeSet{..} <- readTVar transChanges
  writeTVar transChanges
    ChangeSet
    { nodesDeleted = IntSet.insert nid nodesDeleted
    , nodesAdded = IntSet.delete nid nodesAdded
    , .. }

deleteRelation :: Transaction -> Id Relation -> STM ()
deleteRelation Transaction{..} rid = do
  ChangeSet{..} <- readTVar transChanges
  writeTVar transChanges
    ChangeSet
    { relationsDeleted = IntSet.insert rid relationsDeleted
    , relationsAdded = IntSet.delete rid relationsAdded
    , .. }

updateNode :: Transaction -> Id Node -> Node -> STM (Id Node)
updateNode t@Transaction{..} nid n = do
  deleteNode t nid
  nid' <- addNode t n
  db@Db{..} <- readTVar (connDb transConn)
  let ver   = headDb db
      rfrom = fromMaybe IntSet.empty (IntMap.lookup nid relationAIndex) `IntSet.intersection` selectedRelations ver
      rto   = fromMaybe IntSet.empty (IntMap.lookup nid relationBIndex) `IntSet.intersection` selectedRelations ver
  forM_ (IntSet.toList rfrom) $ \rid ->
    case IntMap.lookup rid relations of
      Just (Relation _ b prop) -> void $ updateRelation t rid (Relation nid' b prop)
      Nothing -> return ()  -- error?
  forM_ (IntSet.toList rto) $ \rid ->
    case IntMap.lookup rid relations of
      Just (Relation a _ prop) -> void $ updateRelation t rid (Relation a nid' prop)
      Nothing -> return ()  -- error?
  return nid'

updateRelation :: Transaction -> Id Relation -> Relation -> STM (Id Relation)
updateRelation t rid r = do
  deleteRelation t rid
  addRelation t r
