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
    , connect
    , emptyDb
    , viewDb
    , viewDbHead
    , getDb
    , printDb
    , printView
    , transaction
    , commit
    , addNode
    , addRelation
    , deleteNode
    , deleteRelation
    ) where

import           Control.Concurrent.STM
import           Control.Monad
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HashMap
import           Data.IntMap.Strict     (IntMap)
import qualified Data.IntMap.Strict     as IntMap
import           Data.IntSet            (IntSet)
import qualified Data.IntSet            as IntSet
import           Data.List              (sort)
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as Text

type Properties = HashMap Field Value

properties :: [(Field, Value)] -> Properties
properties = HashMap.fromList

type Field = Text

data Value
  = S Text
  | I Int
  | B Bool
  deriving (Show, Eq, Ord)

type PropertyIndexes = HashMap Field Index

type Index = HashMap Value IntSet

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
  { nodes         :: IntMap Node      -- ^ map from node id to node
  , relations     :: IntMap Relation  -- ^ map from relation id to relation
  , versions      :: IntMap Version   -- ^ history of the database
  , nodeIndex     :: PropertyIndexes  -- ^ index for node properties
  , relationIndex :: PropertyIndexes  -- ^ index for node properties
  , nodeseq       :: Int              -- ^ next available node id
  , relationseq   :: Int              -- ^ next available relation id
  , versionseq    :: Int              -- ^ next available version number
  } deriving (Show)

emptyDb :: Db
emptyDb = Db mempty mempty (IntMap.singleton 0 emptyVersion) mempty mempty 0 0 1

headDb :: Db -> Version
headDb db = versionDb db (pred $ versionseq db)

versionDb :: Db -> Int -> Version
versionDb Db{..} vid = IntMap.findWithDefault (error "No such version") vid versions

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

viewDbHead :: Db -> View
viewDbHead db = viewDb db (pred $ versionseq db)

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
  putStrLn " ---- Versions ----"
  forM_ (IntMap.toAscList versions) $ \(vid, Version nodeset relset) ->
    putStrLn $ "  v" ++ show vid ++ ": " ++
               unwords ( map (\i -> "n" ++ show i) (IntSet.toList nodeset)
                      ++ map (\i -> "r" ++ show i) (IntSet.toList relset) )

printView :: View -> IO ()
printView View{..} = do
  putStrLn "View:"
  putStrLn " ---- Nodes ----"
  forM_ (IntMap.toAscList viewNodes) $ \(nid, Node p) ->
    putStrLn $ unwords ["  n" ++ show nid ++ ":", props p]
  putStrLn " ---- Relations ----"
  forM_ (IntMap.toAscList viewRelations) $ \(rid, Relation a b p) ->
    putStrLn $ unwords ["  r" ++ show rid ++ ":", "n" ++ show a ++ " -> n" ++ show b, props p]

props :: Properties -> String
props p =
  unwords $ ["{"]
          ++ concatMap pair (sort $ HashMap.toList p)
          ++ ["}"]

pair :: (Text, Value) -> [String]
pair (k, v) = [Text.unpack k, "=", show v]

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
  writeTVar (connDb transConn)
    Db
    { nodes = IntMap.insert nid n nodes
    , nodeseq = succ nodeseq
    , .. }
  ChangeSet{..} <- readTVar transChanges
  writeTVar transChanges
    ChangeSet
    { nodesAdded = IntSet.insert nid nodesAdded
    , nodesDeleted = IntSet.delete nid nodesDeleted
    , .. }
  return nid

addRelation :: Transaction -> Relation -> STM (Id Relation)
addRelation Transaction{..} r = do
  Db{..} <- readTVar (connDb transConn)
  let rid = relationseq
  writeTVar (connDb transConn)
    Db { relations = IntMap.insert rid r relations
       , relationseq = succ relationseq
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
  Db{..} <- readTVar (connDb transConn)
  writeTVar (connDb transConn)
    Db
    { nodes = IntMap.delete nid nodes
    , nodeseq = succ nodeseq
    , .. }
  ChangeSet{..} <- readTVar transChanges
  writeTVar transChanges
    ChangeSet
    { nodesDeleted = IntSet.insert nid nodesDeleted
    , nodesAdded = IntSet.delete nid nodesAdded
    , .. }

deleteRelation :: Transaction -> Id Relation -> STM ()
deleteRelation Transaction{..} rid = do
  Db{..} <- readTVar (connDb transConn)
  writeTVar (connDb transConn)
    Db { relations = IntMap.delete rid relations
       , relationseq = succ relationseq
       , .. }
  ChangeSet{..} <- readTVar transChanges
  writeTVar transChanges
    ChangeSet
    { relationsDeleted = IntSet.insert rid relationsDeleted
    , relationsAdded = IntSet.delete rid relationsAdded
    , .. }
