{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

main :: IO ()
main = do
  c <- connect emptyDb

  (v1, n1, n2, r1) <- transaction c $ \t -> do
    n1 <- addNode t $ Node $ properties [("name", S "Node 1")]
    n2 <- addNode t $ Node $ properties [("name", S "Node 2")]
    r1 <- addRelation t $ Relation n1 n2 $ properties [("name", S "Relation 1")]
    v1 <- commit t
    return (v1, n1, n2, r1)

  (v2, n3) <- transaction c $ \t -> do
    n3 <- addNode t $ Node $ properties [("name", S "Node 3")]
    n4 <- addNode t $ Node $ properties [("name", S "Node 4")]
    _r2 <- addRelation t $ Relation n3 n4 $ properties [("name", S "Relation 2")]
    v2 <- commit t
    return (v2, n3)

  v3 <- transaction c $ \t -> do
    deleteNode t n1
    deleteNode t n2
    deleteRelation t r1
    commit t

  v4 <- transaction c $ \t -> do
    _n3' <- updateNode t n3 $ Node $ properties [("name", S "Node 3 updated")]
    commit t

  db <- getDb c

  printDb db
  printView $ viewDb db v1
  printView $ viewDb db v2
  printView $ viewDb db v3
  printView $ viewDb db v4
  printChangeSet $ diffDb db v1 v3
  printChangeSet $ diffDb db v3 v4
