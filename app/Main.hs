{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

main :: IO ()
main = do
  c <- connect emptyDb

  v1 <- transaction c $ \t -> do
    n1 <- addNode t $ Node $ properties [("name", S "Node 1")]
    n2 <- addNode t $ Node $ properties [("name", S "Node 2")]
    r1 <- addRelation t $ Relation n1 n2 $ properties [("name", S "Relation 1")]
    commit t

  v2 <- transaction c $ \t -> do
    n3 <- addNode t $ Node $ properties [("name", S "Node 3")]
    n4 <- addNode t $ Node $ properties [("name", S "Node 4")]
    r2 <- addRelation t $ Relation n3 n4 $ properties [("name", S "Relation 2")]
    commit t

  db <- getDb c
  printDb db
  printView $ viewDb db v1
  printView $ viewDb db v2
