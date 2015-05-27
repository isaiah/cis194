{-# OPTIONS_GHC -Wall #-}

module Ch02 where

import Log

parseMessage :: String -> LogMessage
parseMessage s =
  parseLine $ words s
  where
    parseLine arr =
      case arr of
        ("E" : sev : ts : contents) ->
          LogMessage (Error (read sev :: Int)) (read ts :: Int) (unwords contents)
        ("I" : ts : contents) ->
          LogMessage Info (read ts :: Int) (unwords contents)
        ("W" : ts : contents) ->
          LogMessage Warning (read ts :: Int) (unwords contents)
        _ -> Unknown s

parse :: String -> [LogMessage]
parse logFile =
  map parseMessage $ lines logFile

insert :: LogMessage -> MessageTree -> MessageTree
insert message tree =
  case message of
    Unknown _ -> tree
    LogMessage _ ts _ ->
      let node = Node Leaf message Leaf in
        case tree of
            Leaf -> node
            Node l m@(LogMessage _ nts _) r ->
                if ts > nts
                then Node l m (insert message r)
                else Node (insert message l) m r
            _ -> tree

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder tree =
  go tree []
  where
    go Leaf ret = ret
    go (Node l m r) _ =
          inOrder l ++ (m : inOrder r)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ms =
  map errorMessage $ inOrder (build (filter severeError ms))
  where
    severeError :: LogMessage -> Bool
    severeError (LogMessage (Error sev) _ _) = sev >= 50
    severeError _ = False
    errorMessage :: LogMessage -> String
    errorMessage (LogMessage _ _ s) = s
    errorMessage _ = ""
