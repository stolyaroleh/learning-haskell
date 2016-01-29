module Hw2.Hw2
    ( testParse,
      testWhatWentWrong,
      parse,
      insert,
      build,
      inOrder,
      whatWentWrong,
      printLog
    ) where

import Hw2.Log
import Text.Read

popMessageType :: [String] -> (Maybe MessageType, [String])
popMessageType words@(mt:rest)
    | mt == "E" = if rest /= [] then
                  case readMaybe (head rest) :: Maybe Int of
                       Just x  -> (Just (Error x), tail rest)
                       Nothing -> (Nothing, rest)
                  else
                  (Nothing, rest)
    | mt == "I" = (Just Info, rest)
    | mt == "W" = (Just Warning, rest)
    | otherwise = (Nothing, words)
popMessageType [] = (Nothing, [])

popTimeStamp :: [String] -> (Maybe TimeStamp, [String])
popTimeStamp (ts:rest) = (readMaybe ts, rest)
popTimeStamp words = (Nothing, words)

parseLine :: String -> LogMessage
parseLine line = case (messageType, timeStamp) of
                 (Nothing, _)          -> Unknown line
                 (_, Nothing)          -> Unknown line
                 (Just mt, Just ts)    -> LogMessage mt ts (unwords messageWords)
                 where (messageType, timeStampAndMessage) = popMessageType (words line)
                       (timeStamp, messageWords) = popTimeStamp timeStampAndMessage

parse :: String -> [LogMessage]
parse text = [parseLine line | line <- (lines text)]

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ ts _) (Node left nodeMsg@(LogMessage _ nodeTs _) right)
    | ts < nodeTs  = Node (insert msg left) nodeMsg right
    | ts > nodeTs  = Node left nodeMsg (insert msg right)
    | ts == nodeTs = Node left msg right

build :: [LogMessage] -> MessageTree
build messages = insertAll messages Leaf
    where insertAll []     tree = tree
          insertAll (x:xs) tree = insertAll xs (insert x tree)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong log = [message | LogMessage (Error severity) _ message <- sortedLog, severity >= 50]
    where sortedLog = inOrder (build log)

printLog :: [LogMessage] -> IO()
printLog = putStrLn . unlines . map show