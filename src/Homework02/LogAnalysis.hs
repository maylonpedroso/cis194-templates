{-# OPTIONS_GHC -Wall #-}

----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 02
--
----------------------------------------------------------------------

module Homework02.LogAnalysis where

import Homework02.Log

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- |
--
-- >>> parseMessage "E 2 562 help help"
-- LogMessage (Error 2) 562 "help help"
-- >>> parseMessage "I 29 la la la"
-- LogMessage Info 29 "la la la"
-- >>> parseMessage "This is not in the right format"
-- Unknown "This is not in the right format"

parseMessage :: String -> LogMessage
parseMessage message
    | logType == "I" = LogMessage Info (logInt 1) (logText 2)
    | logType == "W" = LogMessage Warning (logInt 1) (logText 2)
    | logType == "E" = LogMessage (Error (logInt 1)) (logInt 2) (logText 3)
    | otherwise = Unknown message
    where messageWords = words message
          logType = head messageWords
          logInt n = read (messageWords !! n) :: Int
          logText n = unwords (drop n messageWords)

parse :: String -> [LogMessage]
parse content = map parseMessage (lines content)

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

-- |
--
-- >>>
--

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert message Leaf = Node Leaf message Leaf
insert message1@(LogMessage _ time1 _) (Node nl message2@(LogMessage _ time2 _) nr)
    | time1 < time2 = Node (insert message1 nl) message2 nr
    | otherwise = Node nl message2 (insert message1 nr)
insert _ _ = undefined

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>>
--

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (logMessage:logs) = insert logMessage (build logs) 

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

-- |
--
-- >>>
--

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lTree message rTree) = inOrder lTree ++ [message] ++ inOrder rTree

----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

-- |
--
-- >>>
--

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = map getLogMessageText (inOrder (build (filter isErrorFifty messages)))

getLogMessageText :: LogMessage -> String
getLogMessageText (LogMessage _ _ text) = text
getLogMessageText (Unknown text) = text 

isErrorFifty :: LogMessage -> Bool
isErrorFifty logMessage = isErrorLevel logMessage 50

isErrorLevel :: LogMessage -> Int -> Bool
isErrorLevel (LogMessage (Error code) _ _) level
    | code >= level = True
isErrorLevel _ _ = False

----------------------------------------------------------------------
-- Exercise 6 (Optional)
----------------------------------------------------------------------

whoDidIt :: String
whoDidIt = undefined
