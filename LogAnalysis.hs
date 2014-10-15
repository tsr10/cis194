{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Data.List
import Data.Char

--1
parseMessage :: String -> MaybeLogMessage
parseMessage string = parseMessageHelper (words string)

parseMessageHelper :: [String] -> MaybeLogMessage
parseMessageHelper (w:x:y:z)
    | (w == "E") && (errorlevel /= -1) && (timestamp /= -1) = ValidLM (LogMessage (Error errorlevel) timestamp (unwords z))
    where
    	errorlevel = maybeIntVal(readInt x)
    	timestamp = maybeIntVal(readInt y)
parseMessageHelper (w:x:y)
    | (w == "W") && (timestamp /= -1) = ValidLM (LogMessage Warning timestamp (unwords y))
    | (w == "I") && (timestamp /= -1) = ValidLM (LogMessage Info timestamp (unwords y))
    where
    	timestamp = maybeIntVal(readInt x)
parseMessageHelper w = InvalidLM (unwords w)

maybeIntVal :: MaybeInt -> Int
maybeIntVal (ValidInt val) = val
maybeIntVal InvalidInt = -1

--2
validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly [] = []
validMessagesOnly ((ValidLM x):y) = (x:(validMessagesOnly y))
validMessagesOnly (_:y) = validMessagesOnly y


--3
parse :: String -> [LogMessage]
parse string = validMessagesOnly(parseHelper (lines string))

parseHelper :: [String] -> [MaybeLogMessage]
parseHelper [] = []
parseHelper (i:inputstring) = ((parseMessage i) : (parseHelper inputstring))

--4
compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ time1 _) (LogMessage _ time2 _)
	| (time1 > time2) = GT
	| (time1 == time2) = EQ
	| (time1 < time2) = LT

--5
sortMessages :: [LogMessage] -> [LogMessage]
sortMessages x = sortBy compareMsgs x

--6
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong x = whatWentWrongHelper (sortBy compareMsgs x)

whatWentWrongHelper :: [LogMessage] -> [String]
whatWentWrongHelper [] = []
whatWentWrongHelper ((LogMessage (Error e) _ z):y) 
    | (e >= 50) = (z:(whatWentWrongHelper y))
    | otherwise = whatWentWrongHelper y
whatWentWrongHelper (_:y) = whatWentWrongHelper y

--7
messagesAbout :: String -> [LogMessage] -> [LogMessage]
messagesAbout _ [] = []
messagesAbout string (lm@(LogMessage _ _ y):z)
    | ((map toLower string) `elem` (words (map toLower y))) = (lm:(messagesAbout string z))
    | otherwise = messagesAbout string z

--8
whatWentWrongEnhanced :: String -> [LogMessage] -> [String]
whatWentWrongEnhanced _ [] = []
whatWentWrongEnhanced string ((LogMessage (Error e) _ z):y) 
    | (e >= 50) = (z:(whatWentWrongEnhanced string y))
whatWentWrongEnhanced string ((LogMessage _ _ y):z)
    | ((map toLower string) `elem` (words (map toLower y))) = (y:(whatWentWrongEnhanced string z))
    | otherwise = whatWentWrongEnhanced string z
whatWentWrongEnhanced string (_:y) = whatWentWrongEnhanced string y
