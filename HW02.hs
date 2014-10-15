{-
Name: <your name here>
Collaborators: <your collaborators here, or "none">
Notes: <any particular notes about your work -- what you struggled with,
        what's not working, what's really cool, etc.>
-}

module HW02 where

import Words
import Data.List

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:

--HOMEWORK 2

--1
type Hand = [Char]
formableBy :: String -> Hand -> Bool
formableBy [] h = True
formableBy s [] = False
formableBy s h
	| (head s) `elem` h = formableBy (tail s) (delete (head s) h)
	| otherwise = False

--2
wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

--3
wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate [] hand [] = True
wordFitsTemplate [] hand template = False
wordFitsTemplate template hand [] = False
wordFitsTemplate template hand string
    | ((head template) == '?') && formableBy [(head string)] hand = wordFitsTemplate (tail template) hand (tail string)
    | ((head template) == (head string)) = wordFitsTemplate (tail template) hand (tail string)
    | otherwise = False

--4
wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate template hand = wordsFittingTemplateHelper template hand allWords

wordsFittingTemplateHelper :: Template -> Hand -> [String] -> [String]
wordsFittingTemplateHelper template hand [] = []
wordsFittingTemplateHelper template hand (w:wordlist)
	| ((wordFitsTemplate template hand w) == True) = w:wordsFittingTemplateHelper template hand wordlist
	| ((wordFitsTemplate template hand w) == False) = wordsFittingTemplateHelper template hand wordlist

--5
scrabbleValueWord :: String -> Int
scrabbleValueWord [] = 0
scrabbleValueWord string = scrabbleValue(head string) + scrabbleValueWord (tail string)

--6
bestWords :: [String] -> [String]
bestWords [] = []
bestWords wordlist = bestWordsHelper wordlist [] 0

bestWordsHelper :: [String] -> [String] -> Int -> [String]
bestWordsHelper [] oldstring int = oldstring
bestWordsHelper (w:wordlist) oldstring int
    | ((scrabbleValueWord w) > int) = bestWordsHelper wordlist [w] (scrabbleValueWord w)
    | ((scrabbleValueWord w) == int) = bestWordsHelper wordlist (w:oldstring) int
    | ((scrabbleValueWord w) < int) = bestWordsHelper wordlist oldstring int

--7
scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate [] [] = 0
scrabbleValueTemplate stemplate word = (scrabbleValueMultiplier stemplate) * (scrabbleValueTemplateHelper stemplate word 0)

scrabbleValueMultiplier :: STemplate -> Int
scrabbleValueMultiplier [] = 1
scrabbleValueMultiplier (s:stemplate)
    | (s == '2') = 2 * (scrabbleValueMultiplier stemplate)
    | (s == '3') = 3 * (scrabbleValueMultiplier stemplate)
    | otherwise = 1 * (scrabbleValueMultiplier stemplate)

scrabbleValueTemplateHelper :: STemplate -> String -> Int -> Int
scrabbleValueTemplateHelper [] [] score = score
scrabbleValueTemplateHelper (s:stemplate) (w:word) score
    | (s == 'D') = scrabbleValueTemplateHelper stemplate word (score + (scrabbleValue w) * 2)
    | (s == 'T') = scrabbleValueTemplateHelper stemplate word (score + (scrabbleValue w) * 3)
    | otherwise = scrabbleValueTemplateHelper stemplate word (score + (scrabbleValue w))
