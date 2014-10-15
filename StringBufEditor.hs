module Main where

import StringBuffer
import Editor
import JoinList
import Sized

main = runEditor editor $ (Single (scoreString sample, Size 1) sample)
         --[ "This buffer is for notes you don't want to save, and for"
         --, "evaluation of steam valve coefficients."
         --, "To load a different file, type the character L followed"
         --, "by the name of the file."
         --]

sample = "sample"