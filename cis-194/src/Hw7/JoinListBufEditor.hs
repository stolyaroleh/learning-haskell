module Hw7.JoinListBufEditor where

import Hw7.Buffer
import Hw7.JoinList
import Hw7.JoinListBuffer
import Hw7.Editor

initialBuffer = fromString s :: JoinListBuffer
    where s = unlines [ "This buffer is for notes you don't want to save, and for"
                      , "evaluation of steam valve coefficients."
                      , "To load a different file, type the character L followed"
                      , "by the name of the file."]

main = runEditor editor initialBuffer
