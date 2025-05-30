module Justify (justify) where
import Data.List (intercalate)

mw1 = 16
ws1 = ["This", "is", "an", "example", "of", "text", "justification."]

mw2 = 16
ws2 = ["What","must","be","acknowledgment","shall","be"]

mw3 = 20
ws3 = ["Science","is","what","we"
      ,"understand","well"
      ,"enough","to","explain","to"
      ,"a","computer.","Art","is"
      ,"everything","else","we","do"]

space :: [String] -> Int -> String
space [] _ = []
space [w] maxWidth = w ++ replicate (maxWidth - length w) ' '
space words@(w:ws) maxWidth = intercalate leftSpace beforeWord ++ rightWords where
  ceilDiv :: (Integral b, Integral a1, Integral a2) => a1 -> a2 -> b
  ceilDiv x y = ceiling (fromIntegral x / fromIntegral y)
  spaces = maxWidth - sum (map length words)
  words_to_pad = length ws
  rightSpaceLength = quot spaces words_to_pad
  leftSpaceLength = ceilDiv spaces words_to_pad
  rightSpace = replicate rightSpaceLength ' '
  leftSpace = replicate leftSpaceLength ' '
  count = spaces - (words_to_pad * rightSpaceLength)
  (beforeWord, leftWord) = splitAt (count+1) words
  rightWords = if null leftWord
                then ""
                else rightSpace ++ intercalate rightSpace leftWord

cutWords :: [String] -> Int -> ([String], [String])
cutWords words maxWidth = _cutWords words maxWidth 0 [] where
  _cutWords [] _ _ acc = (reverse acc, [])
  _cutWords words@(w:ws) maxWidth currWidth acc =
    if (currWidth + wordLength) > maxWidth
       then (reverse acc, words)
       else _cutWords ws maxWidth (currWidth + wordLength + 1) (w:acc) where
            wordLength = length w

cutAll :: [String] -> Int -> [[String]]
cutAll words maxWidth = line:lines where
  lines = if null next
            then []
            else cutAll next maxWidth
  (line, next) = cutWords words maxWidth

justify :: Int -> [String] -> [String]
justify maxWidth words = partial ++ [lastFill] where
  cutLines = cutAll words maxWidth
  partial = map (`space` maxWidth) (init cutLines)
  lastLine = unwords (last cutLines)
  lastFill = lastLine ++ replicate (maxWidth - length lastLine) ' '

printJustified :: Int -> [String] -> IO ()
printJustified w = do mapM_ putStrLn . justify w
