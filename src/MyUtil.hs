module MyUtil where

import Data.Text
import Data.Char

stripAll = Data.Text.concat . (Data.Text.split Data.Char.isSpace) . Data.Text.strip

fstOccurrence f ls =
  inner 0 ls
  where
    inner acc [] = Nothing
    inner acc (c:cs) =
      if f c then Just acc else inner (acc + 1) cs
