module Data.String.HIUtils (
  -- * Padding
  padl, padr, pad
  ) where

-- |Pad right side with spaces such that @length (pad n s) >= length s + n@
pad :: Int -> String -> String
pad = padr

-- |Pad right side with spaces such that @length (pad n s) >= length s + n@
padr :: Int -> String -> String
padr n s = s ++ replicate (n - length s) ' '

-- |Pad left side with spaces such that @length (pad n s) >= length s + n@
padl :: Int -> String -> String
padl n s = replicate (n - length s) ' ' ++ s
