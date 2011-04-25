module Utils where

(!!?) :: [a] -> Int -> Maybe a
as !!? n | n < 0     = Nothing
         | otherwise = loop as n
  where
  loop []     _  = Nothing
  loop (t:_)  0  = Just t
  loop (_:ts) n' = loop ts (n'-1)
