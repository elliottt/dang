{-# LANGUAGE Safe #-}

module Dang.Utils where

(!!?) :: [a] -> Int -> Maybe a
as !!? n | n < 0     = Nothing
         | otherwise = loop as n
  where
  loop []     _  = Nothing
  loop (t:_)  0  = Just t
  loop (_:ts) n' = loop ts (n'-1)

uncons :: [a] -> Maybe (a,[a])
uncons (a:as) = Just (a,as)
uncons _      = Nothing

splitLast :: [a] -> Maybe ([a],a)
splitLast  = loop
  where
  loop [a] = return ([],a)
  loop []  = Nothing
  loop as  = do
    (x,xs)  <- uncons as
    (ini,l) <- loop xs
    return (x:ini,l)
