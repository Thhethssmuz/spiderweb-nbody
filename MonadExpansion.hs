module MonadExpansion
  ( repetitiousCombine
  , repetitiousCombineM
  , repetitiousCombineM_
  , uniqueCombine
  , uniqueCombineM
  , uniqueCombineM_
  ) where

repetitiousCombine :: (a -> a -> b) -> [a] -> [b]
repetitiousCombine _ [] = []
repetitiousCombine f (x:xs) = map (f x) (x:xs) ++ repetitiousCombine f xs

repetitiousCombineM :: Monad m => (a -> a -> m b) -> [a] -> m [b]
repetitiousCombineM f xs = sequence (repetitiousCombine f xs)

repetitiousCombineM_ :: Monad m => (a -> a -> m b) -> [a] -> m ()
repetitiousCombineM_ f xs = sequence_ (repetitiousCombine f xs)

uniqueCombine :: (a -> a -> b) -> [a] -> [b]
uniqueCombine _ [] = []
uniqueCombine f (x:[]) = [] 
uniqueCombine f (x:xs) = map (f x) xs ++ uniqueCombine f xs

uniqueCombineM :: Monad m => (a -> a -> m b) -> [a] -> m [b]
uniqueCombineM f xs = sequence (uniqueCombine f xs)

uniqueCombineM_ :: Monad m => (a -> a -> m b) -> [a] -> m ()
uniqueCombineM_ f xs = sequence_ (uniqueCombine f xs)

main = do
  print "Done"




