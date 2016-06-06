module LinkedList where
  data LinkedList a = Empty | Element a (LinkedList a)

  nil :: LinkedList a
  nil = Empty

  isNil :: LinkedList a -> Bool
  isNil Empty = True
  isNil _     = False

  new :: a -> LinkedList a -> LinkedList a
  new a as = Element a as

  next :: LinkedList a -> LinkedList a
  next (Element _ xs) = xs

  datum :: LinkedList a -> a
  datum (Element x _) = x

  toList :: LinkedList a -> [a]
  toList Empty  = []
  toList a      = datum a : toList (next a)

  fromList :: [a] -> LinkedList a
  fromList []       = nil
  fromList (a:as)   = new a (fromList as)

  reverseLinkedList :: LinkedList a -> LinkedList a
  reverseLinkedList = fromList . reverse . toList
