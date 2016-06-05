module LinkedList where
  data LinkedList a = LinkedList [(a, LinkedList a)] deriving (Show)

  nil :: LinkedList a
  nil = LinkedList []

  isNil :: LinkedList a -> Bool
  isNil (LinkedList as) = null as

  new :: a -> LinkedList a -> LinkedList a
  new a as = LinkedList [(a, as)]

  next :: LinkedList a -> LinkedList a
  next (LinkedList as) = snd $ head as

  datum :: LinkedList a -> a
  datum (LinkedList as) = fst $ head as

  toList :: LinkedList a -> [a]
  toList (LinkedList [])  = []
  toList a                = datum a : toList (next a)

  fromList :: [a] -> LinkedList a
  fromList []       = nil
  fromList (a:as)   = new a (fromList as)

  reverseLinkedList :: LinkedList a -> LinkedList a
  reverseLinkedList = fromList . reverse . toList
