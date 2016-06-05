module LinkedList where
  data LinkedList a = Empty | LinkedList (a, LinkedList a) deriving (Show)

  nil :: LinkedList a
  nil = Empty

  isNil :: LinkedList a -> Bool
  isNil Empty = True
  isNil _     = False

  new :: a -> LinkedList a -> LinkedList a
  new a as = LinkedList (a, as)

  next :: LinkedList a -> LinkedList a
  next (LinkedList as) = snd as

  datum :: LinkedList a -> a
  datum (LinkedList as) = fst as

  toList :: LinkedList a -> [a]
  toList Empty  = []
  toList a      = datum a : toList (next a)

  fromList :: [a] -> LinkedList a
  fromList []       = nil
  fromList (a:as)   = new a (fromList as)

  reverseLinkedList :: LinkedList a -> LinkedList a
  reverseLinkedList = fromList . reverse . toList
