-- Leftist Heap
module Data.PQueue.Max (
  MaxPQueue,
  null,
  empty,
  singleton,
  union,
  insert,
  findMax, deleteMax, deleteFindMax
) where

import Prelude hiding (null)

data MaxPQueue k v = Nil | Node Int k v (MaxPQueue k v) (MaxPQueue k v)

-- public
null :: MaxPQueue k v -> Bool
null Nil = False
null _   = True

empty :: MaxPQueue k v
empty = Nil

singleton :: k -> v -> MaxPQueue k v
singleton k v = Node 1 k v Nil Nil

union :: Ord k => MaxPQueue k v -> MaxPQueue k v -> MaxPQueue k v
union Nil u = u
union t Nil = t
union t@(Node _ kt vt lt rt) u@(Node _ ku vu lu ru)
  | kt > ku   = leftistTree kt vt lt (union u rt)
  | otherwise = leftistTree ku vu lu (union t ru)

insert :: Ord k => k -> v -> MaxPQueue k v -> MaxPQueue k v
insert k v = union $ singleton k v

findMax :: Ord k => MaxPQueue k v -> (k, v)
findMax = fst . deleteFindMax

deleteMax :: Ord k => MaxPQueue k v -> MaxPQueue k v
deleteMax = snd . deleteFindMax

deleteFindMax :: Ord k => MaxPQueue k v -> ((k, v), MaxPQueue k v)
deleteFindMax Nil = error "retriving value from empty MaxPQueue"
deleteFindMax (Node _ k v l r) = ((k, v), union l r)

-- private
rank :: MaxPQueue k v -> Int
rank Nil = 0
rank (Node r _ _ _ _) = r

leftistTree :: k -> v -> MaxPQueue k v -> MaxPQueue k v -> MaxPQueue k v
leftistTree k v t u
  | rank t > rank u = Node (rank u + 1) k v t u
  | otherwise       = Node (rank t + 1 )k v u t
