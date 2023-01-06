module Data.HashMap.Dependant

import Data.HashMap.Internal
import Decidable.Equality

export
data HashDMap : (key : Type) -> (val : key -> Type) -> Type where
    Empty : DecEq key => Hashable key => HashDMap key val
    Trie : DecEq key => Hashable key => HAMT key val -> HashDMap key val

%name HashDMap hm

export
empty : DecEq key => Hashable key => HashDMap key val
empty = Empty

export
lookup : (k : key) -> HashDMap key val -> Maybe (val k)
lookup key Empty = Nothing
lookup key (Trie hamt) = lookup key hamt

export
insert : (k : key) -> val k -> HashDMap key val -> HashDMap key val
insert key val Empty = Trie $ singleton key val
insert key val (Trie hamt) = Trie $ insert key val hamt

export
delete : key -> HashDMap key val -> HashDMap key val
delete key Empty = Empty
delete key (Trie hamt) = case delete key hamt of
    Just hamt' => Trie hamt'
    Nothing => Empty

export
foldWithKey : (f : (x : k) -> v x -> acc -> acc) -> acc -> HashDMap k v -> acc
foldWithKey f z Empty = z
foldWithKey f z (Trie hamt) = foldWithKey f z hamt

export
toList : HashDMap k v -> List (x : k ** v x)
toList hm = foldWithKey (\key, val, acc => (key ** val) :: acc) [] hm

export
keys : HashDMap k v -> List k
keys hm = foldWithKey (\key, val, acc => key :: acc) [] hm

export
fromList : Hashable k => DecEq k => List (x : k ** v x) -> HashDMap k v
fromList lst = foldr (\(k ** v) => insert k v) empty lst

export
map : (forall x . v x -> v' x) -> HashDMap k v -> HashDMap k v'
map f Empty = Empty
map f (Trie hamt) = Trie $ trieMap f hamt

export
Show key => (forall x . Show (val x)) => Show (HashDMap key val) where
    show hm = "fromList \{show $ toList hm}"
