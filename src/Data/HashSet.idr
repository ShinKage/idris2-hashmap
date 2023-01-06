module Data.HashSet

import Data.HashMap
import Data.Maybe

export
record HashSet k where
    constructor MkHashSet
    {auto hashableImpl : Hashable k}
    {auto eqImpl : Eq k}
    hashmap: HashMap k ()

export
empty : Hashable k => Eq k => HashSet k
empty = MkHashSet empty

export
insert : k -> HashSet k -> HashSet k
insert k (MkHashSet hashmap) = MkHashSet (insert k () hashmap)

export
contains : k -> HashSet k -> Bool
contains k (MkHashSet hm) = isJust $ lookup k hm

export
union : HashSet k -> HashSet k -> HashSet k
union (MkHashSet {hashableImpl, eqImpl} hm0) (MkHashSet hm1) = MkHashSet {hashableImpl, eqImpl} (unionWith const hm0 hm1)
