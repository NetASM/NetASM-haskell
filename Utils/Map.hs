---------------------------------------------------------------------------------
--
--  https://github.storm.gatech.edu/NetASM
--
--  File:
--        Map.hs
--
--  Project:
--        NetASM: A Network Assembly for Orchestrating Programmable Network Devices
--
--  Author:
--        Muhammad Shahbaz
--
--  Copyright notice:
--        Copyright (C) 2014 Georgia Institute of Technology
--           Network Operations and Internet Security Lab
--
--  Licence:
--        This file is a part of the NetASM development base package.
--
--        This file is free code: you can redistribute it and/or modify it under
--        the terms of the GNU Lesser General Public License version 2.1 as
--        published by the Free Software Foundation.
--
--        This package is distributed in the hope that it will be useful, but
--        WITHOUT ANY WARRANTY; without even the implied warranty of
--        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--        Lesser General Public License for more details.
--
--        You should have received a copy of the GNU Lesser General Public
--        License along with the NetASM source package.  If not, see
--        http://www.gnu.org/licenses/.

module Utils.Map where

-- A map is a list of key/value pairs, for any types k and v.
type Map k v = [(k, v)]

-- Return the key component of a pair.
key :: (k, v) -> k
key (k, v) = k

-- Return the value component of a pair.
value :: (k, v) -> v
value (k, v) = v

-- Does the given map contain the given key?
includes :: Eq k => (Map k v, k) -> Bool
includes([]  , k) = False
includes(p:ps, k) = key(p) == k  ||  includes(ps, k)

-- Return the value associated with a given key in a given map.
fetch :: (Eq k, Show k) => (Map k v, k) -> v
fetch([]  , k) = error("Map.fetch: key " ++ show(k) ++ " not in map")
fetch(p:ps, k) = if key(p) == k then value(p) else fetch(ps, k)

-- Infix operator for fetching.
m ! k = fetch(m, k)

-- Associate given key with given value in a given map.
insert :: Eq k => (Map k v, k, v) -> Map k v
insert([]  , k, v) = [(k, v)]
insert(p:ps, k, v) = if key(p) == k then (k, v):ps else p:insert(ps, k, v)