---------------------------------------------------------------------------------
--
--  https://github.storm.gatech.edu/NetASM
--
--  File:
--        List.hs
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

module Utils.List where

import Data.List as Li
import Data.Sequence as Seq
import Data.Foldable as Fold

main = print . toList . fromList $ [1..10]

splitAt :: (Int, [a]) -> ([a], [a])
splitAt (i, xs) = Li.splitAt i xs

removeAt :: (Eq a) => (Int, [a]) -> [a]
removeAt (i, xs) = xs'
  where xs' = Li.filter (\e -> e/=(xs!!i)) xs

updateAt :: (Int, [a], a) -> [a]
updateAt (i, xs, x) = Fold.toList $ Seq.update i x $ Seq.fromList xs