---------------------------------------------------------------------------------
--
--  https://github.storm.gatech.edu/NetASM
--
--  File:
--        PacketParser.hs
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

module Core.PacketParser where

import Core.Language
import Utils.Map

-- Returns a header with special fields.
spclFlds :: Hdr
spclFlds = [("PID", (0, True))
           ,("DRP", (0, True))]

-- Returns a header with user-defined pattern.
genHdr :: Ptrn -> Hdr
genHdr xs = spclFlds++genHdr'(xs)

genHdr' :: Ptrn -> Hdr
genHdr' [] = []
genHdr' (x:xs) 
  | isSpclFld = error "Error: can't overwrite special fields."
  | otherwise = (f, (v, True)):genHdr'(xs)
  where 
    (f, v) = x
    isSpclFld = (f, (0, True)) `elem` spclFlds
   
-- Sets or unsets the header field
setVld :: (Hdr, Fld, Bool) -> Hdr
setVld (h, f, b) = insert(h, f, (fst(h!f), b))
