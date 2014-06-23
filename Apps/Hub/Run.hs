---------------------------------------------------------------------------------
--
--  https://github.storm.gatech.edu/NetASM
--
--  File:
--        Hub/Run.hs
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

module Apps.Hub.Run where

import Utils.Map
import Core.Language
import Core.PacketParser
import Apps.Hub.Code

----------
-- Hub ---
----------

-- Test header (a.k.a packet) stream
h0 = genHdr([("inport", 1), ("outport", 0)])
h1 = genHdr([("inport", 2), ("outport", 0)])

-- Input sequence
is = [HDR(h0), 
      HDR(h1)]

-- Emulate the code
emulateEx :: [Hdr]
emulateEx = emulate([], is, c)

-- Profile the code
profileEx :: String 
profileEx = profile([], is, c)

-- Main
main = do 
        print emulateEx
        print profileEx
