---------------------------------------------------------------------------------
--
--  https://github.storm.gatech.edu/NetASM
--
--  File:
--        SimpleMACLearner/Run.hs
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

module Apps.StatefulMACLearner.Run where

import Utils.Map
import Core.Language
import Core.PacketParser
import Apps.StatefulMACLearner.Code

---------------------------
-- Stateful MAC Learner ---
---------------------------

-- Test header (a.k.a. packet) stream
h0 = genHdr([("inport",     1)
            ,("outport",    0)
            ,("srcmac",     1234)
            ,("dstmac",     4321)
            ,("i",          0)])

h1 = genHdr([("inport",     3)
            ,("outport",    0)
            ,("srcmac",     6543)
            ,("dstmac",     5432)
            ,("i",          0)])

h2 = genHdr([("inport",     4)
            ,("outport",    0)
            ,("srcmac",     4321)
            ,("dstmac",     1234)
            ,("i",          0)])

h3 = genHdr([("inport",     2)
            ,("outport",    0)
            ,("srcmac",     5432)
            ,("dstmac",     6543)
            ,("i",          0)])

-- Input sequence
is = [HDR(h0),
      HDR(h1), 
      HDR(h2), 
      HDR(h3)]

-- Emulate the code
emulateEx :: [Hdr]
emulateEx = emulate(ic, is, tc)

-- Profile the code
profileEx :: String 
profileEx = profile(ic, is, tc)

-- main
main = do 
        putStrLn $ prettyPrint $ emulateEx
        putStrLn profileEx