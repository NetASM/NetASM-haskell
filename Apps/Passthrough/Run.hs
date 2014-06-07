---------------------------------------------------------------------------------
--
--  https://github.storm.gatech.edu/NetASM
--
--  File:
--        Passthrough/Run.hs
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

module Apps.Passthrough.Run where

import Utils.Map
import Core.Language
import Core.PacketParser
import Apps.Passthrough.Code

------------------
-- Passthrough ---
------------------

-- Test header (a.k.a. packet) stream
h0 = genHdr([("inport",     1)
            ,("outport",    0)
            ,("srcmac",     1234)
            ,("dstmac",     4321)
            ,("i",          0)])

h1 = genHdr([("inport",     2)
            ,("outport",    0)
            ,("srcmac",     6543)
            ,("dstmac",     5432)
            ,("i",          0)])

-- Test control stream
c0 = WRT(mt_t, [("inport", 1)], 0)    -- write inport=1 in mt_t table at index 0
c1 = WRT(md_t, [("outport", 2)], 0)   -- write outport=2 in md_t table at index 0
c2 = WRT(mt_t, [("inport", 2)], 1)    -- write inport=2 in mt_t table at index 1
c3 = WRT(md_t, [("outport", 1)], 1)   -- write outport=1 in md_t table at index 1

-- Input sequence
is = [CTRL(c0), 
      CTRL(c1), 
      CTRL(c2), 
      CTRL(c3), 
      HDR(h0), 
      HDR(h1)]

-- Emulate the code
emulateEx :: [Hdr]
emulateEx = emulate(ic, is, tc)

-- Profile the code
profileEx :: String 
profileEx = profile(ic, is, tc)

-- Debug
debugEx :: String
debugEx = outputI
        ++outputC0
        ++outputC1
        ++outputC2
        ++outputC3
        ++output0
        ++output1
  where
    (_, rs, ts) = debugInit(ic)
    outputI = print'(([], rs, ts), 0)
    ((_, rsC0, tsC0), _) = debugInput(CTRL(c0), tc, ([], rs, ts))
    outputC0 = print'(([], rsC0, tsC0), 0)
    ((_, rsC1, tsC1), _) = debugInput(CTRL(c1), tc, ([], rsC0, tsC0))
    outputC1 = print'(([], rsC1, tsC1), 0)
    ((_, rsC2, tsC2), _) = debugInput(CTRL(c2), tc, ([], rsC1, tsC1))
    outputC2 = print'(([], rsC2, tsC2), 0)
    ((_, rsC3, tsC3), _) = debugInput(CTRL(c3), tc, ([], rsC2, tsC2))
    outputC3 = print'(([], rsC3, tsC3), 0)
    ((h0', rs0, ts0), n0) = debugInput(HDR(h0), tc, ([], rsC3, tsC3))
    output0 = print'((h0', rs0, ts0), n0)
    ((h1', rs1, ts1), n1) = debugInput(HDR(h1), tc, ([], rs0, ts0))
    output1 = print'((h1', rs1, ts1), n1)

-- Main
main = do 
        --print emulateEx
        --print profileEx
        putStrLn debugEx