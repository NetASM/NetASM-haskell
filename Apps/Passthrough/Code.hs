---------------------------------------------------------------------------------
--
--  https://github.storm.gatech.edu/NetASM
--
--  File:
--        Passthrough/Code.hs
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

module Apps.Passthrough.Code where

import Utils.Map
import Core.Language
import Core.PacketParser

------------------
-- Passthrough ---
------------------

-- Table size
t_s = 2

-- Match table specs with default values
mt_s = t_s                                        -- table size
mt_t = Dynamic("mt0", (mt_s, ["inport"]))         -- dynamic table (mt0)
mt_v = Static([[("inport", 0)] | x <- [1..t_s]])  -- default values

-- Match table specs with default values
md_s = t_s
md_t = Dynamic("md0", (md_s, ["outport"]))        -- dynamic table (md_t)
md_v = Static([[("outport", 0)] | x <- [1..t_s]]) -- default values

-- Initialisation code for mac learning
ic = [MKT(mt_t, mt_v)  -- create match table with given table type and default values
    , MKT(md_t, md_v)] -- create modify table with given table type and default values

-- Topology code for mac learning
tc = [IBRTF(mt_t, "i", "l_miss") -- if (match on table mt_t) then set index field "i" in the header to the matched index and move to next instruction else jump to label "l_miss"
    , LDFTF(md_t, "i")           -- load header with md_t table content at index "i"
    , JMP("l_end")               -- jump to label "l_end"
    , LBL("l_miss")              -- label "l_miss"
    , DRP                        -- drop i.e., set drop field in the header to 1
    , LBL("l_end")               -- label "l_end"
    , HLT]                       -- halt

