---------------------------------------------------------------------------------
--
--  https://github.storm.gatech.edu/NetASM
--
--  File:
--        StatefulMACLearner/Code.hs
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

module Apps.StatefulMACLearner.Code where

import Utils.Map
import Core.Language
import Core.PacketParser

---------------------------
-- Stateful MAC Learner ---
---------------------------

-- Table size
t_s = 10

-- Match table specs with default values
mt_s = t_s                                          -- table size
mt_t = Dynamic("mt0", (mt_s, ["dstmac"]))           -- dynamic table "mt0"
mt_v = Static([[("dstmac", 0)] | x <- [1..mt_s]])   -- default values
mt_p = [("dstmac", "srcmac")]                       -- pattern for loading the match table

-- Modify table specs with default values
md_s = t_s                                          -- table size
md_t = Dynamic("md0", (md_s, ["outport"]))          -- dynamic table "md0"
md_v = Static([[("outport", 0)] | x <- [1..md_s]])  -- default values      
md_p = [("outport", "inport")]                      -- pattern for loading the modify table

-- Initialisation code for MAC learning
ic = [MKT(mt_t, mt_v) -- create match table with given table type and default values
    , MKT(md_t, md_v) -- create modify table with given table type and default values
    , MKR("r", 0)]    -- create register 'r' to index currently selected row

-- Topology code for MAC learning
tc = [IBRTF(mt_t, "i", "l_miss")         -- if (match on mt_t table) then set index field "i" in the header with matched index and goto next instruction else jump label "l_miss"
    , LDFTF(md_t, "i")                   -- load header with md_t table content at index "i"
    , JMP("l_end")                       -- jump to label "l_end"
    , LBL("l_miss")                      -- label "l_miss"
    , LDTFR(mt_t, mt_p, "r")             -- update "dstmac" in the mt_t table at location "r" with "srcmac" from the header
    , LDTFR(md_t, md_p, "r")             -- update "outport" in the md_t table at location "r" with "inport" from the header
    , OPF("outport", "inport", Xor, _1s) -- set "outport" (bitmap) to all 1s except for the "inport" i.e., flood the packet
    , OPR("r", "r", Add, 1)              -- increment register "r" i.e., move the current index to next row
    , BRR("r", Lt, t_s, "l_end")         -- if (register "r" less than t_s) then jump to label "l_end" else goto next instruction
    , LDR("r", 0)                        -- set register "r" = 0 
    , LBL("l_end")                       -- label "l_end"  
    , HLT]                               -- halt

