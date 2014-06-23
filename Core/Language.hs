---------------------------------------------------------------------------------
--
--  https://github.storm.gatech.edu/NetASM
--
--  File:
--        Language.hs
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

-- Definition of the NetASM language

module Core.Language where

import Prelude hiding (drop, id)
import Data.Bits

import Utils.List as Li
import Utils.Map 

{-
	TODO: Create examples
          - Multiple match tables to single action table
          - P4 example
-}

--------------------
-- Syntax of NetASM.
--------------------

-- Input is either a header or control instruction.
data Input =
    HDR (Hdr)
  | CTRL (CtrlInstr)

-- InitCode is a list of initialise instructions.
type InitCode = [InitInstr]

-- TopoCode is a list of topology instructions.
type TopoCode = [TopoInstr]
   
-- Fields represented by strings.
type Fld = String

-- Values represented as int.
type Val = Int

-- Label represented as strings.
type Lbl = String

-- Maps fields to values (e.g., used in the definition of tables).
type Ptrn = Map Fld Val

-- The header maps fields to tuples (v,b), where v is the value of the field 
-- and b indicates if the field is valid or not. Fields a listed in the order of 
-- the received packet header.
type Hdr = Map Fld (Val, Bool)
-- It contains some special fields (PID: Instr id, DRP: Drop). These special 
-- fields are always written in capital letters. 

-- Register represented as strings.
type Reg = String

-- Table definition (can be static or dynamic).
data Tbl = Static  ([Ptrn])
         | Dynamic (String, (Int, [Fld])) -- (name, (# rows), fields)
  deriving Show
  
-- Arithmetic/Bitwise operators.
data Op = Add | Sub | And | Or | Xor
  deriving Show

-- Comparison operators.
data CmpOp = Eq | Neq | Lt | Gt | Le | Ge
  deriving Show
  
-- Initialise instructions.
data InitInstr =
    MKR     (Reg, Val)                -- Make a new register (r) with default value (v).
  | MKT     (Tbl, Tbl)                -- Make a new dynamic table (t) and load it with content from static table (t0).
  deriving Show

-- Topology Instructions.
data TopoInstr =
    HLT                               -- Halt: end of code.
  | LBL (Lbl)                         -- Label: label (l) for jump and branch instructions.
  | JMP (Lbl)                         -- Jump: jump control to label (l).
  -- Update instructions.
  -- -- register.
  | LDR     (Reg, Val)                -- Load register: load register (r) with value (v).
  | LDRR    (Reg, Reg)                -- Load register with register: load register (r) with register (r).
  | LDRF    (Reg, Fld)                -- Load register with field: load register (r) with field (f).
  | OPR     (Reg, Reg, Op, Val)       -- r0 = r1 op v: apply operation (op) on register (r1) and value (v) and store it in register (r0).
  | OPRR    (Reg, Reg, Op, Reg)       -- r0 = r1 op r2: apply operation (op) on register (r1) and register (r2) and store it in register (r0).
  | OPRF    (Reg, Reg, Op, Fld)       -- r0 = r1 op f: apply operation (op) on register (r1) and field (f) and store it in register (r0).
  -- -- table.
  | LDTP    (Tbl, Ptrn, Val)          -- Load table with pattern (f,v) at value: load table (t) with pattern (p) at index value (v).
  | LDTPR   (Tbl, Ptrn, Reg)          -- Load table with pattern (f,v) at register: load table (t) with pattern (p) at index register (r).
  | LDTPF   (Tbl, Ptrn, Fld)          -- Load table with pattern (f,v) at field: load table (t) with pattern (p) at index field (f).
  | LDTF    (Tbl, Map Fld Fld, Val)   -- Load table with pattern (f,f) at value: load table (t) with pattern (p) at index value (v).
  | LDTFR   (Tbl, Map Fld Fld, Reg)   -- Load table with pattern (f,f) at register: load table (t) with pattern (p) at index register (r).
  | LDTFF   (Tbl, Map Fld Fld, Fld)   -- Load table with pattern (f,f) at field: load table (t) with pattern (p) at index field (f).
  -- -- header fields.
  | ID                                -- Identity: pass the incoming the state as is to the next instruction.
  | DRP                               -- Drop: set the "DRP" field in the header to 1 (i.e., True).
  | ADD     (Fld)                     -- Add header field: set header field (f) to 1 i.e., add field in the header.
  | DEL     (Fld)                     -- Del header field: set header field (f) to 0 i.e., delete field from the header.
  | LDF     (Fld, Val)                -- Load header field: load header field (f) with the value (v).
  | LDFR    (Fld, Reg)                -- Load header field with register: load header field (f) with the register (r).
  | LDFF    (Fld, Fld)                -- Load header field with field: load header field (f0) with the field (f1).
  | LDFT    (Tbl, Val)                -- Load header with table at value: load header with the table (t) at index value (v).
  | LDFTR   (Tbl, Reg)                -- Load header with table at register: load header with the table (t) at index register (r). 
  | LDFTF   (Tbl, Fld)                -- Load header with table at field: load header with the table (t) at index field (f).
  | OPF     (Fld, Fld, Op, Val)       -- f0 = f1 op v: apply operation (op) on field (f1) and value (v) and store it in field (f0).
  | OPFR    (Fld, Fld, Op, Reg)       -- f0 = f1 op r: apply operation (op) on field (f1) and register (r) and store it in field (f0).
  | OPFF    (Fld, Fld, Op, Fld)       -- f0 = f1 op f2: apply operation (op) on field (f1) and field (f2) and store it in field (f0).
  -- Branch instructions.
  -- -- register.
  | BRR     (Reg, CmpOp, Val, Lbl)    -- if (r op v) -> l: branch control to label (l) if the result of the comparison on register (r) and value (v) is true.
  | BRRR    (Reg, CmpOp, Reg, Lbl)    -- if (r0 op r1) -> l: branch control to label (l) if the result of the comparison on register (r0) and register (r1) is true.
  | BRRF    (Reg, CmpOp, Fld, Lbl)    -- if (r op f) -> l: branch control to label (l) if the result of the comparison on register (r) and field (f) is true.
  -- -- table.
  | BRTR    (Tbl, Reg, Lbl)           -- Branch on table at register: branch control to label (l) based on whether a pattern in table (t) is present in the header 
                                      -- and set the register (r) to matched index.
  | BRTF    (Tbl, Fld, Lbl)           -- Branch on table at field: branch control to label (l) based on whether a pattern in table (t) is present in the header 
                                      -- and set the field (f) to matched index.
  | IBRTR   (Tbl, Reg, Lbl)           -- Branch on table at register: branch control to label (l) based on whether a pattern in table (t) is not present in the header 
                                      -- and set the register (r) to matched index.
  | IBRTF   (Tbl, Fld, Lbl)           -- Branch on table at field: branch control to label (l) based on whether a pattern in table (t) is not present in the header 
                                      -- and set the field (f) to matched index.
  -- -- header fields.
  | BRF     (Fld, CmpOp, Val, Lbl)    -- if (f op v) -> l: branch control to label (l) if the result of the comparison on field (f) and value (v) is true.
  | BRFR    (Fld, CmpOp, Reg, Lbl)    -- if (f op r) -> l: branch control to label (l) if the result of the comparison on field (f) and register (r) is true.
  | BRFF    (Fld, CmpOp, Fld, Lbl)    -- if (f0 op f1) -> l: branch control to label (l) if the result of the comparison on field (f0) and field (f1) is true.
  deriving Show
--  | CNCR  ??
--  | CHKSUM ??
--  | COUNT ??

-- Control Instructions.
data CtrlInstr = 
    WRR (Reg, Val)        -- Write register: write register (r) with value (v).
  -- | RDR (Reg)
  | WRT (Tbl, Ptrn, Val)  -- Write table with pattern (f,v) at value: write table (t) with pattern (p) at index value (v).
  -- | RDT (Tbl, Val)

-----------------------
-- Semantics of NetASM.
-----------------------

-- Register collection mapping name to tuple (register, value).
type Regs = Map Reg Val

-- Table collection mapping name to tuple (table, pattern).
type Tbls = Map String (Tbl, [Ptrn])

-- State of the abstract machine.
type State = (Hdr, Regs, Tbls)

-- Profiling information: state of the abstract machine
-- extended with the number of instructions executed.
type Prof = (State, Int)

-- Create registers and tables and update the system state.
nextInit :: (InitInstr, State) -> State
nextInit (i, (h, rs, ts)) = 
  case i of
    MKR(r, v)                            ->  incPID $ mkr((h, rs, ts), r, v)
    MKT(Dynamic(n, (s, fs)), Static(ps)) ->  incPID $ mkt((h, rs, ts), Dynamic(n, (s, fs)), Static(ps))
    
-- Create register with default value.
mkr :: (State, Reg, Val) -> State
mkr ((h, rs, ts), r, v)
  | (includes(rs, r)) = (h, rs, ts) -- error "Error: register already created."
  | otherwise         = (h, insert(rs, r, v), ts)
  
-- Create dynamic table.
mkt :: (State, Tbl, Tbl) -> State
mkt ((h, rs, ts), Dynamic(n, (s, fs)), Static(sps))
  | not (isVldTbl(fs, Static(sps))) = error "Error: the input pattern doesn't comply with the table pattern."
  | (includes(ts, n))               = (h, rs, ts) -- error "Error: table already created."
  | otherwise                       = (h, rs, insert(ts, n, ((Dynamic(n, (s, fs))), dps)))
  where dps = sps

-- Execute topology instructions and update the system state.
nextTopo :: (TopoCode, State) -> State
nextTopo (c, (h, rs, ts)) =
  case c!!fst(h!"PID") of
    LBL(l)                                  ->  incPID $ lbl       (h, rs, ts)        
    JMP(l)                                  ->  incPID $ jmp   (c, (h, rs, ts), l)
    -- Register update instructions.
    LDR(r, v)                               ->  incPID $ ldr   (   (h, rs, ts), r, v)
    LDRR(r, r0)                             ->  incPID $ ldrr  (   (h, rs, ts), r, r0)
    LDRF(r, f)                              ->  incPID $ ldrf  (   (h, rs, ts), r, f)
    OPR(r, r0, op, v)                       ->  incPID $ opr   (   (h, rs, ts), r, r0, op, v)
    OPRR(r, r0, op, r1)                     ->  incPID $ oprr  (   (h, rs, ts), r, r0, op, r1)
    OPRF(r, r0, op, f)                      ->  incPID $ oprf  (   (h, rs, ts), r, r0, op, f)
    -- Table update instructions.
    LDTP(Dynamic(n, (s, fs)), p, i)         ->  incPID $ ldtp  (   (h, rs, ts), Dynamic(n, (s, fs)), p, i)
    LDTPR(Dynamic(n, (s, fs)), p, r)        ->  incPID $ ldtpr (   (h, rs, ts), Dynamic(n, (s, fs)), p, r)
    LDTPF(Dynamic(n, (s, fs)), p, f)        ->  incPID $ ldtpf (   (h, rs, ts), Dynamic(n, (s, fs)), p, f)
    LDTF(Dynamic(n, (s, fs)), p, i)         ->  incPID $ ldtf  (   (h, rs, ts), Dynamic(n, (s, fs)), p, i)
    LDTFR(Dynamic(n, (s, fs)), p, r)        ->  incPID $ ldtfr (   (h, rs, ts), Dynamic(n, (s, fs)), p, r)
    LDTFF(Dynamic(n, (s, fs)), p, f)        ->  incPID $ ldtff (   (h, rs, ts), Dynamic(n, (s, fs)), p, f)
    -- Header field update instructions.
    ID                                      ->  incPID $ id        (h, rs, ts)
    DRP                                     ->  incPID $ drop      (h, rs, ts)
    ADD(f)                                  ->  incPID $ add   (   (h, rs, ts), f)
    DEL(f)                                  ->  incPID $ del   (   (h, rs, ts), f)
    LDF(f, v)                               ->  incPID $ ldf   (   (h, rs, ts), f, v)
    LDFR(f, r)                              ->  incPID $ ldfr  (   (h, rs, ts), f, r)
    LDFF(f, f0)                             ->  incPID $ ldff  (   (h, rs, ts), f, f0)
    LDFT(t, v)                              ->  incPID $ ldft  (   (h, rs, ts), t, v)
    LDFTR(t, r)                             ->  incPID $ ldftr (   (h, rs, ts), t, r)
    LDFTF(t, f)                             ->  incPID $ ldftf (   (h, rs, ts), t, f)
    OPF(f, f0, op, v)                       ->  incPID $ opf   (   (h, rs, ts), f, f0, op, v)
    OPFR(f, f0, op, r)                      ->  incPID $ opfr  (   (h, rs, ts), f, f0, op, r)
    OPFF(f, f0, op, f1)                     ->  incPID $ opff  (   (h, rs, ts), f, f0, op, f1)
    -- Branch instrucions.
    BRR(r, op, v, l)                        ->  incPID $ brr   (c, (h, rs, ts), r, op, v, l)
    BRRR(r, op, r0, l)                      ->  incPID $ brrr  (c, (h, rs, ts), r, op, r0, l)
    BRRF(r, op, f, l)                       ->  incPID $ brrf  (c, (h, rs, ts), r, op, f, l)
    BRTR(t, r, l)                           ->  incPID $ brtr  (c, (h, rs, ts), t, r, l)
    BRTF(t, f, l)                           ->  incPID $ brtf  (c, (h, rs, ts), t, f, l)
    IBRTR(t, r, l)                          ->  incPID $ ibrtr (c, (h, rs, ts), t, r, l)
    IBRTF(t, f, l)                          ->  incPID $ ibrtf (c, (h, rs, ts), t, f, l)
    BRF(f, op, v, l)                        ->  incPID $ brf   (c, (h, rs, ts), f, op, v, l)
    BRFR(f, op, r, l)                       ->  incPID $ brfr  (c, (h, rs, ts), f, op, r, l)
    BRFF(f, op, f0, l)                      ->  incPID $ brff  (c, (h, rs, ts), f, op, f0, l)
  
-- Increment PID.
incPID :: State -> State
incPID (h, rs, ts) = (insert(h, "PID", (fst(h!"PID")+1, True)), rs, ts)

-- Label for jump and branch instructions.
lbl :: State -> State
lbl (h, rs, ts) = (h, rs, ts)

-- Determine id of given label in code.
lblId :: (TopoCode, Lbl) -> Int
lblId(c, l) = lblId'(c, l, 0)

lblId' :: (TopoCode, Lbl, Int) -> Int
lblId'(LBL(l):c, l0, n) | l == l0 = n
lblId'(     i:c, l, n)            = lblId'(c, l, n+1)

-- Jump to fixed value PID.
jmp :: (TopoCode, State, Lbl) -> State
jmp (c, (h, rs, ts), l)
  | not (v > fst(h!"PID")) = error "Error: only forward jumps are allowed."
  | otherwise              = (insert(h, "PID", (v, True)), rs, ts)
  where v = lblId(c, l)

-- Load register with value.
ldr :: (State, Reg, Val) -> State
ldr ((h, rs, ts), r, v)
  | not (includes(rs, r)) = error "Error: invalid register, use MKR to create the register."
  | otherwise             = (h, insert(rs, r, v), ts)

-- Load register with register.
ldrr :: (State, Reg, Reg) -> State
ldrr ((h, rs, ts), r, r0) = ldr((h, rs, ts), r, v)
  where v = rs!r0

-- Load register with field.
ldrf :: (State, Reg, Fld) -> State
ldrf ((h, rs, ts), r, f) = ldr((h, rs, ts), r, v)
  where v = fst(h!f)

-- Operate on register (add bit checks)
-- with value. 
opr :: (State, Reg, Reg, Op, Val) -> State
opr ((h, rs, ts), r, r0, op, v) = (h, insert(rs, r, opert(rs!r0, op, v)), ts)

-- with register.
oprr :: (State, Reg, Reg, Op, Reg) -> State
oprr ((h, rs, ts), r, r0, op, r1) = opr((h, rs, ts), r, r0, op, v)
  where v = rs!r1

-- with field.
oprf :: (State, Reg, Reg, Op, Fld) -> State
oprf ((h, rs, ts), r, r0, op, f) = opr((h, rs, ts), r, r0, op, v)
 where v = fst(h!f)

-- Load table with pattern (fields to values)
-- at index value.
ldtp :: (State, Tbl, Ptrn, Val) -> State
ldtp ((h, rs, ts), Dynamic(n, (s, fs)), p, i)
  | not (isVldPtrn(fs, p))           = error "Error: the input pattern doesn't comply with the table pattern."
  | not (includes(ts, n) && (i < s)) = error "Error: invalid table or index out of range, use MKT to create the table."
  | otherwise                        = (h, rs, insert(ts, n, (fst(ts!n), xs)))
  where xs = Li.updateAt(i, snd(ts!n), p)

-- at index in register.
ldtpr :: (State, Tbl, Ptrn, Reg) -> State
ldtpr ((h, rs, ts), Dynamic(n, (s, fs)), p, r) = ldtp((h, rs, ts), Dynamic(n, (s, fs)), p, i)
  where i = rs!r

-- at index in field.
ldtpf :: (State, Tbl, Ptrn, Fld) -> State
ldtpf ((h, rs, ts), Dynamic(n, (s, fs)), p, f) 
  | isSpclFld(f) = error "Error: cannot operate on special fields."
  | otherwise    = ldtp((h, rs, ts), Dynamic(n, (s, fs)), p, i)
  where i = fst(h!f)

-- Load table with pattern (fields to fields)
-- at index value.
ldtf :: (State, Tbl, Map Fld Fld, Val) -> State
ldtf ((h, rs, ts), Dynamic(n, (s, fs)), p, i) = ldtp((h, rs, ts), Dynamic(n, (s, fs)), p', i)
  where p' = [(f, fst(h!f0)) | (f, f0) <- p]

-- at index in register.
ldtfr :: (State, Tbl, Map Fld Fld, Reg) -> State
ldtfr ((h, rs, ts), Dynamic(n, (s, fs)), p, r) = ldtf((h, rs, ts), Dynamic(n, (s, fs)), p, i)
  where i = rs!r

-- at index in field.
ldtff :: (State, Tbl, Map Fld Fld, Fld) -> State
ldtff ((h, rs, ts), Dynamic(n, (s, fs)), p, f) 
  | isSpclFld(f) = error "Error: cannot operate on special fields."
  | otherwise    = ldtf((h, rs, ts), Dynamic(n, (s, fs)), p, i)
  where i = fst(h!f)

-- Identity.
id :: State -> State
id (h, rs, ts) = (h, rs, ts)

-- Drop.
drop :: State -> State
drop (h, rs, ts) = (insert(h, "DRP", (1, True)), rs, ts)

-- Add.
add :: (State, Fld) -> State
add ((h, rs, ts), f) = (insert(h, f, (fst(h!f), True)), rs, ts)

-- Delete.
del :: (State, Fld) -> State
del ((h, rs, ts), f) = (insert(h, f, (fst(h!f), False)), rs, ts)

-- Modify with value.
ldf :: (State, Fld, Val) -> State
ldf ((h, rs, ts), f, v)
  | isSpclFld(f) = error "Error: cannot modify special fields."
  | otherwise    = (insert(h, f, (v, snd(h!f))), rs, ts)

-- Modify with register.
ldfr :: (State, Fld, Reg) -> State
ldfr ((h, rs, ts), f, r) = ldf((h, rs, ts), f, v)
  where v = rs!r

-- Modify with field.
ldff :: (State, Fld, Fld) -> State
ldff ((h, rs, ts), f, f0) = ldf((h, rs, ts), f, v)
  where v = fst(h!f0)

-- Modify with table
-- at index value
ldft :: (State, Tbl, Val) -> State
ldft ((h, rs, ts), t, v) =
  case t of
    Static(sps)         -> (ldft'(h, sps!!v), rs, ts)
    Dynamic(n, (s, fs)) -> (ldft'(h, dps!!v), rs, ts)
      where dps = snd(ts!n)

-- at index value in the register.
ldftr :: (State, Tbl, Reg) -> State
ldftr ((h, rs, ts), t, r) = ldft((h, rs, ts), t, v)
  where v = rs!r

-- at index value in the field.
ldftf :: (State, Tbl, Fld) -> State
ldftf ((h, rs, ts), t, f) 
  | isSpclFld(f) = error "Error: cannot modify special fields."
  | otherwise    = ldft ((h, rs, ts), t, v)
  where v = fst(h!f)

ldft' :: (Hdr, Ptrn) -> Hdr
ldft' (h, []) = h
ldft' ([], _) = []
ldft' (((f, v):hs), p) = 
  (
    if (includes(p, f)) 
      then (f, (p!f, snd(v)))
      else (f, v)
  )
  : ldft'(hs, p)

-- Operate on field
-- with value.
opf :: (State, Fld, Fld, Op, Val) -> State
opf ((h, rs, ts), f, f0, op, v)
  | isSpclFld(f) = error "Error: cannot operate on special fields."
  | otherwise    = (insert(h, f, (opert(fst(h!f0), op, v), snd(h!f))), rs, ts) 

-- with register.
opfr :: (State, Fld, Fld, Op, Reg) -> State
opfr ((h, rs, ts), f, f0, op, r) = opf((h, rs, ts), f, f0, op, v)
  where v = rs!r

-- with field.
opff :: (State, Fld, Fld, Op, Fld) -> State
opff ((h, rs, ts), f, f0, op, f1) = opf((h, rs, ts), f, f0, op, v)
 where v = fst(h!f1)

-- Branch on register
-- with value to fixed value PID.
brr :: (TopoCode, State, Reg, CmpOp, Val, Lbl) -> State
brr (c, (h, rs, ts), r, op, v, l)
  | not (v0 > fst(h!"PID")) = error "Error: only forward branches are allowed."
  | otherwise = 
      if (logic(rs!r, op, v))
        then (insert(h, "PID", (v0, True)), rs, ts)
        else (h, rs, ts)
  where v0 = lblId(c, l)

-- with register to fixed value PID.
brrr :: (TopoCode, State, Reg, CmpOp, Reg, Lbl) -> State
brrr (c, (h, rs, ts), r, op, r0, l) = brr (c, (h, rs, ts), r, op, v, l)
  where v = rs!r0

-- with field to fixed value PID.
brrf :: (TopoCode, State, Reg, CmpOp, Fld, Lbl) -> State
brrf (c, (h, rs, ts), r, op, f, l) = brr (c, (h, rs, ts), r, op, v, l)
  where v = fst(h!f)


-- with a match on table to fixed value PID and set register with matched index.
brtr :: (TopoCode, State, Tbl, Reg, Lbl) -> State
brtr (c, (h, rs, ts), t, r, l)
  | not (v > fst(h!"PID")) = error "Error: only forward branches are allowed."
  | otherwise = 
      case t of
        Static(sps) -> 
          if (b')
            then (insert(h, "PID", (v, True)), rs', ts) 
            else (h, rs, ts)
          where (rs', b') = mtchtr(r, rs, h, sps, 0)
        Dynamic(n, (s, fs)) ->
          if (b') 
            then (insert(h, "PID", (v, True)), rs', ts)
            else (h, rs, ts)
          where dps  = snd(ts!n)
                (rs', b') = mtchtr(r, rs, h, dps, 0)
  where v = lblId(c, l)

-- with not a match on table to fixed value PID and set register with matched index.
ibrtr :: (TopoCode, State, Tbl, Reg, Lbl) -> State
ibrtr (c, (h, rs, ts), t, r, l)
  | not (v > fst(h!"PID")) = error "Error: only forward branches are allowed."
  | otherwise = 
      case t of
        Static(sps) -> 
          if (b')
            then (h, rs', ts) 
            else (insert(h, "PID", (v, True)), rs, ts) 
          where (rs', b') = mtchtr(r, rs, h, sps, 0)
        Dynamic(n, (s, fs)) ->
          if (b') 
            then (h, rs', ts) 
            else (insert(h, "PID", (v, True)), rs, ts)
          where dps  = snd(ts!n)
                (rs', b') = mtchtr(r, rs, h, dps, 0)
  where v = lblId(c, l)

mtchtr :: (Reg, Regs, Hdr, [Ptrn], Val) -> (Regs, Bool)
mtchtr (_, rs, _, [], _) = (rs, False)
mtchtr (r, rs, h, (xs:xss), i)
  | isMtch = (insert(rs, r, i), True)
  | otherwise = mtchtr(r, rs, h, xss, i+1)
  where isMtch = all (`elem` [(f', v') | (f', (v', b')) <- h]) xs

-- with a match on table to fixed value PID and set field with matched index.
brtf :: (TopoCode, State, Tbl, Fld, Lbl) -> State
brtf (c, (h, rs, ts), t, f, l)
  | not (v > fst(h!"PID")) = error "Error: only forward branches are allowed."
  | isSpclFld(f)           = error "Error: cannot compare on special fields."
  | otherwise = 
      case t of
        Static(sps) -> 
          if (b')
            then (insert(h', "PID", (v, True)), rs, ts) 
            else (h, rs, ts)
          where (h', b') = mtchtf(f, h, sps, 0)
        Dynamic(n, (s, fs)) ->
          if (b') 
            then (insert(h', "PID", (v, True)), rs, ts)
            else (h, rs, ts)
          where dps  = snd(ts!n)
                (h', b') = mtchtf(f, h, dps, 0)
  where v = lblId(c, l)

-- with not a match on table to fixed value PID and set field with matched index.
ibrtf :: (TopoCode, State, Tbl, Fld, Lbl) -> State
ibrtf (c, (h, rs, ts), t, f, l)
  | not (v > fst(h!"PID")) = error "Error: only forward branches are allowed."
  | isSpclFld(f)           = error "Error: cannot compare on special fields."
  | otherwise = 
      case t of
        Static(sps) -> 
          if (b')
            then (h', rs, ts) 
            else (insert(h, "PID", (v, True)), rs, ts) 
          where (h', b') = mtchtf(f, h, sps, 0)
        Dynamic(n, (s, fs)) ->
          if (b') 
            then (h', rs, ts) 
            else (insert(h, "PID", (v, True)), rs, ts)
          where dps  = snd(ts!n)
                (h', b') = mtchtf(f, h, dps, 0)
  where v = lblId(c, l)

mtchtf :: (Fld, Hdr, [Ptrn], Val) -> (Hdr, Bool)
mtchtf (_, h, [], _) = (h, False)
mtchtf (f, h, (xs:xss), i)
  | isMtch = (insert(h, f, (i, snd(h!f))), True)
  | otherwise = mtchtf(f, h, xss, i+1)
  where isMtch = all (`elem` [(f', v') | (f', (v', b')) <- h]) xs
  
-- Branch on field
-- with value to fixed value PID.
brf :: (TopoCode, State, Fld, CmpOp, Val, Lbl) -> State
brf (c, (h, rs, ts), f, op, v, l)
  | not (v0 > fst(h!"PID")) = error "Error: only forward branches are allowed."
  | isSpclFld(f)            = error "Error: cannot compare on special fields."
  | otherwise = 
      if (logic(fst(h!f), op, v))
        then (insert(h, "PID", (v0, True)), rs, ts)
        else (h, rs, ts)
  where v0 = lblId(c, l)

-- with register to fixed value PID.
brfr :: (TopoCode, State, Fld, CmpOp, Reg, Lbl) -> State
brfr (c, (h, rs, ts), f, op, r, l) = brf(c, (h, rs, ts), f, op, v, l)
  where v = rs!r

-- with field to fixed value PID.
brff :: (TopoCode, State, Fld, CmpOp, Fld, Lbl) -> State
brff (c, (h, rs, ts), f, op, f0, l) = brf(c, (h, rs, ts), f, op, v, l)
  where v = fst(h!f0)
  
-- Execute control instructions and update the system state.
nextCtrl :: (CtrlInstr, State) -> State
nextCtrl (i, (h, rs, ts)) =
  case i of
    WRR(r, v)                      ->  ldr ((h, rs, ts), r, v)
    WRT(Dynamic(n, (s, fs)), p, i) ->  ldtp((h, rs, ts), Dynamic(n, (s, fs)), p, i)

-- Helper functions.
-- Arithematic and Bitwise operations.
opert :: (Int, Op, Int) -> Int
opert (x, Add, y) = x + y
opert (x, Sub, y) = x - y
opert (x, And, y) = x .&. y
opert (x, Or,  y) = x .|. y
opert (x, Xor, y) = x `xor` y

-- Logic.
logic :: (Int, CmpOp, Int) -> Bool
logic (x, Eq, y)  = x==y
logic (x, Neq, y) = x/=y
logic (x, Lt, y)  = x<y
logic (x, Gt, y)  = x>y
logic (x, Le, y)  = x<=y
logic (x, Ge, y)  = x>=y

-- Checks.
-- if it is a special field.
isSpclFld :: (Fld) -> Bool
isSpclFld (f) = f `elem` ["DRP", "PID"]

-- if the pattern is valid and comply with the fields.
isVldPtrn :: ([Fld], Ptrn) -> Bool
isVldPtrn (fs, p) = fs==fs'
  where fs' = [f | (f, v) <- p]

-- if the static table is valid and comply with the fields.
isVldTbl :: ([Fld], Tbl) -> Bool
isVldTbl (_, Static([])) = True
isVldTbl (fs, Static(p:ps)) = isVld && isVldTbl(fs, Static(ps))
  where isVld = isVldPtrn(fs, p)

-- Run initialisation intructions.
runInit :: (InitCode, State) -> State
runInit ([], s) = s
runInit ((i:cs), s) = runInit(cs, nextInit(i, s))

-- Run topology intructions.
runTopo :: (TopoCode, Prof) -> Prof
runTopo (c, (s, n)) =
  case c!!fst(h!"PID") of
    HLT      -> (s, n)
    LBL(l)   -> runTopo(c, (nextTopo(c, s), n))
    other    -> runTopo(c, (nextTopo(c, s), n+1))
  where (h, _, _) = s
  
-- Run control intructions.
runCtrl :: (CtrlInstr, State) -> State
runCtrl (i, s) = nextCtrl(i, s)

-- Emulator: takes (initialisation code, list of headers, topology code) and produces list of headers.
emulate :: (InitCode, [Input], TopoCode) -> [Hdr]
emulate (_, [], _) = error "Error: empty header list."
emulate (_, _, []) = error "Error: empty code list."
emulate (ic, is, tc) = emulateInput(is, tc, s)
  where s = emulateInit(ic)

emulateInit :: (InitCode) -> State
emulateInit c = runInit(c, ([], [], []))

emulateInput :: ([Input], TopoCode, State) -> [Hdr]
emulateInput ([], _, _) = []
emulateInput ((CTRL(i):is), c, (_, rs, ts)) = emulateInput(is, c, ([], rs', ts'))
  where (_, rs', ts') = runCtrl(i, ([], rs, ts))
emulateInput ((HDR(h):is), c, (_, rs, ts))  = h':emulateInput(is, c, ([], rs', ts'))
  where ((h', rs', ts'), n) = runTopo(c, ((h, rs, ts), 0))

-- Profiler: takes (intialize code, list of headers, topology code) and produces profile report.
profile :: (InitCode, [Input], TopoCode) -> String
profile (ic, hs, tc) =
     "Instructions executed: " ++ show(n)
  where n = profileInput(hs, tc, s)
        s = profileInit(ic)
  
profileInit :: (InitCode) -> State
profileInit c = runInit(c, ([], [], []))

profileInput :: ([Input], TopoCode, State) -> Int
profileInput ([], _, _) = 0
profileInput ((CTRL(i):is), c, (_, rs, ts)) = profileInput(is, c, ([], rs', ts'))
  where (_, rs', ts') = runCtrl(i, ([], rs, ts))
profileInput ((HDR(h):is), c, (_, rs, ts))  = n + profileInput(is, c, ([], rs', ts'))
  where ((_, rs', ts'), n) = runTopo(c, ((h, rs, ts), 0))

-- Debugger.
debugInit :: (InitCode) -> State
debugInit (c) = ([], rs, ts)
  where (_, rs, ts) = runInit(c, ([], [], []))

debugInput :: (Input, TopoCode, State) -> (State, Int)
debugInput (CTRL(i), _, (_, rs, ts)) = (([], rs', ts'), 0)
  where (_, rs', ts') = runCtrl(i, ([], rs, ts))
debugInput (HDR(h), c, (_, rs, ts))  = ((h', rs', ts'), n)
  where ((h', rs', ts'), n) = runTopo(c, ((h, rs, ts), 0))

-- Printer.
print' :: (State, Int) -> String
print' ((h, rs, ts), n) = 
      "Header: "++show(h)++"\n"
    ++"Registers: "++show(rs)++"\n"
    ++"Tables: "++show(ts)++"\n"
    ++"Profile (instructions executed): "++show(n)++"\n\n"
    
-- Pretty Print
prettyPrint :: [Hdr] -> String
prettyPrint ([])   = ""
prettyPrint (h:hs) = show h 
                  ++ "\n" 
                  ++ prettyPrint(hs)

-- Constants.
_1s :: Int
_1s = (2^31-1)