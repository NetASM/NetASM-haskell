# NetASM-haskell
NetASM is a network assembler that takes high-level SDN languages (NetKAT, P4, etc.) and maps the primitives to device-specific directives in FPGAs and various chipsets. It's a nascent work on building a network assembly language for programmable network devices (e.g., FPGAs, RMT, Intel’s FlexPipe, NPUs). NetASM provides assembly instructions that directly reflect the capabilities of the underlying device, thus providing either a human programmer or compiler precise, fine-grained control over the device’s resources. It exposes the details in the language such as creating tables and defining layouts of the processing pipeline. 

It's a glimpse into the future of hardware support for software-defined networking (SDN), where the data plane is no longer a fixed-function device, but rather a fully programmable device whose behavior is dictated by the programmer, with the ability to reconfigure it on-demand.

## Overview

NetASM is analogous to an x86 or MIPS-like instructions set.  However, unlike updating main memory and registers, it defines the layout (i.e., topology and states) of the data plane. Topology defines how the packet is traversed through the data plane, and state refers to the type of memory element (i.e., register or table). These state elements have a well-defined data structure and type declaration in NetASM, which makes it easy to identify bugs early in the compilation process.

The NetASM instruction set has three types of instructions:

 - Initialization: to create state elements (like tables and registers)
 - Topology: to define how the packet is traversed and processed in the data plane
 - Control: to provide an external control to populate the states (i.e., over OpenFlow or other interfaces)

The syntax of the NetASM language is defined in the Core/Language.hs file.

## Setting up NetASM on your VM
NetASM is based on Haskell, so before continuing with the installation process make sure that the Haskell platform is setup on your VM.

### Installing Haskell Platform

Perform the following steps to install Haskell platform on your machine:

- Change to root directory

``` bash
$ cd ~
```
- Install the haskell-platform package from the ubuntu install-base.  This process may take some time (several minutes to tens of minutes) depending on the speed of your connection.

``` bash
$ sudo apt-get install haskell-platform
```

> **Important:** It’s been reported that the above command is failing on most of the VMs. If this is happening with you, install haskell using the following command instead.
> 
> ``` bash
> $ sudo apt-get install ghc6
> ```

- To test if the haskell platform is installed correctly, run ghci (a haskell interpreter)

``` bash
$ ghci
```

- If installation is successful, you should see the following output:

``` bash
GHCi, version 7.6.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude>
```

- On the gchi terminal, type :quit to exit the ghci interpreter

``` bash
Prelude> :quit
```

Once the Haskell platform is setup, you are now ready to install NetASM.

### Installing NetASM

Perform the following steps to install NetASM on your machine:

- Change to root directory

``` bash
$ cd ~
```

- Clone the NetASM-haskell repository

``` bash
$ git clone https://github.com/NetASM/NetASM-haskell.git
```

To test if NetASM is working on your VM, run the Hub example. 

- Change to NetASM-haskell root directory

``` bash
cd ~/NetASM-haskell
```

- Run the Hub example

``` bash
$ runhaskell Apps/Hub/Run.hs
```

- If this test run is successful, you should see the following output:

```
[[("PID",(1,True)),("DRP",(0,True)),("inport",(1,True)),("outport",(2147483646,True))]
[("PID",(1,True)),("DRP",(0,True)),("inport",(1,True)),("outport",(2147483645,True))]]

"Instructions executed: 2"
```

## Understanding NetASM with Hub example 

Now let’s take a look at the Hub example in detail. 

The code listing below shows the program for the Hub example written in NetASM.

``` haskell
module Apps.Hub.Code where

import Utils.Map
import Core.Language
import Core.PacketParser

----------
-- Hub ---
----------

-- Topology code for hub
c = [ OPF("outport", "inport", Xor, _1s) 
    , HLT]
```

Almost every program in NetASM, starts by importing three modules: 

- **Utils.Map:** provides utility functions for operating on maps (i.e., [key, value] pairs)
- **Core.Language:** contains definitions of the syntax and semantics of the NetASM language
- **Core.PacketParser:** not used in this code, but provides helping functions for generating sample headers. 

	> Note: in actual system, based on FPGA or any other network device, this header information will be passed on to NetASM from the packet parser.

A code `c`, shown in the latter half of the program above, is an ordered list of NetASM topology instructions. The code `c` contains two instructions: `OPF` and `HLT`. The `OPF` instruction performs an `Xor` operation on the `inport` field with a vector of all 1s and stores its result in the `outport` field of the header. The `Xor` operation sets all bits of the `outport` field (which is a bitmap) to 1s except the bit for the incoming port, effectively performing a flood operation. A brief description and usage of `OPF` and `HLT` instructions are provided in the Appendix.

### Running the Hub program

To run the Hub program, we have written another program, `Run.hs`, which generates a series of input packet headers and executes the Hub code on them in the order in which they arrived. The following code listing shows the body of the `Run.hs` program.

``` haskell
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
        putStrLn $ prettyPrint $ emulateEx
        putStrLn $               profileEx
```

- As in the Hub program, the first thing we do in the `Run.hs` program is to import the three primary modules, this time also importing the Hub code. 
- We use the `genHdr` function (defined in the `Core.PacketParser` module) to generate two headers `h0` and `h1`. The `genHdr` function takes a list of key value pairs (i.e., header description with default values) as input and returns a header object `Hdr` (as defined in the `Core.Language` module).
- We create a test input sequence `is`, containing the two headers, with `h0` being the first header to be executed.
- We then create two wrapper functions, `emulateEx` and `profileEX`, for emulating and profiling the Hub code, respectively. These functions take *initialization code* (an empty list in this case), input sequence, and the program code as arguments. The `emulate` and `profile` functions are defined in the `Core.Language` module.
- Finally, `main` executes these two functions.

To test the Hub program:

- Change to NetASM-haskell root directory

``` bash
$ cd ~/NetASM-haskell
```

- Run the program

``` bash
$ runhaskell Apps/Hub/Run.hs
```

### Analysing output of the Hub program

```
[[("PID",(1,True)),("DRP",(0,True)),("inport",(1,True)),("outport",(2147483646,True))]
[("PID",(1,True)),("DRP",(0,True)),("inport",(1,True)),("outport",(2147483645,True))]]

"Instructions executed: 2" 
```

The output is an updated list of headers, `h0` and `h1`. The Hub code sets the outport field (bitmap) to all ones except the incoming port, effectively, flooding the packet on all ports except the incoming one. Also, notice that there are two extra fields in the header: `PID` and `DRP`. These are special fields and can’t be modified by the programmer, directly. The `PID` field contains the `ID` of the current instruction that is processing the header. The `DRP` field indicates whether the packet is to be dropped or not. The last line shows the number of assembly instructions executed during this run.

> The `outport` field is a 31-bit wide bitmap, where each bit corresponds to an egress interface. If all bits are set to 1 then, in hex, it’s shown as: `7FFFFFFF`, meaning send packets to all egress interfaces. Now, in the case of flood, when the packet comes in from the first input port then the flood value for `outport` field will be: `7FFFFFFE` i.e., last bit set to 0 which in decimal notation is shown as `2147483646`.

## APPENDIX: List of NetASM Instruction Set

### Initialization Instructions

Instruction | Description | Usage
----------- | ----------- | -----
**MKR (Reg, Val)** | **Make Register:** make a new register (`r`) with default value (`v`) | **MKR(“r0”, 0):** make a register `r0` with default value of 0
**MKT (Tbl, Tbl)** | **Make Table:** Make a new dynamic table (`t`) and load it with content from static table (`t0`) |  **MKT(t0, v0):** make a new dynamic table `t0` and load it with defaults values `v0`. `t0` and `v0` are defined using Dynamic and Static table types.

> ``` haskell
> Tbl = Dynamic (String, (Int, [Fld]))
> Tbl = Static  ([Ptrn]) 
> ```

### Topology Instructions

Instruction | Description | Usage
----------- | ----------- | -----
**HLT** | **Halt:** Indicates the end of topology code | see Hub example above
**OPF (Fld, Fld, Op, Val)** | **f0 = f1 op v:** apply operation (`op`) on field (`f1`) and value (`v`) and store it in field (`f0`) | **OPF(“outport”, “inport”, Add, 1):** Add 1 to inport and store it in outport field
**OPR (Reg, Reg, Op, Val)** | **r0 = r1 op v:** apply operation (`op`) on register (`r1`) and value (`v`) and store it in register (`r0`) | **OPF(“r0”, “r1”, Add, 1):** Add 1 to `r1` and store it in `r0` register
**LDR (Reg, Val)** | **Load Register:** load register (`r`) with value (`v`) | **LDR(“r0”, 10):** Loads registers `r0` with value 10
**LBL (Lbl)** | **Label:** label (`l`) for jump and branch instructions | **LBL(“l0”):** Creates a label `l0`
**JMP (Lbl)** | **Jump:** jump control to label (`l`) | **JMP(“l0”):** Skips the instructions till the label
**BRR (Reg, CmpOp, Val, Lbl)** | **if (r op v) then l else fallthrough:** branch control to label (`l`) if the result of the comparison on register (`r`) and value (`v`) is true else fall through to the next instruction | **BRR(“r0”, Gt, 10, “l0”):** Jump to label `l0`, if `r0` is greater than 10
**IBRTF (Tbl, Fld, Lbl)** | Branch control to label (`l`) if any pattern in table (`t`) is not present in the header, else, move to the next instruction and set the field (`f`) to matched index | **IBRTF(t0, “i”, “l0”):** Jump to label `l0` if any pattern in table `t0` does not match the header, else, move to the next instruction and set the field `i` to matched index
**LDFTF (Tbl, Fld)** | **Load header with table at field:** load header with the table (`t`) at index field (`f`) | **LDFTF(t0, “i”):** Load header with contents of table `t0` at index value in field `i`
**LDFTR (Tbl, Reg)** | **Load header with table at register:** load header with the table (`t`) at index register (`r`) | **LDFTF(t0, “r0”):** Load header with contents of table `t0` at index value in register `r0`

> ```haskell 
> Op = Add | Sub | And | Or | Xor 
> CmpOp = Eq | Neq | Lt | Gt | Le | Ge 
> ```

### Control Instructions

Instruction | Description | Usage
----------- | ----------- | -----
**WRR (Reg, Val)** | **Write Register:** write register (`r`) with value (`v`) | **WRR(“r0”, 10):** Write register `r0` with default value of 10
**WRT (Tbl, Ptrn, Val)** | **Write table with pattern [(f,v)] at index value:** write table (`t`) with pattern (`p`) at index value (`v`) | **WRT(t0, [("inport", 1)], 1):** Write the pattern, `inport=1`, in table `t0` at index value 1

# Contact Information
- [Muhammad Shahbaz](http://www.cs.princeton.edu/~mshahbaz/)
- [Nick Feamster](http://www.cs.princeton.edu/~feamster/)

**Email:** lastname @ cs.princeton.edu
