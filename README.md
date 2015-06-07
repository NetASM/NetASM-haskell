NetASM-haskell
==============
NetASM is a network assembler that takes high-level SDN languages (NetKAT, P4, etc.) and maps the primitives to device-specific directives in FPGAs and various chipsets. It's a nascent work on building a network assembly language for programmable network devices (e.g., FPGAs, RMT, Intel’s FlexPipe, NPUs). NetASM provides assembly instructions that directly reflect the capabilities of the underlying device, thus providing either a human programmer or compiler precise, fine-grained control over the device’s resources. It exposes the details in the language such as creating tables and defining layouts of the processing pipeline. 

It's a glimpse into the future of hardware support for software-defined networking (SDN), where the data plane is no longer a fixed-function device, but rather a fully programmable device whose behavior is dictated by the programmer, with the ability to reconfigure it on-demand.

----------

Overview
--------

Contact Information
===================
Muhammad Shahbaz (shahbaz  at cc.gatech.edu)

Nick Feamster    (feamster at cc.gatech.edu)
