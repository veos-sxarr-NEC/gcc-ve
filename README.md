
# gcc for VE

gcc for VE is developed based on gcc (the GNU Compiler Collection) version 7.1.0.  
User can compile a program for Vector Engine by using this software.  
Please read this document carefully before use.

##### What is VE

VE stands for Vector Engine which is vector processors of [SX-Aurora TSUBASA](https://www.nec.com/en/global/solutions/hpc/sx/index.html). 


## Notice

  1. This software needs binutils for VE. It is available on the [SX-Aurora 
     TSUBASA yum repository including SDK runtime](https://www.hpc.nec/repos/runtime/sdk/) for free.
  2. This software can compile only C language.
      - Only C compiler is available.
  3. This software can't generate vector instructions.
  4. It is required to use ncc, nc++, or nfort which are compilers for SX-Aurora 
     TSUBASA to link objects compiled by ncc, nc++ or nfort.
  5. This software is developed only for the purpose to build glibc for VE. 
     Please note that NEC verified only the functionality required for it.
  6. NEC doesn't support this software as the SX-Aurora TSUBASA product software.
     Please note that NEC is not obligated to respond if user sends a bug report.  

## Known Issues

  1. A binary may malfunction when optimize options are enabled. In such case, 
     use '-O0' option.
  2. Gcc for VE is installed to /opt/nec/ve/bin/gcc.
     When user set /opt/nec/ve/bin in $PATH before /usr/bin, /opt/nec/ve/bin/gcc
     is invoked instead of /usr/bin/gcc.
     - NEC MPI setup script adds /opt/nec/ve/bin into the head of $PATH. Therefore,
       a build of VH side MPI program fails.
     - A command expecting /usr/bin/gcc may invoke /opt/nec/ve/bin/gcc.

##  Disclaimer

  1. The contents of this document may change without prior notice.
  2. NEC assumes no liability for any loss, including loss of earnings, 
     arising from the use of this software.
  3. This software is not intended for use in medical, nuclear, aerospace, mass
     transit or other applications where human life may be at stake or high
     reliability is required, nor is it intended for use in controlling such
     applications. We disclaim liability for any personal injury and property
     damages caused by such use of this software.

## Installation

Please refer to [releases](https://github.com/veos-sxarr-NEC/gcc-ve/releases).

## License

Gcc for VE is distributed under the GNU General Public License, version 3.

##  Bug Report and Sharing of Information

* [Issues](https://github.com/veos-sxarr-NEC/gcc-ve/issues) of this project
* [Aurora Web Forum](https://www.hpc.nec/forums/)
     





