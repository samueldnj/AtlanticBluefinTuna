# AtlanticBluefinTuna

A repository holding local versions of the ABTMSE package and
some scripts for testing changes to the package required to
run the DD model based MP.

# Installation

Clone the repository, navigate to `testDDMP/` and open
an `R` instance. Then

```
source("runExampleMSE.R")
```

This should install all required CRAN/github packages, 
force install the ABTMSE package (ver 4.4.5.999) from the 
local folder, and run a couple of example closed loop
simulations.

# Modifications to the ABTMSE package

We made a few modifications to the ```ABTMSE``` package to 
integrate the tunaDelay MP. The changes are detailed at 
the bottom of this readme, but we'll list the files here,
including their location in the directory tree:

```
ABTMSE/
|-R/
| |- TMBMPs.R
| |- MSE_Object.R*
|
|-Src/
| |- tunaDelay.cpp
| |- Makefile
|
|-data/
  |- indicesOM.RData
```

Any further modifications should be limited to these files
to avoid corruption of the core functions of the ABTMSE package. 
Note that the `ABTMSE/R/MSE_Object.R` file is the closed loop
simulator, so make backups of this file before modifying anything.

To test updates, start a new R session and follow the installation
instructions - this will force install the updated ABTMSE package
and run an example MSE loop.

# Detailed modifications



