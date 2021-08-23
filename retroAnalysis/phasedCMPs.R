# script defining the phased versions 
# of each CMP


source("BR3.R")
source("constU.R")
source("EA_1.R")

# EA CMPs

EA_gammE <- 0.15
EA_targE <- 1.0
EA_capE  <- 5e7

EA_gammW <- 0.15
EA_targW <- 2

formals(EA_1_E)$cap <- EA_capE

# No phase
EA_E <- EA_1_E
formals(EA_E)$phaseTime <- 0
formals(EA_E)$Targ <- EA_targE
formals(EA_E)$Gamma <- EA_gammE
class(EA_E) <- "MP"

EA_W <- EA_1_W
formals(EA_W)$phaseTime <- 0
formals(EA_W)$Targ <- EA_targW
formals(EA_W)$Gamma <- EA_gammW
class(EA_W) <- "MP"


# 5-year
EA_E_Phz5 <- EA_1_E
formals(EA_E_Phz5)$phaseTime <- 5
formals(EA_E_Phz5)$Targ <- EA_targE
formals(EA_E_Phz5)$Gamma <- EA_gammE
class(EA_E_Phz5) <- "MP"

EA_W_Phz5 <- EA_1_W
formals(EA_W_Phz5)$phaseTime <- 5
formals(EA_W_Phz5)$Targ <- EA_targW
formals(EA_W_Phz5)$Gamma <- EA_gammW
class(EA_W_Phz5) <- "MP"

# 10-year
EA_E_Phz10 <- EA_1_E
formals(EA_E_Phz10)$phaseTime <- 10
formals(EA_E_Phz10)$Targ <- EA_targE
formals(EA_E_Phz10)$Gamma <- EA_gammE
class(EA_E_Phz10) <- "MP"

EA_W_Phz10 <- EA_1_W
formals(EA_W_Phz10)$phaseTime <- 10
formals(EA_W_Phz10)$Targ <- EA_targW
formals(EA_W_Phz10)$Gamma <- EA_gammW
class(EA_W_Phz10) <- "MP"


# BR CMPs

alp <- 3.23
bet <- 0.94

# No Phase
BR_E <- BR_E3s
formals(BR_E)$phaseTime <- 0
formals(BR_E)$alp <- alp
class(BR_E) <- "MP"

BR_W <- BR_W3s
formals(BR_W)$phaseTime <- 0
formals(BR_W)$bet <- bet
class(BR_W) <- "MP"


# 5-year
BR_E_Phz5 <- BR_E3s
formals(BR_E_Phz5)$phaseTime <- 5
formals(BR_E_Phz5)$alp <- alp
class(BR_E_Phz5) <- "MP"

BR_W_Phz5 <- BR_W3s
formals(BR_W_Phz5)$phaseTime <- 5
formals(BR_W_Phz5)$bet <- bet
class(BR_W_Phz5) <- "MP"

# 10-year
BR_E_Phz10 <- BR_E3s
formals(BR_E_Phz10)$phaseTime <- 10
formals(BR_E_Phz10)$alp <- alp
class(BR_E_Phz10) <- "MP"

BR_W_Phz10 <- BR_W3s
formals(BR_W_Phz10)$phaseTime <- 10
formals(BR_W_Phz10)$bet <- bet
class(BR_W_Phz10) <- "MP"


# LW CMPs
mE <- 7.0
mW <- 1.61

# no phase
conU_E <- ConstU_E
formals(conU_E)$phaseTime <- 0
formals(conU_E)$multiplierE <- mE
class(conU_E) <- "MP"

conU_W <- ConstU_W
formals(conU_W)$phaseTime <- 0
formals(conU_W)$multiplierW <- mW
class(conU_W) <- "MP"


# 5 yr
conU_E_Phz5 <- ConstU_E
formals(conU_E_Phz5)$phaseTime <- 5
formals(conU_E_Phz5)$multiplierE <- mE
class(conU_E_Phz5) <- "MP"

conU_W_Phz5 <- ConstU_W
formals(conU_W_Phz5)$phaseTime <- 5
formals(conU_W_Phz5)$multiplierW <- mW
class(conU_W_Phz5) <- "MP"

# 10-year
conU_E_Phz10 <- ConstU_E
formals(conU_E_Phz10)$phaseTime <- 10
formals(conU_E_Phz10)$multiplierE <- mE
class(conU_E_Phz10) <- "MP"

conU_W_Phz10 <- ConstU_W
formals(conU_W_Phz10)$phaseTime <- 10
formals(conU_W_Phz10)$multiplierW <- mW
class(conU_W_Phz10) <- "MP"


