# Automatically generated constU MPs over E/W multiplier grid 

source("constU.R")

# EAST MPs
# pH30
constU_E_m7.35 <- ConstU_E 
formals(constU_E_m7.35)$multiplierE <- 7.35
class(constU_E_m7.35) <- 'MP' 

# pYrH
constU_E_m8.2 <- ConstU_E 
formals(constU_E_m8.2)$multiplierE <- 8.2
class(constU_E_m8.2) <- 'MP' 


# WEST MPs
# pH30
constU_W_m8.45 <- ConstU_E 
formals(constU_W_m8.45)$multiplierE <- 8.45
class(constU_W_m8.45) <- 'MP' 

# pYrH
constU_W_m9.0 <- ConstU_E 
formals(constU_W_m9.0)$multiplierE <- 9.0
class(constU_W_m9.0) <- 'MP' 

# minpYr
constU_W_m7.9 <- ConstU_E 
formals(constU_W_m7.9)$multiplierE <- 7.9
class(constU_W_m7.9) <- 'MP' 

# "grid"
gridMPs <- list(c('constU_E_m7.35', 'constU_W_m8.45'),
                c('constU_E_m8.2', 'constU_W_m9.0'),
                c('constU_E_m7.35', 'constU_W_m7.9'))


