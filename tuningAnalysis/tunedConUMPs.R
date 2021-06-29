# Automatically generated constU MPs over E/W multiplier grid 

source("constU.R")

# EAST MPs
# pH30
constU_E_m7.35 <- ConstU_E 
formals(constU_E_m7.35)$multiplierE <- 7.35
class(constU_E_m7.35) <- 'MP' 

# pYrH
constU_E_m8.9 <- ConstU_E 
formals(constU_E_m8.9)$multiplierE <- 8.9
class(constU_E_m8.9) <- 'MP' 


# WEST MPs
# pH30
constU_W_m8.45 <- ConstU_E 
formals(constU_W_m8.45)$multiplierE <- 8.45
class(constU_W_m8.45) <- 'MP' 

# pYrH
constU_W_m9.3 <- ConstU_E 
formals(constU_W_m9.3)$multiplierE <- 9.3
class(constU_W_m9.3) <- 'MP' 

# minpYr
constU_W_m7.9 <- ConstU_E 
formals(constU_W_m7.9)$multiplierE <- 7.9
class(constU_W_m7.9) <- 'MP' 

# "grid"
gridMPs <- list(c('constU_E_m7.35', 'constU_W_m8.45'),
                c('constU_E_m8.9', 'constU_W_m9.3'),
                c('constU_E_m7.35', 'constU_W_m7.9'))


