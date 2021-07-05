# Automatically generated constU MPs over E/W multiplier grid 

source("BR3.R")

# EAST MPs
# pH30
BR3_E_alp3.806 <- BR_E3s 
formals(BR3_E_alp3.806)$alp <- 3.806
class(BR3_E_alp3.806) <- 'MP' 

BR3_E_alp3.809 <- BR_E3s 
formals(BR3_E_alp3.809)$alp <- 3.809
class(BR3_E_alp3.809) <- 'MP' 

BR3_E_alp10hiCap <- BR_E3s 
formals(BR3_E_alp10hiCap)$alp <- 10
formals(BR3_E_alp10hiCap)$TACcap <- 55
class(BR3_E_alp10hiCap) <- 'MP' 

# West MPs
Br3_W_bet1.01 <- BR_E3s 
formals(Br3_W_bet1.01)$bet <- 1.01
class(Br3_W_bet1.01) <- 'MP' 

Br3_W_bet1.00 <- BR_E3s 
formals(Br3_W_bet1.00)$bet <- 1.00
class(Br3_W_bet1.00) <- 'MP' 

Br3_W_bet1.2hiCap <- BR_W3s 
formals(Br3_W_bet1.2hiCap)$bet <- 1.2
formals(Br3_W_bet1.2hiCap)$TACcap <- Inf
class(Br3_W_bet1.2hiCap) <- 'MP' 

# "grid"
gridMPs <- list(c('BR3_E_alp3.806', 'Br3_W_bet1.01'),
                c('BR3_E_alp10hiCap', 'Br3_W_bet1.2_hiCap'),
                c('BR3_E_alp3.809', 'Br3_W_bet1.00'))


