# Automatically generated EA MPs over E/W Gamma grid 



# eGamma = 0.1
EA_1_E_gam0.1 <- EA_1_E 
formals(EA_1_E_gam0.1)$Gamma <- 0.1
class(EA_1_E_gam0.1) <- 'MP' 


# eGamma = 0.3
EA_1_E_gam0.3 <- EA_1_E 
formals(EA_1_E_gam0.3)$Gamma <- 0.3
class(EA_1_E_gam0.3) <- 'MP' 


# eGamma = 0.5
EA_1_E_gam0.5 <- EA_1_E 
formals(EA_1_E_gam0.5)$Gamma <- 0.5
class(EA_1_E_gam0.5) <- 'MP' 


# eGamma = 0.7
EA_1_E_gam0.7 <- EA_1_E 
formals(EA_1_E_gam0.7)$Gamma <- 0.7
class(EA_1_E_gam0.7) <- 'MP' 


# eGamma = 0.9
EA_1_E_gam0.9 <- EA_1_E 
formals(EA_1_E_gam0.9)$Gamma <- 0.9
class(EA_1_E_gam0.9) <- 'MP' 


# wGamma = 0.1
EA_1_W_gam0.1 <- EA_1_W 
formals(EA_1_W_gam0.1)$Gamma <- 0.1
class(EA_1_W_gam0.1) <- 'MP' 


# wGamma = 0.3
EA_1_W_gam0.3 <- EA_1_W 
formals(EA_1_W_gam0.3)$Gamma <- 0.3
class(EA_1_W_gam0.3) <- 'MP' 


# wGamma = 0.5
EA_1_W_gam0.5 <- EA_1_W 
formals(EA_1_W_gam0.5)$Gamma <- 0.5
class(EA_1_W_gam0.5) <- 'MP' 


# wGamma = 0.7
EA_1_W_gam0.7 <- EA_1_W 
formals(EA_1_W_gam0.7)$Gamma <- 0.7
class(EA_1_W_gam0.7) <- 'MP' 


# wGamma = 0.9
EA_1_W_gam0.9 <- EA_1_W 
formals(EA_1_W_gam0.9)$Gamma <- 0.9
class(EA_1_W_gam0.9) <- 'MP' 


# End automatically generated q grid for EA MPs 
# Full grid of pairs of eGamma and wGamma 
gridMPs <- list(c('EA_1_E_gam0.1', 'EA_1_W_gam0.1'),
                c('EA_1_E_gam0.3', 'EA_1_W_gam0.1'),
                c('EA_1_E_gam0.5', 'EA_1_W_gam0.1'),
                c('EA_1_E_gam0.7', 'EA_1_W_gam0.1'),
                c('EA_1_E_gam0.9', 'EA_1_W_gam0.1'),
                c('EA_1_E_gam0.1', 'EA_1_W_gam0.3'),
                c('EA_1_E_gam0.3', 'EA_1_W_gam0.3'),
                c('EA_1_E_gam0.5', 'EA_1_W_gam0.3'),
                c('EA_1_E_gam0.7', 'EA_1_W_gam0.3'),
                c('EA_1_E_gam0.9', 'EA_1_W_gam0.3'),
                c('EA_1_E_gam0.1', 'EA_1_W_gam0.5'),
                c('EA_1_E_gam0.3', 'EA_1_W_gam0.5'),
                c('EA_1_E_gam0.5', 'EA_1_W_gam0.5'),
                c('EA_1_E_gam0.7', 'EA_1_W_gam0.5'),
                c('EA_1_E_gam0.9', 'EA_1_W_gam0.5'),
                c('EA_1_E_gam0.1', 'EA_1_W_gam0.7'),
                c('EA_1_E_gam0.3', 'EA_1_W_gam0.7'),
                c('EA_1_E_gam0.5', 'EA_1_W_gam0.7'),
                c('EA_1_E_gam0.7', 'EA_1_W_gam0.7'),
                c('EA_1_E_gam0.9', 'EA_1_W_gam0.7'),
                c('EA_1_E_gam0.1', 'EA_1_W_gam0.9'),
                c('EA_1_E_gam0.3', 'EA_1_W_gam0.9'),
                c('EA_1_E_gam0.5', 'EA_1_W_gam0.9'),
                c('EA_1_E_gam0.7', 'EA_1_W_gam0.9'),
                c('EA_1_E_gam0.9', 'EA_1_W_gam0.9'))
# END auto grid MP list

