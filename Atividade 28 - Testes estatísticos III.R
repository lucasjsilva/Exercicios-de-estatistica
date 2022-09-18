'Ex. 1)'
N <- 4 # medições 
T0 <- 2.4 # segundos
Tm <- 2.5056666 # segundos
sdm <- 0.0400816 # segundos
sd <- 0.0801632 # segundos
Tc <- 3.31

# a) resultado final para o período de oscilação deste sistema
Tm
sdm

# b) intervalo de confiança de 95,45%
teste_t <- (Tm - T0)/sdm
lim_sup <- Tm + Tc*sdm
lim_inf <- Tm - Tc*sdm

# d) 
lim_d_sup <- T0 + Tc*sdm
lim_d_inf <- T0 - Tc*sdm

'Ex. 2)'

# a)

# b)intervalos
int_inf <- Tm - 2*sdm
int_sup <- Tm + 2*sdm

# d)
inf_d <- T0 - 2*sdm
sup_d <- T0 + 2*sdm