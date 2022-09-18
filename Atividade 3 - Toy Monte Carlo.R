'Exercício 1 '

r0 <- 15
sr <- 1
N <- 10000

r <- r0 +sr*rnorm(N)
x <- pi*(r^2)

sdx <- sd(x)
valor_med <- mean(x)
sdx_med <- sdx/sqrt(N)
sdx
valor_med
sdx_med

###########################

'Exercício 2'

d0 <- 5
sigma_d0 <- 1
N <- 10000

d <- d0 + sigma_d0*rnorm(N)
y <- d**3

sdy <- sd(y)
y_med <- mean(y)
sdy_med <- sdy/sqrt(N)

sdy
y_med
sdy_med

###########################

'Exercício 3'
a0 <- 10
sigma_a0 <- 2
b0 <- 20
sigma_b0 <- 2
N <- 10000

a <- a0 + sigma_a0*rnorm(N)
b <- b0 + sigma_b0*rnorm(N)
z <- a/b
sdz <- sd(z)
valor_medz <- mean(z)
sdz_med <- sdz/sqrt(N)
sdz
valor_medz
sdz_med

#usando b fixo

z_bfixo <- a/b0
sdz_bfixo <- sd(z_bfixo)
val_med_bfixo <- mean(z_bfixo)
sdz_bfixo_med <- sdz_bfixo/sqrt(N)
sdz_bfixo
val_med_bfixo
sdz_bfixo_med

#usando a fixo

z_afixo <- a0/b
sdz_afixo <- sd(z_afixo)
val_med_afixo <- mean(z_afixo)
sdz_afixo_med <- sdz_afixo/sqrt(N)
sdz_afixo
val_med_afixo
sdz_afixo_med