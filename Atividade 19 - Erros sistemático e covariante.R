'Exercício 1'
# Valores verdadeiros
x0 <- 110
y0 <- 100
sc <- 4 # incerteza calibração
sL <- 3 # incerteza leitura
N <- 1000 # conjunto de pares

# variáveis aleatórias
rc <- rnorm(N)
rx <- rnorm(N)
ry <- rnorm(N)

# valores
Ec <- sc*rc
x <- x0 +sL*rx + Ec
y <- y0 +sL*ry + Ec

# a) freq relativa e incerteza de quando x e y tem mesmo sinal
n <- 0 # contador da igualdade do sinal
for (i in 1:N) {
   erro_x <- x[i] - x0
   erro_y <- y[i] - y0
   
   if (erro_x > 0 & erro_y >0){
      n <- n + 1
   }
   if (erro_x < 0 & erro_y < 0){
      n <- n + 1
   }
}
frel <- n/N
sfref <- sqrt(n*(1-(n/N)))
frel
sfref

# b) covariancia amostral e correlação amostral
xm <- mean(x)
sdx <- sd(x)
ym <- mean(y)
sdy <- sd(y)
somatorio = 0
for (i in 1:N) {
      somatorio <- somatorio + ((x[i]-xm)*(y[i]-ym))
}
Vxy <- somatorio/(N-1)
R <- Vxy/(sdx*sdy)
inc_Vxy <- sdx*sdy*sqrt((1+R**2)/(N-1))
inc_R <- (1-R**2)/sqrt(N-1)
Vxy
inc_Vxy
R
inc_R

# c) dw  e sdw
w <- x + y
dw <- sd(w)
sw <- dw/sqrt(2*(N-1))
dw
sw

# d) dz e sdz
z <- x - y
dz <- sd(z)
sdz <- dz/sqrt(2*(N-1))
dz
sdz

# e) propagação de incertezas
covar <- sc^2
prop <- sqrt((sdx)^2 + (sdy)^2 + 2*covar)
prop2 <- sqrt((sdx)^2 + (sdy)^2 - 2*covar)
prop
prop2

'Exercício 2'

d0 <- 200
ss <- 4
sa <- 3
M <- 10000 # conjunto
N <- 25 # dados
guarda_M <- c()


for (i in 1:M){
   E_sis <- rnorm(1, mean = 0, sd = ss)
   E_al <- rnorm(N, mean = 0, sd =sa)
   d <- d0 + E_sis + E_al
   dmed <- mean(d)
   guarda_M <- c(guarda_M, dmed)
   
}

M_med <- mean(guarda_M)
sd_M <- sd(guarda_M)
inc_sdM <- sd_M/sqrt(2*(M-1))

M_med
sd_M
inc_sdM