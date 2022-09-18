'Exercício 1'
x0 <- 1.48353 #rad
sx <- 0.174533 #rad
N <- 10000
x_1 <- x0 + sx*rnorm(N)
y1 <- c()
for (i in 1:10000) {
      y <- sin(x_1[i])
      y1 <- c(y1, y)
}
sd <- sd(y1)
y_medio <- mean(y1)
sd_med <- sd/N

sd
y_medio
sd_med

'Exercício 2'
valor_x <- function(sa, ss, x0, n){
      erro_sistematico <- ss*rnorm(1)
      x <- x0 + erro_sistematico + sa*rnorm(n)
      xf <- mean(x)
      return(xf)
}

rep <- 1:10000
valores_finais <- c()
for (i in 1:10000) {
      xf <- valor_x(28, 3, 100, 50)
      valores_finais <- c(valores_finais, xf)
}
# a) desvio padrão amostral dos x_f
sigmaf <- sd(valores_finais)
print(sigmaf)

# b) quantos x_f são maiores que x0
maiores <- 0
for (i in 1:10000) {
      if (valores_finais[i] > 100){
            maiores <- maiores + 1
      }
}
print(maiores)

# c) quantos o modulo da diferença entre x0 e xf é menor que sigma f
menores <- 0
for (i in 1:10000) {
      mod <- valores_finais[i] - 100
      if (mod < 0){
            mod <- - mod
      }
      if (mod < sigmaf){
            menores <- menores + 1
      }
}
print(menores)

'Exercício 3'
t0 <- 2.525 #s
n <- 287
sa <- 0.15
valor_ <- function(sa, t0, n){
      #erro_sistematico <- ss*rnorm(1)
      t <- t0 + sa*rnorm(n)
      tf <- mean(t)
      return(tf)
}
valores_med <- c()
for (i in 1:10000) {
      tf <- valor_(sa, t0, n)
      valores_med <- c(valores_med, tf)
}

# a) desvio padrão da aceleração
dH <- 34
a <- (2*dH)/valores_med**2
sigma_a <- sd(a)

# b) a maior que a_0 /// módulo de a-a_0 < sigma_a
maior_a <- 0
menor_sig <- 0
a_0 <- (2*dH)/t0**2
for (j in 1:10000){
      if (a[j] > 10.6656210175473){
            maior_a <- maior_a + 1
      }
}
for (i in 1:10000){
      modulo <- a[i] - a_0 
      if (modulo < 0){
            modulo <- -modulo
      }
      if (modulo < sigma_a){
            menor_sig <- menor_sig + 1
      }
}
sigma_a
maior_a
menor_sig

'Exercício 4'
t0 <- 2.525 #s
n <- 287
sa <- 0.15
a_0 <- (2*dH)/t0**2
a_linha <- function(t0, n, sa){
      t <- t0 + sa*rnorm(n)
      a <- (2*34)/t**2
      af <- mean(a)
}
repeticoes <- c()
for (i in 1:10000) {
      af_linha <- a_linha(t0, n, sa)
      repeticoes <- c(repeticoes, af_linha)
}
sigma_linha <- sd(repeticoes)

greater <- 0
lesser <- 0

for (i in 1:10000) {
      if (repeticoes[i] > a_0){
            greater <- greater + 1
      }
}

for (i in 1:10000) {
      module <- repeticoes[i] - a_0
      if (module < 0){
            module <- -module
      }
      if (module < sigma_linha ){
            lesser <- lesser + 1
      }
}
 
sigma_linha
greater
lesser