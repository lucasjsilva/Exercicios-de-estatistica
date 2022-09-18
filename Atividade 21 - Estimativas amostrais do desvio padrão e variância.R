
x0 <- 0
sig0 <- 1
M <- 10000 # conjuntos
N <- 100 # dados por conjunto

desvios <- c() # vai guardar os desvios padrão
variancias <- c() # vai guardar as variâncias

for (i in 1:M) {
      x <- rnorm(N)
      xm <- mean(x)
      somatorio <- 0
      # vou calcular o desvio padrão
      for (j in 1:N) {
         diferenca <- (x[j] - xm)^2
         somatorio <- somatorio + diferenca
         
      }
      
      sdx <- sqrt(somatorio/(N-1))
      var <- sdx^2
      # guardo as variáveis em um vetor
      desvios <- c(desvios, sdx)
      variancias <- c(variancias, var)
}

# a) histograma de N = 100, N = 5, N = 2

hist(desvios, main = "Histograma dos 
     valores de s")
hist(variancias, main = "Histograma
     dos valores de V")


# b.1 & b.2
sd_med <- mean(desvios)
sd_s <- sd(desvios)
inc_sd <- sd_s/sqrt((M))
sd_med
inc_sd
var_med <- mean(variancias)
sd_v <- sd(variancias)
inc_var <- sd_v/sqrt(M)
var_med
inc_var

# b.3
cont_s <- 0
cont_v <- 0
for (i in 1:M) {
      if (desvios[i] <= sig0){
           cont_s <- cont_s + 1 
      }
      if (variancias[i] <= sig0^2){
            cont_v <- cont_v + 1
      }
}
inc_conts <- sqrt(cont_s*(1-(cont_s/M)))
cont_s
inc_conts
inc_contv <- sqrt(cont_v*(1-(cont_v/M)))
cont_v
inc_contv