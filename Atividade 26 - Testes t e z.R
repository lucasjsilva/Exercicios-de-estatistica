# Gerar N conjuntos de dados guassianos
x0 <- 50
s0 <- 10

# Rotina para gerar o conjunto de N dados

N <- 3
x <- rnorm(N, x0, s0)
xm <- mean(x)
sig_til <- sum((x - xm)**2)/(N - 1)
sigm_til <- sig_til/sqrt(N)

# b) gerar 10000 repetições para teste t
M <- 10000 # repetições
t2 <- 4.53
guarda_t <- c()
conta_t2 <- 0
for (j in 1:M) {
      x <- rnorm(N, x0, s0)
      xm <- mean(x)
      sig_til <- sqrt(sum((x - xm)**2)/(N - 1))
      sigm_til <- sig_til/sqrt(N)
      t <- (xm - x0)/sigm_til
      # vejo se esse t tem módulo menor ou igual a t2
      if (abs(t) <= t2){
            conta_t2 <- conta_t2 + 1
      }
}

frel_t <- conta_t2/M
inc_t <- sqrt(conta_t2*(1-(frel_t)))

# c) gerar 10000 repetições para teste z

t2_z <- 2
conta_z2 <- 0
for (j in 1:M) {
      x <- rnorm(N, x0, s0)
      xm <- mean(x)
      z <- (xm - x0)/(s0/sqrt(N))
      # vejo se esse t tem módulo menor ou igual a t2
      if (abs(z) <= t2_z){
            conta_z2 <- conta_z2 + 1
      }
}

frel_z <- conta_z2/M
inc_z <- sqrt(conta_z2*(1-(frel_z)))

# d) repetição de todo o processo para N = 101
t2 <- 2.03
N = 101
conta_z_101 <- 0
conta_t_101 <- 0 
for(i in 1:M){
      x <- rnorm(N, x0, s0)
      xm <- mean(x)
      s_til2 <- sqrt(sum((x-xm)**2)/(N - 1))
      sm_til2 <- s_til2/sqrt(N)
      # teste t e teste z, respectivamente:
      t_linha <- (xm - x0)/sm_til2
      z_linha <- (xm - x0)/(s0/sqrt(N))
      # avalio se t_linha e z_linha são menores que os valores tabelados
      if (abs(t_linha) <= t2){
            conta_t_101 <- conta_t_101 + 1
      }
      if (abs(z_linha) <= t2_z){
            conta_z_101 <- conta_z_101 + 1
      }
}

# determino a frequência relativa e sua incerteza
ft_2 <- conta_t_101/M
inc_t101 <- sqrt(conta_t_101*(1-ft_2))
fz_2 <- conta_z_101/M
inc_z101 <- sqrt(conta_z_101*(1-fz_2))