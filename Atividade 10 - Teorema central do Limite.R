'Rotina para gerar valores de x à partir da F.D.P dada'
fdp <- function(N){
      xmax <- 1
      xmin <- -1
      ymax <- 3/2
      contador <- 0
      x <- c()
      
      while(contador < N){
         xc <- xmin + (xmax - xmin)*runif(1)
         yv <- ymax*runif(1)
         if ( yv < ymax*(xc**2)){
            contador <- contador + 1
            x <- c(x, xc)
         }
      }
      return(x)
}
'a'
# vamos agora calcular quantos elementos estão no intervalo

N <- 10000
x <- fdp(N)

xmed <- mean(x)
sdx <- sd(x)
# função para calcular os intervalos
intervalo <- function(x, N, L, valor_medio, sd){
      # x = vetor, N = elementos, L = numero de sigmas
      contador <- 0
      for (i in 1:N) {
            if (x[i] > valor_medio - L*sd & x[i] < valor_medio + L*sd){
                  contador <- contador + 1
            }
      }
      return(contador)
}
l1 <- intervalo(x, N, 1, xmed, sdx)
l1
l2 <- intervalo(x, N, 1.5, xmed, sdx)
l2
l3 <- intervalo(x, N, 2, xmed, sdx)
l3
l4 <- intervalo(x, N, 2.5, xmed, sdx)
l4
l5 <- intervalo(x, N, 3, xmed, sdx)
l5

'b'
x1 <- fdp(N)
x2 <- fdp(N)

y <- x1+x2
ymed <- mean(y)
sdy <- sd(y)

l1 <- intervalo(y, N, 1, ymed, sdy)
l1
l2 <- intervalo(y, N, 1.5, ymed, sdy)
l2
l3 <- intervalo(y, N, 2, ymed, sdy)
l3
l4 <- intervalo(y, N, 2.5, ymed, sdy)
l4
l5 <- intervalo(y, N, 3, ymed, sdy)
l5

'rotina para somar vetores aleatórios'

soma_xs <- function(N, M){
   # N = numero de elementos por x
   # M = número de conjuntos na soma
   xmax <- 1
   xmin <- -1
   ymax <- 3/2
   conjunto <- matrix(0, N, M) # matriz que conterá todos os valores a serem guardados
   vsoma <-c() # vetor que vai somar os conjuntos
   # cada coluna representa um conjunto x_i
   
   for (i in 1:M) {
      x <- c()
      contador <- 0
      while(contador < N){
         xc <- xmin + (xmax-xmin)*runif(1)
         yv <- ymax*runif(1)
         if ( yv < ymax*(xc**2)){
            conjunto[contador, i] <- xc
            contador <- contador + 1
            
         }
      }
      if (i == 1){
         vsoma <- conjunto[,i]
      }
      else {
         vsoma <- vsoma +conjunto[,i]
      }
   }
   return(vsoma)
   
}

'M=3'
m3 <- soma_xs(10000,3)
m3med <- mean(m3)
sd3 <- sd(m3)

l1_3 <- intervalo(m3, N, 1, m3med, sd3)
l1_3
l2_3 <- intervalo(m3, N, 1.5, m3med, sd3)
l2_3
l3_3 <- intervalo(m3, N, 2, m3med, sd3)
l3_3
l4_3 <- intervalo(m3, N, 2.5, m3med, sd3)
l4_3
l5_3 <- intervalo(m3, N, 3, m3med, sd3)
l5_3


'M=5'
m5 <- soma_xs(10000,5)
m5med <- mean(m5)
sd5 <- sd(m5)

l1_5 <- intervalo(m5, N, 1, m5med, sd5)
l1_5
l2_5 <- intervalo(m5, N, 1.5, m5med, sd5)
l2_5
l3_5 <- intervalo(m5, N, 2, m5med, sd5)
l3_5
l4_5 <- intervalo(m5, N, 2.5, m5med, sd5)
l4_5
l5_5 <- intervalo(m5, N, 3, m5med, sd5)
l5_5


'M=10'
m10 <- soma_xs(10000,10)
m10med <- mean(m10)
sd10 <- sd(m10)

l1_10 <- intervalo(m10, N, 1, m10med, sd10)
l1_10
l2_10 <- intervalo(m10, N, 1.5, m10med, sd10)
l2_10
l3_10 <- intervalo(m10, N, 2, m10med, sd10)
l3_10
l4_10 <- intervalo(m10, N, 2.5, m10med, sd10)
l4_10
l5_10 <- intervalo(m10, N, 3, m10med, sd10)
l5_10


'M=100'
m100 <- soma_xs(10000,100)
m100med <- mean(m100)
sd100 <- sd(m100)

l1_100 <- intervalo(m100, N, 1, m100med, sd100)
l1_100
l2_100 <- intervalo(m100, N, 1.5, m100med, sd100)
l2_100
l3_100 <- intervalo(m100, N, 2, m100med, sd100)
l3_100
l4_100 <- intervalo(m100, N, 2.5, m100med, sd100)
l4_100
l5_100 <- intervalo(m100, N, 3, m100med, sd100)
l5_100

'Valores gaussianos'
gauss <- rnorm(N)
l1_N <-intervalo(gauss, N, 1, 0, 1)
l1_N
l2_N <-intervalo(gauss, N, 1.5, 0, 1)
l2_N
l3_N <-intervalo(gauss, N, 2, 0, 1)
l3_N
l4_N <-intervalo(gauss, N, 2.5, 0, 1)
l4_N
l5_N <-intervalo(gauss, N, 3, 0, 1)
l5_N