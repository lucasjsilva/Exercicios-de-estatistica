'Exerc�cio 1'
# vamos primeiro gerar a rotina para gerar N dados pelo m�todo da exclus�o

funcao1 <- function(G, N){
      xmax <- 1
      xmin <- -1
      ymax <- G+1/2*G # na fun��o do exerc�cio colocar x=0
      contador <- 0 # vai verificar se o n�mero de elementos � igual a N
      x <- c() # vetor vazio para armazenar os valores que pertencem � F.D.P
      
      while (contador < N) {
            xc <- xmin + (xmax-xmin)*runif(1) # candidato
            yv <- ymax*runif(1) # verificador
            if (yv < ymax*(1-(Mod(xc))**G)){
                  x <- c(x, xc)
                  contador <- contador + 1
            }
            
      }
      return(x)
}

# b) desvio padr�o amostral de x
G <- 3
N <- 10000
x <- funcao1(G, N)
sdx <- sd(x)
sdx

# c) frequ�ncia de ocorr�ncia
freq1 <- 0
xmed <- mean(x)
for (i in 1:N) {
      if (x[i] > xmed - sdx & x[i] < xmed + sdx){
            freq1 <- freq1 + 1
      }
}
F1 <- freq1/N
F1

'Exerc�cio 2'
funcao2 <- function(L, N){
      xmax <- 1000 # n�mero grande para representar +infinito
      xmin <- -1000 # n�mero grande para representar -infinito
      ymax <- 1/2*L
      contador <- 0 # respons�vel por fazer o n�mero de elementos chegar a N
      y <- c()
      
      while (contador < N) {
            yc <- xmin + (xmax - xmin)*runif(1) # candidato
            fv <- ymax*runif(1) # verificador
            if (fv < ymax*exp(-Mod(yc)/L)){
                  contador <- contador + 1
                  y <- c(y, yc)
            }
      }
      return(y)
}

# b) desvio padr�o amostral de y
L <- 2
N <- 10000
y <- funcao2(L, N)
sdy <- sd(y)
sdy

# c) frequ�ncia de ocorr�ncia
ymed <- mean(y)
freq2 <- 0
for (i in 1:N) {
      if (y[i] > ymed - sdy & y[i] < ymed + sdy){
            freq2 <- freq2 + 1
      }
}
F2 <- freq2/N
F2