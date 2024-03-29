'exerc�cio 1 - valor de pi � partir de uma circunfer�ncia'
picirc <- function(N){
      xmax <- 1
      ymax <- 1
      n <- 0 # vari�vel que contar� quais n�meros est�o dentro da circunfer�ncia
      # vou gerar vari�veis aleat�rias para ver se pertencem ao intervalo desejado
      x <- xmax*runif(N)
      y <- ymax*runif(N)
      for (i in 1:N){
            if (x[i]**2 + y[i]**2 <= 1){
                  n <- n + 1
            }
      }
      # a) valor de n
      print("O valor de n �:")
      print(n)
      
      # b) incerteza no valor de n
      # baseado na f�rmula dada pelo exerc�cio:
      sigma_n <- sqrt(n*(1-(n/N)))
      print("A incerteza de n �:")
      print(sigma_n)
      
      # c) valor de pi e incerteza
      pi <- (4*n)/N
      sigma_pi <- (4*sigma_n)/N
      print("O valor estimado de pi e sua incerteza s�o:")
      print(pi)
      print(sigma_pi)
      
      return(pi)
}

'Exerc�cio 2 - valor de pi � partir de uma esfera'
piesfera <- function(N){
      xmax <- 1
      ymax <- 1
      zmax <- 1
      n <- 0 # variav�l que vai contar quantos dados est�o dentro da esfera
      
      # vou agora gerar as vari�veis aleat�rias e ver quantas cumprem o desejado
      x <- xmax*runif(N)
      y <- ymax*runif(N)
      z <- zmax*runif(N)
      for (i in 1:N) {
            if (x[i]**2 + y[i]**2 + z[i]**2 <= 1){
                  n <- n + 1
            }
      }
      # a) valor de n
      print("O valor de n �:")
      print(n)
      
      # b) incerteza de n
      sigma_n <- sqrt(n*(1-(n/N)))
      print("A incerteza de n �:")
      print(sigma_n)
      
      # c) valor de pi e sua incerteza
      pi <- (6*n)/N
      sigma_pi <- (6*sigma_n)/N
      print("O valor de pi e sua incerteza s�o:")
      print(pi)
      print(sigma_pi)
      
      return(pi)
}

'Ex 3 - Desvios padr�es verdadiros'
desv_verd <- function(N){
      p_circ <- pi/4 # baseado no enunciado
      p_esfera <- pi/6 # baseado no enunciado
      desv_circ <- sqrt(N*p_circ*(1 - p_circ))
      desv_esf <- sqrt(N*p_esfera*(1 - p_esfera))
      
      print(desv_circ)
      print(desv_esf)
      return(desv_esf)
}

'Ex 4'
# valor de N1
N1 = 16*((pi/4)-(pi/4)**2)/(0.003)*2
N1

n2 = 36*((pi/6)-(pi/6)**2)/(0.001)*2
n2