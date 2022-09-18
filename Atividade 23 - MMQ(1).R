MMQ <- function(y, t, si, G){
      # vou iniciar os vetores que serão usados ao longo do código
      N <- length(y)
      
      P <- dim(G)[2] # quantidade de coeficientes no polinômio F(x,a)
      # O sistema de equções do MMQ pode ser escrito na forma matricial D = M %*% A
      # em que D é um vetor, M uma matriz e A um vetor 
      # vou então iniciar uma matriz e um vetor vazios para M e D
      M <- matrix(0, nrow = P, ncol = P) # tenho 2 gs no polinômio F(x,a)
      D <- c()
      
      # realizo agora o somatório através dos loops
      for (L in 1:P) {
            # vou atualizando o D
            D[L] <- sum(y*G[,L]/(sh^2))
            for (C in 1:P) {
                  M[L, C] <- sum(G[,L]*G[,C]/(sh^2))
            }
      }
      
      # para determinar a matriz de covariância, basta calcular a matriz inversa de M
      covA <- solve(M)
      
      # a matriz de correlação também pode ser determinada
      s_a1 <- sqrt(covA[1,1])
      s_a2 <- sqrt(covA[2,2])
      corA <- covA
      corA[1,1] <- corA[1,1]/(s_a1^2)
      corA[1,2] <- corA[1,2]/(s_a1*s_a2)
      corA[2,1] <- corA[2,1]/(s_a1*s_a2)
      corA[2,2] <- corA[2,2]/(s_a2^2)
      
      # para determinar o vetor A é necessário multiplicar o inverso da matriz M com 
      # o vetor D
      A <- covA %*% D
      
      # Os valores de F(x,a) podem ser deteminados pelo produto de G e A
      F <- G %*% A
      
      # com isso o Chi2 pode ser determinado como
      Chi2 <- sum(((y-F)/(sh))^2)
      return(list(coeficientes=A,
                  incerteza1 = s_a1, incerteza2= s_a2, 
                  covariancia = covA, correlação = corA,
                  Chi2 = Chi2))
}


# vou iniciar os vetores que serão usados ao longo do código
t <- c(1, 2, 3, 4, 5) # s
h <- c(1.7, 3.0, 4.2, 4.8, 5.4) # cm
sh <- 0.2 #c(0.2, 0.2, 0.2, 0.2, 0.2) # cm
N <- length(h)
g1 <- c(1, 1, 1, 1, 1)
G <- matrix(c(g1, t), nrow = N, ncol = 2)

ajuste1 <- MMQ(h, t, sh,G)
a <- ajuste1$coeficientes[1]
b <- ajuste1$coeficientes[2]
funcao1 <- a + b*t

t_linha <- t-3
G_linha2 <- matrix(c(g1, t_linha), nrow = N, ncol = 2)
ajuste2 <- MMQ(h, t_linha, si, G_linha)
alpha <- ajuste2$coeficientes[1]
beta <- ajuste2$coeficientes[2]
funcao2 <- alpha + beta*t_linha


plot(t, funcao1, col = "black", type = "l",
     main = "MMQ - plot das duas funções", xlab = "t (s)", ylab = "h (cm)")
par(new = TRUE)
plot(t_linha, funcao2, col = "red", type = "l", 
     main = "MMQ - plot das duas funções", xlab = "t (s)", ylab = "h (cm)")

