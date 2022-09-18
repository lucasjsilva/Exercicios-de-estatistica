meuMMQ <- function(y, si, G){
      # G = [g1, g2, ..., gP] <- matriz que vai agrupar os coeficientes
      N <- length(y)
      P <- dim(G)[2]
      D <- matrix(data = 0, nrow = P, ncol = 1)
      M <- matrix(data = 0, nrow = P, ncol = P)
      
      for (L in 1:P) {
            D[L] <- sum(y*G[,L]/(si^2))
            for (C in 1:P) {
                  M[L,C] <- sum(G[,L]*G[,C]/(si^2))
            }
      }
      
      VA <- solve(M)# matriz inversa de M
      A <- VA %*% D
      F <- G %*% A
      Chi2 <- sum(((y -F)/(si))^2)
            
      return(list(A = A, VA = VA, F = F, Chi2 = Chi2))
}

'Exemplo da aula'
y <- c(42.4, 28.0, 34.3, 36.5, 29.5)
G <- matrix(c(c(120.3, 195.1, 10.2, 320.9, 110.6),
       c(451.6, 115.3, 523.5, 54.2, 277.4)), nrow = 5, ncol = 2)
si <- c(0.5)
matriz_G <- matrix(G, ncol = 2)



t <- c(1, 2, 3, 4, 5) # s
X <- t-3
y <- c(1.7, 3.0, 4.2, 4.8, 5.4) # cm
sh <- 0.2 #c(0.2, 0.2, 0.2, 0.2, 0.2) # cm
N <- length(h)
g1 <- c(1, 1, 1, 1, 1)
G_linha <- matrix(c(g1, X), nrow = N, ncol = 2)
teste2 <- meuMMQ(y, sh, G_linha)