'Incerteza de uma variável por TMC'
 x0 <- 10
 sx <- 0.2
 N <- 1000
 
 x <- x0 + sx*rnorm(N)
 w <- 5*(x^2)
# colocar aqui a conta do desvio padrão
 sd <- sd(w)
 print("A incertezas é: ")
 sd
 
 ' Contribuição de cada grandeza para a incerteza final por TMC'
 
 a0 <- 10
 sa <- 0.2
 b0 <- 5
 sb <- 0.2
 n <- 1000
 a <- a0 + sa*rnorm(n)
 b <- b0 + sb*rnorm(n)
 za <- 2*(a^2)*b0
 zb <- 2*(a0^2)*b
 
 sd_za <- sd(za)
 sd_zb <- sd(zb)
 print("za:")
 print(sd_za)
 print("zb")
 print(sd_zb)
 print("sz")
 sz <- ((sd_za)^2 + (sd_zb)^2)^(1/2)
 sz