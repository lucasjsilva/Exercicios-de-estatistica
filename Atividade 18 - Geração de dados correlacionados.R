'a)'
a0 <- 30
b0 <- 20
sa <- 2
sb <- 2

r1 <- rnorm(500)
r2 <- rnorm(500)

det_cov <- function(rho){
      
      # a) - determina a e b
      a <- a0 + sa*r1
      b <- b0 + sb*(rho*r1 + sqrt(1-rho^2)*r2)
      
      # 1 - determina o número de vezes que a tem o mesmo sinal que b
      n_erro <- 0 # numero de vezes que o erro de a tem o mesmo sinal que o erro de b
      for (i in 1:500) {
            erro_a = a[i] - a0
            erro_b = b[i] - b0
            
            if (erro_a > 0 & erro_b > 0){
                  n_erro <- n_erro + 1
            }
            
            if (erro_a < 0 & erro_b < 0){
                  n_erro <- n_erro + 1
            }
      }
      
      # 1 - usando a incerteza binomial para determinar a incerteza de n_erro
      s_nerro <- sqrt(n_erro*(1-(n_erro/500)))
      #print(n_erro)
      #print(s_nerro)
      
      # 2 - frequência relativa e incerteza
      frel <- n_erro/500
      sfrel <- s_nerro/500
      #print(frel)
      #print(sfrel)
      
      # 3 - Covariancia amostral e correlação amostral
      am <- mean(a)
      bm <- mean(b)
      sd_a <- sd(a)
      sd_b <- sd(b)
      
      
      somatorio <- 0
      for (i in 1:500) {
            somatorio <- somatorio + (a[i]-am)*(b[i]-bm)
      }
      Vab <- somatorio/(500-1)
      R <- Vab/(sd_a*sd_b)
      
      incV <- (sd_a*sd_b)*sqrt((1+(R**2))/(500-1))
      incR <- (1-R^2)/sqrt(500-1)
      
      # 4/5 - incertezas
      w <- a +b
      sdw <- sd(w)
      incsdw <- sdw/sqrt(2*(500-1))
      z <- a - b
      sdz <- sd(z)
      incsdz <- sdz/sqrt(2*(500-1))
      
      # 6 - propagação de incerteza
      prop_w <- sqrt(sa^2 + sb^2)
      prop_z <- sqrt(sa^2 + (-sb)^2)
      prop_w
      prop_z
      
      return(list(a = a, b = b, n_erro = n_erro, s_nerro = s_nerro,
                  frel = frel, sfrel = sfrel, Vab = Vab, incV = incV,
                  R = R, incR = incR, sdw = sdw, incsdw = incsdw, sdz = sdz,
                  incsdz = incsdz, prop_w = prop_w, prop_z = prop_z))
}
