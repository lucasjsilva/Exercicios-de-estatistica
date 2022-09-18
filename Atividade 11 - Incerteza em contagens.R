'F.D.P:'
ocorrencias <- function(N){
      xmin <- 0
      xmax <- 5
      ymax <- 25*(3/125)
      x <- c() # vai armazenar os dados que pertencem à F.D.P.
      #func <- c()
      cont <- 0
      
      while(cont < N){
            xc <- xmin + (xmax-xmin)*runif(1)
            yv <- ymax*runif(1)
            if (yv < (3/125)*(xc**2)){
                  x <- c(x, xc)
                  #func <- c(func, (3/125)*(xc**2))
                  cont <- cont + 1
            }
      }
      # depois que eu obtenho todos os x, tenho que ver quantos vão pertencer aos intervalos
      I1 <- 0 # intervalo 1
      I2 <- 0 # intervalo 2
      I3 <- 0 # intervalo 3
      I4 <- 0 # intervalo 4
      I5 <- 0 # intervalo 5
      for (i in 1:N) {
            if (x[i] >=0 & x[i] <= 1){
                  I1 <- I1 + 1
            }
         if (x[i] >= 1 & x[i] <= 2){
            I2 <- I2 + 1
         }
         if (x[i] > 2 & x[i] < 3){
            I3 <- I3 + 1
         }
         if (x[i] > 3 & x[i] < 4){
            I4 <- I4 + 1
         }
         if (x[i] > 4 & x[i] < 5){
            I5 <- I5 + 1
         }
      }
      print(I1)
      print(I2)
      print(I3)
      print(I4)
      print(I5)
      #hist(x)
      #plot(x, func)
   return(hist(x))
      
}

'caso geral'
com_repeticoes <- function(nREP, N){
   v1 <- c() # vai armazenar as repetições do intervalo 1
   v2 <- c() # vai armazenar as repetições do intervalo 2
   v3 <- c() # vai armazenar as repetições do intervalo 3
   v4 <- c() # vai armazenar as repetições do intervalo 4
   v5 <- c() # vai armazenar as repetições do intervalo 5
   xmin <- 0
   xmax <- 5
   ymax <- 25*(3/125)
   for (i in 1:nREP) { # cada iteração vai armazenar um dado até termos nREP
      x <- c()
      cont <- 0
      while(cont < N){
         xc <- xmin + (xmax-xmin)*runif(1)
         yv <- ymax*runif(1)
         if (yv < (3/125)*xc**2){
            x <- c(x, xc)
            cont <- cont + 1
         }
         
      }
      I1 <- 0 # intervalo 1
      I2 <- 0 # intervalo 2
      I3 <- 0 # intervalo 3
      I4 <- 0 # intervalo 4
      I5 <- 0 # intervalo 5
      for (i in 1:N) {
         if (x[i] >0 & x[i] < 1){
            I1 <- I1 + 1
         }
      }
      for (i in 1:N){
         if (x[i] > 1 & x[i] < 2){
            I2 <- I2 + 1
         }
      }
      for (i in 1:N){
         if (x[i] > 2 & x[i] < 3){
            I3 <- I3 + 1
         }
      }
      for (i in 1:N) {
         if (x[i] > 3 & x[i] < 4){
            I4 <- I4 + 1
         }
      }
      for (i in 1:N) {
         if (x[i] > 4 & x[i] < 5){
            I5 <- I5 + 1
         }
      }
      v1 <- c(v1, I1)
      v2 <- c(v2, I2)
      v3 <- c(v3, I3)
      v4 <- c(v4, I4)
      v5 <- c(v5, I5)
   }
   V <- c(v1, v2, v3, v4, v5)
   print("med")
   print(mean(v1))
   print("sigma med")
   print(sd(v1)/sqrt(length(v1)))
   print("sigma")
   print(sd(v1))
   print("med")
   print(mean(v2))
   print("sigma med")
   print(sd(v2)/sqrt(length(v2)))
   print("sigma")
   print(sd(v2))   
   print("med")
   print(mean(v3))
   print("sigma med")
   print(sd(v3)/sqrt(length(v3)))
   print("sigma")
   print(sd(v3))   
   print("med")
   print(mean(v4))
   print("sigma med")
   print(sd(v4)/sqrt(length(v4)))
   print("sigma")
   print(sd(v4))   
   print("med")
   print(mean(v5))
   print("sigma med")
   print(sd(v5)/sqrt(length(v5)))
   print("sigma")
   print(sd(v5))
   return(hist(V))
}