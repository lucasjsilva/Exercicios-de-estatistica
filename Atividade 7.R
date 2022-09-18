'Exercício 1'
# x >= -1 e x <= 1
# rotina para gerar dados que sigam a F.D.P.:
N <- 10000
x_min <- -1
x_max <- 1
Y <- 3/4
x <- c()
contador <- 0

while(contador<N){
   xc <- x_min + (x_max - x_min)*runif(1)
   yv <- Y*runif(1)
   if (yv <= (3/4)*(1-xc**2)){
      x <- c(x, xc)
      contador <- contador + 1
   }
}

# a) média dos N valores de x
media_x <- mean(x)
#media_fdp <- mean(x)
media_x
#media_fdp

# b) desvio-padrão amostral dos N valores de x
sd_x <- sd(x)
sd_x
sd_med <- sd_x/sqrt(N)
# C) n1 dentro de 1 sigma? Frequência relativa?
n1s <- 0
for (i in 1:N) {
      if (x[i] >= media_x - sd_x & x[i] <= media_x + sd_x ){
            n1s <- n1s + 1
      }
}

f_rel <- n1s/N

n1s
f_rel

'Exercício 2'
N <- 10000
y_min <- -pi/2
y_max <- pi/2
f_y_max <- 1/2


y <- c()
count <- 0
while(count < N){
   yc <- y_min + (y_max - y_min)*runif(1) #Candidatos
   fc <- f_y_max*runif(1) #verificadores
   if(fc <= cos(yc)/2){
      y <- c(y, yc)
      count <- count + 1
   }
   
   
}

# a) a média dos N valores de y
y_med <- mean(y)
y_med

# b) o desvio padrão dos N valores de y
sd_y <- sd(y)
sd_y
sd_m <- sd_y/sqrt(N)

# c) n_1 e frequência relativa
n_1 <- 0
for (j in 1:N) {
      if (y[j] >= y_med - sd_y & y[j] <= y_med + sd_y){
            n_1 <- n_1 + 1
      }
}
frel_y <- n_1/N
frel_y