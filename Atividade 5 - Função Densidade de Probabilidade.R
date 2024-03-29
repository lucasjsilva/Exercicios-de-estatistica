'Exerc�cio 1 - Caracter�sticas da distribui��o gaussiana'
N <- 10000
x0 <- 0
sigma0 <- 1
# a) x_m e sigma_m
x <- x0 + sigma0*rnorm(N)
x_m <- mean(x) # x m�dio
sd_x <- sd(x) # incerteza do x
sigma_med <- sd_x/sqrt(N)

x_m
sigma_med

# b) desvio padr�o amostral de x
sd_x

# c) n�mero de dados entre x_m - sd_x e x_m + sd_x
intervalo1 <- 0
for (i in 1:10000) {
      if(x[i] < x_m + sd_x & x[i] > x_m - sd_x){
            intervalo1 <- intervalo1 + 1
      }
}
intervalo1

# d) n�mero de dados entre x_m - 2*sd_x e x_m + 2*sd_x
intervalo2 <- 0
for (i in 1:10000) {
      if(x[i] < x_m + 2*sd_x & x[i] > x_m - 2*sd_x){
            intervalo2 <- intervalo2 + 1
      }
}
intervalo2

# e) n�mero de dados entre x_m - 3*sd_x e x_m + 3*sd_x
intervalo3 <- 0
for (i in 1:N) {
      if(x[i] > x_m - 3*sd_x & x[i] < x_m + 3*sd_x){
            intervalo3 <- intervalo3 + 1
      }
}
intervalo3

'Exerc�cio 2 - Caracter�sticas da distribui��o uniforme'
N <- 10000
# distribui��o uniforme deve ser de -0.5 at� +0.5
y <- runif(N, min = -0.5, max = 0.5)

# a) y_m e sigma_y
y_m <- mean(y)
sdy <- sd(y)
sdy_m <- sdy/sqrt(N)
y_m
sdy_m

# b) desvio-padr�o amostral de y
sdy

# c) n�mero de dados entre y_m - sd_y e y_m + sd_y
int1 <- 0
for (i in 1:N) {
      if (y[i] > y_m - sdy & y[i] < y_m +sdy){
            int1 <- int1 + 1
      }
}
int1

# d) n�mero de dados entre y_m - 2*sd_y e y_m + 2*sd_y
int2 <- 0
for (i in 1:N) {
      if (y[i] > y_m - 2*sdy & y[i] < y_m +2*sdy){
            int2 <- int2 + 1
      }
}
int2

# e) n�mero de dados entre y_m - 3*sd_y e y_m + 3*sd_y
int3 <- 0
for (i in 1:N) {
      if (y[i] > y_m - 3*sdy & y[i] < y_m +3*sdy){
            int3 <- int3 + 1
      }
}
int3

'Exerc�cio 3 - Caracter�sticas da distribui��o triangular'
N <- 10000
z <-  runif(N, -0.5, 0.5) + runif(N, -0.5, 0.5)

# a) z_m e sigma_z
z_m <- mean(z)
sdz <- sd(z)
sdz_m <- sdz/sqrt(N)
z_m
sdz_m

# b) desvio-padr�o amostral de z
sdz

# c) n�mero de dados entre z_m - sdz e z_m + sdz
inte1 <- 0
for (i in 1:10000) {
      if (z[i] > z_m - sdz & z[i] < z_m +sdz){
          inte1 <- inte1 + 1  
      }
}
inte1

# c) n�mero de dados entre z_m - sdz e z_m + sdz
inte2 <- 0
for (i in 1:10000) {
      if (z[i] > z_m - 2*sdz & z[i] < z_m + 2*sdz){
            inte2 <- inte2 + 1
      }
}
inte2

# c) n�mero de dados entre z_m - sdz e z_m + sdz
interval3 <- 0
for (i in 1:N) {
      if(z[i] > z_m - 3*sdz & z[i] < z_m + 3*sdz){
            interval3 <- interval3 + 1
      }
}
interval3
