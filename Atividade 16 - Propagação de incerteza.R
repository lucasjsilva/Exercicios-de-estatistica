'gerar dados aleatórios w=x.y'
M <- 10000
x0 <- 15
y0 <- 40
sigmax <- 2
sigmay <- 3
x <- rnorm(M, mean = x0, sd = sigmax)
y <- rnorm(M, mean = y0, sd = sigmay)
w <- x*y
# a) valor medio e desvio-padrão
wmed <- mean(w)
sdw <- sd(w)
sd_med <- sdw/sqrt(M)
print(wmed)
print(sdw)
print(sd_med)

# b1) 2 medidas de x e 1 de y
x1 <- rnorm(M, mean = x0, sd = sigmax)
x2 <- rnorm(M, mean = x0, sd = sigmax)
x_b1 <- (x1+x2)/2
y_b1 <- rnorm(M, mean = y0, sd = sigmay)
w_b1 <- x_b1*y_b1
wm_b1 <- mean(w_b1)
sdw_b1 <- sd(w_b1)
sdwmed_b1 <- sdw_b1/sqrt(M)
print(wm_b1)
print(sdw_b1)
print(sdwmed_b1)

# b2) 1 medida de x e 2 de y
x_b2 <- rnorm(M, mean = x0, sd = sigmax)
y1 <- rnorm(M, mean = y0, sd = sigmay)
y2 <- rnorm(M, mean = y0, sd = sigmay)
y_b2 <- (y1 + y2)/2
w_b2 <- x_b2*y_b2
wm_b2 <- mean(w_b2)
sdw_b2 <- sd(w_b2)
sdwm_b2 <- sdw_b2/sqrt(M)
print(wm_b2)
print(sdw_b2)
print(sdwm_b2)

'Caso em que é possível fazer onze medições'
N <- 11 # número máximo
Nx <- 1 # conta o número de xs medidos
Ny <- N-Nx # conta o número de ys medidos
sdw3 <- c() # vai armazenar os 10 valores do desvio padrão
while (Nx < N){ # condição para parada
      x3 <- vector(mode = "numeric", length = M) # cria um vetor de 0s
      for (i in 1:Nx) {
            x3 <- x3 + rnorm(M, mean = x0, sd = sigmax)
            #print(head(x))
      }
      # para finalizar a media divido pelo número de elemento somados
      x_med <- x3/Nx
      # repito o mesmo para y
      
      y3 <- vector(mode = "numeric", length = M)
      for (i in 1:Ny) {
            y3 <- y3 + rnorm(M, mean = y0, sd = sigmay)
      }
      y_med <- y3/Ny
      #print(Ny)
      # preciso agora obter o valor de w e seu desvio padrão
      w3 <- x_med*y_med
      sigma_w3 <- sd(w3)
      
      # agora adiciono isso à lista sdw
      sdw3 <- c(sdw3, sigma_w3)
      # preciso atualizar as variáveis Nx e Ny
      Nx <- Nx + 1
      Ny <- N - Nx
}
nx <- 1:10
ny <- 10:1
print(data.frame(nx = nx, ny = ny, sd = sdw3))
