N <- 100
x0 <- 50 # metros
sigma0 <- 1 # metro
nREP <- 10000
medias <- c() # vai guardar as médias
medianas <- c()# vai guardar as medianas
desvs <- c() # vai guardar os desvios padrões amostrais
nval <- c() # vai guardar n
mdesv <- c() # vai guardar m

# vou gerar o valor médio, a mediana
for (i in 1:nREP) {
      x <- x0+rnorm(N, mean = 0, sd = sigma0)
      xm <- mean(x)
      xM <- median(x)
      sdx <- sd(x)
      n <- 0
      m <- 0
      # agora tenho que avaliar quem está dentro do intervalo de 1 sigma
      for (j in 1:N) {
             if (Mod(x[j] - x0) <= sigma0){
                   n <- n + 1
             }
             if (Mod(x[j] - xm) <= sdx){
                   m <- m + 1
             }
       }
      # atualizo os vetores
      medias <- c(medias, xm)
      medianas <- c(medianas, xM)
      desvs <- c(desvs, sdx)
      nval <- c(nval, n)
      mdesv <- c(mdesv, m)
}
df <- data.frame(medias, medianas, desvs, nval, mdesv, )

'a) Determinar os desvios-padrões amostrais das médias e das medianas'
sd_media <- sd(medias)
inc_med <- sd_media/sqrt(2*(length(medias)-1))
sd_medianas <- sd(medianas)
inc_medianas <- sd_medianas/sqrt(2*length(medianas)-1)
sd_media
inc_med
sd_medianas
inc_medianas

z <- (medias + medianas)/2
sdz <- sd(z)
incz <- sdz/sqrt(2*(length(z) - 1))
sdz
incz

'b) '

# 1
n_med <- mean(nval)
sdn <- sd(nval)
sdn_med<- sdn/sqrt(length(nval))
n_med
sdn_med

m_med <- mean(mdesv)
sdm <- sd(mdesv)
sdm_med <- sdm/sqrt(length(mdesv))
m_med
sdm_med

# 2
p <- 0.6826
v_esperado <- N * p
v_esperado

# 3
inc_sdn <- sdn/sqrt(2*(length(nval) - 1))
sdn
inc_sdn

inc_sdm <- sdm/sqrt(2*(length(mdesv) - 1))
sdm
inc_sdm

# 4
s_esperado <- sqrt(N*p*(1-p))
s_esperado