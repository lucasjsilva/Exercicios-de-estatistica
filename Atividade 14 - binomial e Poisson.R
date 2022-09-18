# Primeiramente, vamos importar os dados a serem usados
dados <- read.csv("Irlanda.csv")
# a coluna que nos interessa é a última, a qual contem os resultados
resultados <- dados$Res

'1 - Total de elementos '
N <- length(resultados)
print(N)

'2 - número médio de gols marcados pelo mandante e pelo visitante
     com suas respectivas incertezas.'
med_mandante <- mean(dados$HG)
sd_mandante <- sd(dados$HG)
sdmed_mand <- sd_mandante/sqrt(N)
med_visitante <- mean(dados$AG)
sd_visitante <- sd(dados$AG)
sdmed_visitante <- sd_visitante/sqrt(N)
print(med_mandante)
print(sd_mandante)
print(sdmed_mand)
print(med_visitante)
print(sd_visitante)
print(sdmed_visitante)

'3 - Contagens e freq relativa de HG'
# inicializo os contadores e depois percorro a coluna para verificar as contagens
cont_0 <- 0
cont_1 <- 0
cont_2 <- 0
cont_3 <- 0
cont_4 <- 0
cont_5 <- 0
cont_6 <- 0 
for (i in 1:N) {
      if (dados$HG[i] == 0){
            cont_0 <- cont_0 + 1
      }
      if (dados$HG[i] == 1){
            cont_1 <- cont_1 + 1
      }
      if (dados$HG[i] == 2){
            cont_2 <- cont_2 + 1
      }
      if (dados$HG[i] == 3){
            cont_3 <- cont_3 + 1
      }
      if (dados$HG[i] == 4){
            cont_4 <- cont_4 + 1
      }
      if (dados$HG[i] == 5){
            cont_5 <- cont_5 + 1
      }
      if (dados$HG[i] == 6){
            cont_6 <- cont_6 + 1
      }
      
}
contadores <- c(cont_0, cont_1, cont_2, cont_3, cont_4, cont_5, cont_6)
# as incertezas serão calculadas pela binomial
sd_c0 <- sqrt(cont_0*(1-(cont_0/N)))
sd_c1 <- sqrt(cont_1*(1-(cont_1/N)))
sd_c2 <- sqrt(cont_2*(1-(cont_2/N)))
sd_c3 <- sqrt(cont_3*(1-(cont_3/N)))
sd_c4 <- sqrt(cont_4*(1-(cont_4/N)))
sd_c5 <- sqrt(cont_5*(1-(cont_5/N)))
sd_c6 <- sqrt(cont_6*(1-(cont_6/N)))
incertezas_c <- c(sd_c0, sd_c1, sd_c2, sd_c3, sd_c4, sd_c5, sd_c6)
# calculo agora as frequências relativas
frel_0 <- cont_0/N
frel_1 <- cont_1/N
frel_2 <- cont_2/N
frel_3 <- cont_3/N
frel_4 <- cont_4/N
frel_5 <- cont_5/N
frel_6 <- cont_6/N
frequencias <- c(frel_0, frel_1, frel_2, frel_3, frel_4, frel_5, frel_6)
#as incertezas das frequências vão ser dadas por:
sd_f0 <- sd_c0/N
sd_f1 <- sd_c1/N
sd_f2 <- sd_c2/N
sd_f3 <- sd_c3/N
sd_f4 <- sd_c4/N
sd_f5 <- sd_c5/N
sd_f6 <- sd_c6/N
incertezas_f <- c(sd_f0, sd_f1, sd_f2, sd_f3, sd_f4, sd_f5, sd_f6)
# agora vou gerar os dados supondo que seguem um modelo de Poisson
PH0 <- (med_mandante**0)*exp(-med_mandante)/factorial(0)
PH1 <- (med_mandante**1)*exp(-med_mandante)/factorial(1)
PH2 <- (med_mandante**2)*exp(-med_mandante)/factorial(2)
PH3 <- (med_mandante**3)*exp(-med_mandante)/factorial(3)
PH4 <- (med_mandante**4)*exp(-med_mandante)/factorial(4)
PH5 <- (med_mandante**5)*exp(-med_mandante)/factorial(5)
PH6 <- (med_mandante**6)*exp(-med_mandante)/factorial(6)
probabilidades <- c(PH0, PH1, PH2, PH3, PH4, PH5, PH6)
# vou colocar todos esses dados em uma tabela
tabela_HG <- data.frame(ocorrencias = contadores, incertezas = incertezas_c, 
                     frequencias = frequencias, incertezas_f = incertezas_f,
                     P_HGm = probabilidades, row.names = c(0,1,2,3,4,5,6))

print(tabela_HG)

'4 - Contagens e freq relativa de AG'
# inicializo os contadores e depois percorro a coluna para verificar as contagens
cont0 <- 0
cont1 <- 0
cont2 <- 0
cont3 <- 0
cont4 <- 0
cont5 <- 0
cont6 <- 0 
for (i in 1:N) {
      if (dados$AG[i] == 0){
            cont0 <- cont0 + 1
      }
      if (dados$AG[i] == 1){
            cont1 <- cont1 + 1
      }
      if (dados$AG[i] == 2){
            cont2 <- cont2 + 1
      }
      if (dados$AG[i] == 3){
            cont3 <- cont3 + 1
      }
      if (dados$AG[i] == 4){
            cont4 <- cont4 + 1
      }
      if (dados$AG[i] == 5){
            cont5 <- cont5 + 1
      }
      if (dados$AG[i] == 6){
            cont6 <- cont6 + 1
      }
      
}
contadores <- c(cont0, cont1, cont2, cont3, cont4, cont5, cont6)
# as incertezas serão calculadas pela binomial
sdc0 <- sqrt(cont0*(1-(cont0/N)))
sdc1 <- sqrt(cont1*(1-(cont1/N)))
sdc2 <- sqrt(cont2*(1-(cont2/N)))
sdc3 <- sqrt(cont3*(1-(cont3/N)))
sdc4 <- sqrt(cont4*(1-(cont4/N)))
sdc5 <- sqrt(cont5*(1-(cont5/N)))
sdc6 <- sqrt(cont6*(1-(cont6/N)))
incertezas_c <- c(sdc0, sdc1, sdc2, sdc3, sdc4, sdc5, sdc6)
# calculo agora as frequências relativas
frel0 <- cont0/N
frel1 <- cont1/N
frel2 <- cont2/N
frel3 <- cont3/N
frel4 <- cont4/N
frel5 <- cont5/N
frel6 <- cont6/N
frequencias <- c(frel0, frel1, frel2, frel3, frel4, frel5, frel6)
# as incertezas das frequências vão ser dadas por:
sdf0 <- sdc0/N
sdf1 <- sdc1/N
sdf2 <- sdc2/N
sdf3 <- sdc3/N
sdf4 <- sdc4/N
sdf5 <- sdc5/N
sdf6 <- sdc6/N
incertezas_f <- c(sdf0, sdf1, sdf2, sdf3, sdf4, sdf5, sdf6)
# agora vou gerar os dados supondo que seguem um modelo de Poisson
PH0 <- (med_visitante**0)*exp(-med_visitante)/factorial(0)
PH1 <- (med_visitante**1)*exp(-med_visitante)/factorial(1)
PH2 <- (med_visitante**2)*exp(-med_visitante)/factorial(2)
PH3 <- (med_visitante**3)*exp(-med_visitante)/factorial(3)
PH4 <- (med_visitante**4)*exp(-med_visitante)/factorial(4)
PH5 <- (med_visitante**5)*exp(-med_visitante)/factorial(5)
PH6 <- (med_visitante**6)*exp(-med_visitante)/factorial(6)
probabilidades <- c(PH0, PH1, PH2, PH3, PH4, PH5, PH6)
# vou colocar todos esses dados em uma tabela
tabela <- data.frame(ocorrencias = contadores, incertezas = incertezas_c, 
                     frequencias = frequencias, incertezas_f = incertezas_f,
                     P_HGm = probabilidades, row.names = c(0,1,2,3,4,5,6))

print(tabela)

'5 - Incerteza por Poisson'
sdP_HG <- sqrt(med_mandante)
sdP_AG <- sqrt(med_visitante)
print(sdP_HG)
print(sdP_AG)

'6 - Valor médio da soma e sua incerteza'
T_soma <- dados$HG + dados$AG
T_med <- mean(T_soma)
T_sd <- sd(T_soma)
print(T_med)
print(T_sd)

'7 - incerteza por Poisson'
sdP_T <- sqrt(T_med)
print(sdP_T)

