'vamos primeiro importar os dados da planilha'
dados <- read.csv('new_league_data.csv')
# com os dados importados, podemos começar a fazer as análises

'1 - número de jogos até fevereiro'
resultados <- dados$Res
N <- length(resultados)
print(N)

'2 - Ánalise dos casos de vitória em casa'
# a) número de vitórias e sua incerteza
nh <- 0
for (i in 1:N) {
      if (resultados[i] == "H"){
            nh <- nh + 1
      }
}
sigma_nh <- sqrt(nh*(1-(nh/N)))

# b) frequência relativa e sua incerteza
fh <- nh/N
sigma_fh <- sigma_nh/N

'3 - Ánalise dos casos de derrota'
na <- 0
for (i in 1:N) {
      if (resultados[i] == "A"){
            na <- na + 1
      }
}
sigma_na <- sqrt(na*(1-(na/N)))

fa <- na/N
sigma_fa <- sigma_na/N

'4 - Ánalise dos casos de empate'
nd <- 0
for (i in 1:N){
      if (resultados[i] == "D"){
            nd <- nd + 1
      }
}
sigma_nd <- sqrt(nd*(1-(nd/N)))

fd <- nd/N
sigma_fd <- sigma_nd/N

'5 - probabilidade de empates'
N <- 10 # nesse caso o total de jogos é diferente
p <- fd
prob <- 0
# vou escrever um progaram de modo que todas as probailidades para 2 <= n <= 10 saiam
for (i in 0:2){
      prob <- (factorial(N)*(p**i)*((1-p)**(N-i)))/(factorial(i)*factorial(N-i)) + prob
      print(i)
      print(prob)
}
 
p10 <- (factorial(N)*(p**10)*((1-p)**(N-10)))/(factorial(10)*factorial(N-10))
print(p10)