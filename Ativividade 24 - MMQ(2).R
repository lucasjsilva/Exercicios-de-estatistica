# primeiro, vou copiar a fun��o do exerc�cio anterior
MMQ <- function(y, t, si, G){
      # vou iniciar os vetores que ser�o usados ao longo do c�digo
      N <- length(y)
      
      P <- dim(G)[2] # quantidade de coeficientes no polin�mio F(x,a)
      # O sistema de equ��es do MMQ pode ser escrito na forma matricial D = M %*% A
      # em que D � um vetor, M uma matriz e A um vetor 
      # vou ent�o iniciar uma matriz e um vetor vazios para M e D
      M <- matrix(0, nrow = P, ncol = P) # tenho 2 gs no polin�mio F(x,a)
      D <- c()
      
      # realizo agora o somat�rio atrav�s dos loops
      for (L in 1:P) {
            # vou atualizando o D
            D[L] <- sum(y*G[,L]/(si^2))
            for (C in 1:P) {
                  M[L, C] <- sum(G[,L]*G[,C]/(si^2))
            }
      }
      
      # para determinar a matriz de covari�ncia, basta calcular a matriz inversa de M
      covA <- solve(M)
      
      # a matriz de correla��o tamb�m pode ser determinada
      s_a1 <- sqrt(covA[1,1])
      s_a2 <- sqrt(covA[2,2])
      corA <- covA
      corA[1,1] <- corA[1,1]/(s_a1^2)
      corA[1,2] <- corA[1,2]/(s_a1*s_a2)
      corA[2,1] <- corA[2,1]/(s_a1*s_a2)
      corA[2,2] <- corA[2,2]/(s_a2^2)
      
      # para determinar o vetor A � necess�rio multiplicar o inverso da matriz M com 
      # o vetor D
      A <- covA %*% D
      
      # Os valores de F(x,a) podem ser deteminados pelo produto de G e A
      F <- G %*% A
      
      # com isso o Chi2 pode ser determinado como
      Chi2 <- sum(((y-F)/(si))^2)
      return(list(F = F, coeficientes=A, incerteza1 = s_a1, incerteza2 = s_a2,
                  covariancia = covA, correla��o = corA,
                   Chi2 = Chi2))
}

# agora vou ler a tabela com os dados
dados <- read.table("dados_osciloscopio.txt")
t <- dados[,1] # dados referentes ao tempo (s)
y <- dados[,2] # dados referentes � tens�o (V)
plot(t,y, xlab = "Tempo (s)", ylab = "Tens�o (V)", main = "Gr�fico da 
     Tens�o x Tempo")
f <- 2 #(Hz)
si <- 0.06 # V
# preciso escrever os elementos para a matriz G
g1 <- cos(2*pi*f*t)
g2 <- sin(2*pi*f*t)
G <- matrix(c(g1, g2), nrow = length(t), ncol = 2)

# aplico agora a fun��o
ajusteMMQ <- MMQ(y, t, si, G)

# determino a amplitude
A <- ajusteMMQ$coeficientes
amplitude <- sqrt((A[1]^2)+(A[2]^2))


'a) par�metros ajustados'
print(ajusteMMQ$coeficientes)
print(ajusteMMQ$incerteza1)
print(ajusteMMQ$incerteza2)

'b) covari�ncia e correla��o'
print(ajusteMMQ$covariancia)
print(ajusteMMQ$correla��o)

'c) amplitude e incerteza'
print(amplitude)
del_a1 <- A[1]/amplitude
del_a2 <- A[2]/amplitude

inc_Amp <- sqrt((del_a1*ajusteMMQ$incerteza1)**2
                + (del_a2*ajusteMMQ$incerteza2)**2 + 
                   (2*del_a1*del_a2*ajusteMMQ$covariancia[2,1])
                   )
print(inc_Amp)

'd) gr�ficos'
# gr�ficos
# dados + fun��o ajustada
f_ajustada <- A[1]*cos(2*pi*f*t) + A[2]*sin(2*pi*f*t)

plot(t, f_ajustada, main = "Gr�fico da fun��o ajustada e dos dados",
     xlab = "Tempo (s)", ylab = "Tens�o (V)", type = "l", col = "red",
     lwd = 5) # plot em linha
par(new = T)
plot(t, y, col = "azure2", axes = FALSE)

# plot dos res�duos
residuos <- y - (ajusteMMQ$F)
plot(t, residuos, main = "Gr�fico de res�duos")

'e) Chi2 e NGL'
NGL = length(y) - 2
print(NGL)
print(ajusteMMQ$Chi2)