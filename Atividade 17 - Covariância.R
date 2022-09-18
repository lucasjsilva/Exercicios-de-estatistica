C_cal <- 129.753
sd_cal <- 42.262
c_h20 <- 4.18441
sd_h20 <- 0.0685581
covariancia <- -2.82012

# a) C_cal e c_h20
print(C_cal)
print(c_h20)

# b) covariância
print(covariancia)

# b1) coeficiente de correlação
coef_correlacao <- covariancia/(sd_cal * sd_h20)
print(coef_correlacao)

# c) Capacidade térmica total quando m=1000g
C <- C_cal + 1000*c_h20
sd_C <- sqrt((sd_cal)**2 + (1000*sd_h20)**2 + (2*1000*covariancia)) 
print(C)
print(sd_C)

# d) determinar a massa p/ qual a incerteza é mínima
'temos que derivar a incerteza em relação à massa e igualar a 0. A expressão que 
obtemos é:'
m_min <- -covariancia/sd_h20**2
sd_min <- sqrt((sd_cal)**2 + (m_min*sd_h20)**2 + (2*m_min*covariancia)) 
print(m_min)
print(sd_min)
 
# e) teste sem covariancia
sd_itemc <- sqrt((sd_cal)**2 + (1000*sd_h20)**2)
sd_itemd <- sqrt((sd_cal)**2 + (m_min*sd_h20)**2) 
print(sd_itemc)
print(sd_itemd)
print(sd_C)
print(sd_min)