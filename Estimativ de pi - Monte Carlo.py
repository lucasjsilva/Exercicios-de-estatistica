# Vamos tentar estimar o valor de pi à partir da área de 1/4 de círculo de raio 1

import numpy as np
r = 1
N = 1000 # número de contagens a serem feitas
n = 0
x = 1 # maior valor de x
y = 1 # maior valor de y

for i in range (N):
    (x1, y1) = np.random.rand(2)
    #y1 = np.random.rand()
    r_linha = x1**2 + y1**2
    if r_linha <= 1:
        n += 1

#uso a relação entre as áreas antes estudada:

pi = 4*n/N
print(pi)
