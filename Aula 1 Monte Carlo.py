# Vamos tentar calcular a integral do sen(x) de o a pi
# esperamos que o valor calculado seja igual a 2
import numpy as np

x0 = 0 #limites da integração
x1 = np.pi
y_max = 1 #limite um pouco maior para garantir que tudo estará no quadrado
N = 10000
n = 0 # pontos que estão abaixo da integral

'''
vou gerar 100 pontos na área desse quadrado
'''

for i in range (N):
    x = x0 + (x1 - x0)*np.random.randn() # o maior número aleatório será 1
    y = y_max*np.random.randn()
    #coloco aqui a condição que vai identificar os pontos que estão abaixo de 1
    if y <= y_max:
        n += 1

area_1 = (n/N)*(x1-x0)*y_max
print("Área ~",area_1)
