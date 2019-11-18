# script di supporto per il calcolo del p-value dei test di uguaglianza di proporzioni

a11 = 198
b11 = 104
c11 = 10845
d11 = 10933

a12 = 227
b12 = 107
c12 = 132
d12 = 678

a13 = 90
b13 = 84
c13 = 165
d13 = 307

# riporto gli odds ratio calcolati precedentemente sul foglio di esercizi
OR11 = 1.83
OR12 = 10.89
OR13 = 1.99

# calcolo la statistica test di uguaglianza delle proporzioni per il p-value
Z_test_11 = log(OR11)/sqrt((1/a11)+(1/b11)+(1/c11)+(1/d11))
Z_test_12 = log(OR12)/sqrt((1/a12)+(1/b12)+(1/c12)+(1/d12))
Z_test_13 = log(OR13)/sqrt((1/a13)+(1/b13)+(1/c13)+(1/d13))

p_value_11 = 2*pnorm(Z_test_11)
p_value_12 = 2*pnorm(Z_test_12)
p_value_13 = 2*pnorm(Z_test_13)
