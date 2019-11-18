# dati
n = 40
alpha = 199.5
bx = -9.38
bz = -0.68
bw = 9.29
SE_x = 2.24
SE_z = 0.56
SE_w = 1.94
dev_res = 480.5
# sottomodello
alpha_2 = 204.56
bx_2 = -9.16
SE_x_2 = 2.83
dev_res_2 = 813.3


# a.: determinare un intervallo di confidenza per il coefficiente di regressione di Z:
inf = bz - qt(0.975, df = n-4) * SE_z
sup = bz + qt(0.975, df = n-4) * SE_z
c(inf, sup) # [-1.816, 0.456] --) contiene lo zero e quindi bz non è significativo.

# b.: stimare la varianza degli errori nel primo modello:
s_quadro = dev_res/(n-4)
s = sqrt(var_err_quadro) # 3.653

# c.: sottoporre a verifica l'ipotesi nulla di uguaglianza a zero dei coefficienti Z e W
dev_y = dev_res_2 - dev_res
f = (dev_y/2)/(dev_res/(n-4))
# ora, per determinare se il test è significativo, calcolo il p-value.
# qualora il p-value risultasse basso, posso rifiutare con confidenza l'ipotesi nulla che afferma
# che i coefficienti Z e W siano entrambi uguali a zero, ovvero che non intervengano in modo
# significativo alla definizione di Y.
p.value = 1 - pf(q = f, df1 = 2, df2 = n-4)
# siccome risulta <0.05 posso rifiutare l'ipotesi nulla.



