# data
n = 40

# mod 1
alpha = -1936.4
beta = 120.9
gamma = 163.0
SE_b = 20.46
SE_g = 21.587
dev_res = 31370.04

# mod 2
alpha_par = 27974.7
beta_par = 112.7
SE_b_par = 115.3
dev_res_par = 79714.12

# intervallo di confidenza al 95% per gamma (il coefficiente di regressione di Y e Z al netto di X)
inf = gamma - qt(0.975, df = n-2) * SE_g
sup = gamma + qt(0.975, df = n-2) * SE_g
# l'intervallo risulta essere quindi:
c(inf, sup) # [119.3, 206.7]

# stimare la varianza degli errori nel primo modello
var_err = dev_res/(n-3) # 847.8

# test t per uguaglianza a zero di beta al netto di X (1%)
test_t = gamma / SE_g # 7.55
val_t = qt(0.995, df = n-2)
# pongo l'ipotesi nulla: gamma = 0.
# Nel caso in cui il valore della statistica capiti nell'intervallo:
# [-val_t, val_t] = [-2.71, 2.71]
# allora posso accettare l'ipotesi nulla.
# In questo caso non posso accettarla, quindi posso dedurre che esista una relazione lineare tra Y e Z al netto di X.

# test f per uguaglianza a zero di beta al netto di Z (1%)
test_f = (beta^2)*dev_x / (dev_res/(n-2))





