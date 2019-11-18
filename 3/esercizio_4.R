# Operazione      No      Si    Totale    % Complicazioni
# Ginecologica    235     5     240       2.1
# Addominale      210     35    245       14.3
# Totale          445     40    485

# La rappresentazione in tabella è una semplificazione di due set di campioni indipendenti
# estratti a caso da due popolazioni con distribuzione Bernoulliana:
# Ginecologia = Bern(p1)  
# Addominale = Bern(p2)

# dati iniziali:
ginec_si = 5
ginec_no = 235
add_si = 35
add_no = 210

ginec_tot = ginec_si + ginec_no
add_tot = add_si + add_no

# calcolo p1 come la il numero di complicazioni in ginecologia sul totale di casi in ginecologia:
p1 = ginec_si/ginec_tot
# eseguo lo stesso calcolo per addominale:
p2 = add_si/add_tot

# calcolo la differenza di probabilità tra p1 e p2:
D_stim = p2-p1 # 0.122

# calcolo l'errore standard della differenza di probabilità tra p1 e p2:
std_err_D = sqrt(p1*((1-p1)/ginec_tot) + p2*((1-p2)/add_tot)) # 0.024

lower = D_stim - (qnorm(0.95) * std_err_D) # 0.082
higher = D_stim + (qnorm(0.95) * std_err_D) # 0.162
# l'intervallo di confidenza al 95% risulta quindi essere: [0.082, 0.162]
# importante notare che l'intervallo non contiene il valore 0

P_tot = (ginec_si + add_si) / (ginec_tot + add_tot)

# calcolo ora la statistica test come: Z = p1-p2 / sqrt(P_tot*(1-P_tot)*((1/ginec_tot)+(1/add_tot)))
# ricordo che asintoticamente si comporta come una normale (0,1)
Z_stat = (p1-p2) / sqrt(P_tot*(1-P_tot)*((1/ginec_tot)+(1/add_tot))) # = -4.88 

# allora il p-value è calcolato come la probabilità che il valore trovato Z_stat sia minore di Z
p_value = 2*pnorm(Z_stat)
# dato che il p-value risulta inferiore a 0.05 rifiutiamo l'ipotesi nulla.


