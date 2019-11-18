n1 = 120
n2 = 150
mean_M = 82005
mean_F = 79005
std_M = 4505
std_F = 5205

# A.1.1: si formulino ipotesi nulla e alternativa
# vogliamo verificare se la differenza tra le medie delle due popolazioni è significativa, formuliamo:
# H0: diff_mean = 0
# contro 
# H1: diff_mean != 0

# A.1.2: si calcoli la statistica test corrispondente
# per il calcolo della statistica test utilizzo la tecnica di differenza tra medie (delta), ovvero calcolo:
delta = mean_M - mean_F # 82005 - 79005 = 3000

# calcolo la varianza combinata delle due popolazioni:
comb_var = sqrt((1/(n1+n2-2)) * ((n1-1)*(std_M)^2 + ((n2-1)*(std_F)^2)))

# e successivamente l'errore standard relativo alla differenza tra medie (SE(delta)):
SE_delta = comb_var * sqrt((1/n1) + (1/n2))

# infine calcolo la statistica test:
stat_t = delta/SE_delta # stat_t = 4.99 con (n1+n2)-2 = 268 gradi di libertà.

# A.1.3: si calcoli il p-value associato alla statistica test e rispondere alla domanda:
# il p-value della statistica è dato dalla formula:
# 2*P(T > stat_t)
# quindi lo calcolo tramite:
p_val = 2 * pt(-abs(stat_t), df = 268) # p_val < 0.05

# dato che risulta un p-value di molto inferiore a 0.05, posso rifiutare l'ipotesi nulla e affermare che tra le due 
# popolazioni vi sia una differenza considerevole.

# A.2.1: si calcoli l'intervallo di confidenza al 95% per la differenza tra medie:
# l'intervallo di confidenza è dato dalla seguente formula:
# IC = [delta - t * SE(delta), delta + t * SE(delta)]
# dove t è il quantile di ordine 1-c con 268 gradi di libertà.
inf = delta - qt(0.975, df = 268) * SE_delta
sup = delta + qt(0.975, df = 268) * SE_delta
IC = c(inf, sup)


