# data initialization
n_uomini = 120
n_donne = 150
mean_uomini = 82005
mean_donne = 79005
sd_uomini = 4505
sd_donne = 5205


# A1.1: Si formulino le ipotesi nulla e alternativa:
# Come prima cosa calcolo il valore della differenza di medie:
diff_mean = mean_uomini - mean_donne # 3000

# In seguito, calcolo la varianza combinata delle due popolazioni:
comb_var = (1/(n_uomini+n_donne-2)) * ((n_uomini-1)*(sd_uomini)^2 + (n_donne-1)*(sd_donne)^2) # 24073954

# Tramite il valore della varianza combinata calcolo quindi il valore dell'errore standard:
std_e = sqrt(comb_var) * sqrt((1/n_uomini) + (1/n_donne)) # 600.924

# Formulo quindi le ipotesi nulla e alternativa:
# H0: diff_mean = 0 contro H1: diff_mean != 0


# A1.2: Si calcoli la statistica test corrispondente:
# La statistica test per la verifica delle ipotesi per la differenza tra medie è data da:
stat_t = diff_mean / std_e # 4.9923


# A1.3: Si calcoli il p-value associato alla statistica t e si risponda alla domanda:
# Il p-value associato alla statistica t osservata è dato dalla formula:
pvalue = 2*pt(stat_t, df = (n_uomini+n_donne-2), lower.tail = FALSE) # 1.0755 x 10^-6
# Essendo il valore del p-value molto piccolo (in particolare, minore di 0.05), posso dire che c'è 
# evidenza empirica contro l'ipotesi nulla, che viene quindi rifiutata. La differenza delle medie è sensibilmente
# diversa da 0.


# A2.1 Costruire l'intervallo di confidenza al 95% per la differenza delle medie, assumendo la normalità
# e l'omoschedasticità:
# Gli estremi dell'intervallo di confidenza al 95% sono dati dalle seguenti due istruzioni:
lower = diff_mean - qt(1-0.05/2, df = (n_uomini+n_donne-2)) * std_e
higher = diff_mean + qt(1-0.05/2, df = (n_uomini+n_donne-2)) * std_e
c(lower, higher)
