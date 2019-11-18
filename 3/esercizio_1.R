# sommario dei dati forniti
car_1 = 5
car_2 = 5
mean_1 = 123.8
mean_2 = 116.4
sd_1 = 4.6
sd_2 = 16.09

# calcolo la differenza tra le medie
# 123.8 - 116.4 = 7.4
diff_mean = mean_1 - mean_2 

# calcolo la varianza combinata delle due popolazioni
# comb_var = 11.83
comb_var = sqrt((1/(car_1+car_2-2)) * ((car_1-1)*(sd_1)^2 + ((car_2-1)*(sd_2)^2)))

# calcolo l'errore standard dello stimatore della differenza tra le medie
# std_err = 7.48
std_err = comb_var * sqrt(1/car_1 + 1/car_2)

# calcolo gli estremi dell'intervallo di confidenza tramite la seguente formula:
# intervallo inferiore: lower = diff_mean - t(0,95, gradi di liberta) * std_err = -6.519276
# intervallo superiore: higher = diff_mean + t(0,95, gradi di liberta) * std_err = 11.809276
lower = diff_mean - qt(1-0.05/2, df = (car_1+car_2)-2) * std_err
higher = diff_mean + qt(1-0.05/2, df = (car_1+car_2)-2) * std_err

# ora calcolo manualmente la statistica T per la differenza tra le medie:
stat_T = (diff_mean - 0) / (std_err) # = 0.35

# calcolo il p-value come 2 volte la probabilità che il valore di T trovato sia inferiore a T con n1+n2-1 gradi di libertà
p.value <- 2 * pt(-abs(stat_T), df = car_1+car_2 - 1)
# dato che il p-value risulta 0.73 deduco che il test non è stato significativo e non posso rifiutare l'ipotesi nulla.



