# inizializzazione dei dati
before = c(100.8, 102.0, 105.9, 108.0, 92.0, 116.7, 110.2, 135.0, 123.5, 95.0, 105.0, 85.0, 107.2, 80.0, 115.1, 103.5, 82.0, 101.5, 103.5, 93.0)
after = c(97.0, 107.5, 97.0, 108.0, 84.0, 111.5, 102.5, 127.5, 118.5, 94.2, 105.0, 82.4, 98.2, 83.6, 115.0, 103.0, 80.0, 101.5, 102.6, 93.0)

# calcolo le medie e le varianze delle due distribuzioni
mean_before = mean(before)
mean_after = mean(after)
sd_before = sd(before)
sd_after = sd(after)

# calcolo la differenza delle medie
diff_mean = mean_before - mean_after # = 2.645

# sotto l'assunzione di omoschedasticità, stimo la varianza combinata
comb_var = sqrt(1/(length(before)+length(after)-2) * ((length(before)-1)*(sd_before)^2 + (length(after)-1)*(sd_after)^2)) # = 13.01
# calcolo l'errore standard basandomi sul valore di varianza combinata calcolato tramite stima
std_err = comb_var * sqrt(1/length(before) + 1/length(after)) # = 4.11

# sottopongo i due dataset al test di uguaglianza delle medie
confronto <- t.test(before, after)

# tramite i parametri forniti dal t.test, osservo le seguenti:
# l'intervallo di confidenza contiene il valore 0, essendo: [-5.68, 10.97]
# il p-value risultante è 0.52, ovvero ci fa capire che il risultato del test 
# non è significativo per affermare che il programma di perdita di peso sia 
# stato significativo oppure no.