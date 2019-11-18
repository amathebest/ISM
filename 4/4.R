#libraries
library("readxl")

# file input e estrazione della dataframe
file_name = "C:/Users/amato/Documents/R/datasets/testingresso.csv"
data <- read.csv(file_name)

# modello lineare in cui variabile esplicativa = data$votomat e variabile di risposta = data$punteggiofinale
mod <- lm(data$punteggiofinale ~ data$votomat, data = data)
summary(mod)

# correlazione
cor(data$votomat, data$punteggiofinale)

plot(x = data$votomat, y = data$punteggiofinale, xlab = "Maturita'", ylab = "Test")
abline(mod, col = "red")

# esecuzione manuale dei calcoli per ricavare i parametri di alpha (intercetta) e beta (pendenza)
mean_x = mean(data$votomat)
mean_y = mean(data$punteggiofinale)
covarianza = cov(data$votomat, data$punteggiofinale)
var_x = var(data$votomat)
var_y = var(data$punteggiofinale)
beta = covarianza/var_x
alpha = mean_y - (beta * mean_x)

# tramite entrambi i metodi usati per calcolare il coefficiente di regressione, esso risulta essere:
# beta = 0.08
# i metodi usati sono stati:
# 1. calcolo dei coefficienti della regressione lineare tramite il modello lm() di R
# 2. calcolo dei coefficieni della regressione lineare tramite il calcolo manuale dei due valori alpha e beta
# 
# Nonostante la retta di regressione abbia una pendenza molto bassa, si può notare tramite il grafico come
# esista poca correlazione tra i due dati X (= voti alla maturità) e Y (= voti al test). La retta ha comunque
# una pendenza crescente, quindi una leggera relazione che indica che al crescere di X cresce anche Y esiste.
# 
# Osservo inoltre che il valore di R^2 non è alto (0.05), il che significa che il modello non spiega bene 
# la relazione che sussiste tra X e Y.

# il test per verificare che esista relazione lineare tra x e y è ottenuto tramite:
# t = beta_stimato / standard_error
# lo standard error è ricavato dal summary(mod) sotto la colonna di Std. Err. e nella riga della variabile
# esplicativa.
std_err = 0.0103
t_stat = beta / std_err

p.value = 1-pnorm(q = t_stat)

# posso anche ottenere lo stesso risultato guardando la statistica F e il suo p.value relativo fornito dal
# summary(mod). Perciò, dato che il p.value risulta basso, rifiuto l'ipotesi nulla che afferma che non vi sia
# relazione lineare tra le due popolazioni studiate, ciò significa che beta è significativamente diverso da zero.

