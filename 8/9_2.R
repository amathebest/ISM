library(plotly)

# dataset input
file_name = "C:/Users/amato/Documents/R/datasets/ais.csv"
data = read.csv(file_name, header = TRUE, sep = ",")
dt = data.frame(data)
n = nrow(dt)
head(dt)
n

# L'idea con cui approccio alla risoluzione del problema è quella di partire da un modello
# generale in cui vengono considerate varie variabili per la definizione di LBM, per poi 
# man mano eliminare quelle meno significative e raggiungere un modello (o più modelli) in
# cui il valore di R^2 risulti abbastanza alto e la significatività dei parametri risulti
# per tutti alta. Verifico infine aggiungendo l'interazione se il modello acquisisce potenza.

filtered_df = data.frame(dt$LBM, dt$Ht, dt$Wt, dt$RCC)
plot(filtered_df)
# Dal plot della dataframe con le colonne di interesse posso vedere chiaramente come RCC non
# sembri avere una relazione di chiara linearità con le altre variabili.
# Questa ipotesi è rafforzata dalla matrice di correlazione:
cor(filtered_df)

# modello senza il sesso
mod1 = lm(LBM ~ Ht + Wt + RCC, data = dt)
summary(mod1)

# modello con il sesso
mod2 = lm(LBM ~ Sex + Ht + Wt + RCC, data = dt)
summary(mod2)

# modello con il sesso e senza RCC
mod3 = lm(LBM ~ Sex + Ht + Wt, data = dt)
summary(mod3)

# verifico che RCC non sia significativo per la definizione di LBM tramite:
anova(mod2, mod3, test = "F")
# il p.value relativo alla statistica F non risulta significativo, quindi posso
# accettare l'ipotesi nulla che indicava che RCC non è significativo per la 
# variabile di risposta ed escluderlo dall'insieme delle variabili esplicative.

# modello senza l'altezza
mod4 = lm(LBM ~ Sex + Wt, data = dt)
summary(mod4)

# modello con interazione tra sesso e peso
dt["xz"] = dt$Sex * dt$Wt
mod5 = lm(LBM ~ Sex + Wt + xz, data = dt)
summary(mod5)
# Noto che il valore di R^2 risulta molto elevato in quest'ultimo modello. Posso quindi affermare
# che le variabili che spiegano meglio la variabile LBM sono il peso conformato con il sesso 
# dell'individuo considerato.
# in particolare, le due rette di regressione, quelle in corrispondenza di Sex=0 e Sex=1 sono:
# Sex=0 --> y = 10.851 + 0.773x
# Sex=1 --> y = (10.851+4.72) + (0.773-0.189)x = 15.571 + 0.584x
# 
# Provando a plottare il risultato si ha:
dt$MasFem[dt$Sex == 0] = "Uomini"
dt$MasFem[dt$Sex == 1] = "Donne"
ggplot(dt, aes(y = LBM, x = Wt, colour = Sex, shape = MasFem)) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE)

# NB: non bisogna considerare non significativo il parametro Sex in questo ultimo modello,
# in quanto avendo inserito l'interazione tra Sex e Wt togliendo la variabile Sex dal modello
# l'interazione perderebbe significato.

