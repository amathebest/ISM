# input
file_name = "C:/Users/amato/Documents/R/datasets/Advertising.csv"
data = read.csv(file_name, header = TRUE, sep = ",")
dt = data.frame(data)
dt$X <- NULL
head(dt)
n = nrow(dt)

# correlazione tra le variabili del dataset:
cor(dt)
# plot per verificare relazioni lineari tra le variabili:
plot(dt)

# provo a costruire un modello lineare tra sales e tv: effetto positivo e significativo
mod1 = lm(Sales ~ TV, data = dt)
summary(mod1)
# noto che è dt$TV è molto significativa per la definizione di Y.

# verifico l'accuratezza della stima dei parametri della regressione lineare calcolando
# l'errore standard dei due parametri:
sigma_q = dev(residuals(mod1))/(n-2)
SE_b = sqrt(sigma_q)/sqrt(dev(dt$TV))

# test di ipotesi su H0: beta = 0 nel modello 1:
t_stat = coef(mod1)[[2]] / SE_b
p.val = 2*pt(t_stat, df = n-2, lower.tail = FALSE) # --) p.val viene basso quindi rifiuto l'ipotesi nulla
# che dice che beta è zero. ciò mi dice che dt$TV è significativo per la definizione di dt$sales

# NB: in summary appare anche Residual Standard Error = 3.256 che significa:
# i parametri y stimati si discostano dalla retta in media di 3.256. 
# questo valore è calcolato manualmente anche eseguendo
anova(mod1)
# e prendendo il valore corrispondente a Mean Sq dei residui e metterlo sotto radice:
sqrt(10.6)
# tramite anova è possibile ottenenere anche manualmente il coefficiente di R^2:
# R^2 = dev(spiegata) / dev(totale)
Rq = 3314.6 / (3314.6+2102.5)

# a questo punto provo ad inserire anche la variabile relativa alle vendite su giornali (dt$newspaper):
mod2 = lm(dt$Sales ~ dt$TV + dt$Newspaper, data = dt)
summary(mod2)
# il coefficiente di dt$Newspaper rappresenta l'incremento medio delle vendite legato ad un incremento unitario,
# tenendo costante la variabile dt$TV.

# provo a inserire anche la variabile dt$Radio:
mod3 = lm(dt$Sales ~ dt$TV + dt$Radio + dt$Newspaper, data = dt)
summary(mod3)
# newspaper è pochissimo significativa per Y al netto delle altre due varibili. confronto
# quindi questo modello con quello in cui il coefficiente di newspaper è posto a 0
# per verificare se il modello senza quella variabile è sufficientemente descrittivo

mod4 = lm(Sales ~ TV + Radio, data = dt)
summary(mod4)
anova(mod4, mod3, test = "F")
# il p.value della statistica F risulta non significativo, quindi non posso rifiutare l'ipotesi
# nulla per la quale il coefficiente di regressione parziale di newspaper potrebbe essere 0.
# --) il modello parziale senza newspaper è sufficiente per la definizione di Y.

# le seguenti sono previsioni di valori stimati dal modello in corrispondenza dei valori selezionati.
# è possibile anche ottenere un intervallo di confidenza della previsione e lo standard error della previsione.
pred_point = data.frame(TV = 10, Radio = 20)
predict(mod4, pred_point)

pred_point = data.frame(TV = 350)
predict(mod1, pred_point, se.fit = TRUE, interval = "prediction", level = 0.95)

# plot dei residui vs valori stimati. Ci da un'idea se i parametri sono legati da relazione lineare o meno, oppure se 
# dobbiamo inserire l'interazione.
plot(fitted(mod4), residuals(mod4))

# modello con interazione:
dt["xz"] = dt$TV * dt$Radio
mod5 = lm(Sales ~ TV + Radio + xz, data = dt)
summary(mod5)
# notiamo che il valore di R^2 è sensibilmente aumentato rispetto a tutti i modelli creati in precedenza.
# è inoltre diminuito l'RSE, ovvero l'errore medio della previsione del modello. 
# il p.value di tutti i parametri risulta significativo, quindi posso dedurre che tutti i parametri sono importanti
# per la definizione di dt$Sales.
