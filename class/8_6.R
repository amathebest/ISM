# txt:
file_name = "C:/Users/amato/Documents/R/datasets/marks.txt"
dt <- read.table(file_name, header = TRUE, sep = "", dec = ".")
plot(dt)
cor(dt)

mod_tot = lm(dt$statistics ~ dt$mechanics + dt$vectors + dt$algebra + dt$analysis, data = dt)
summary(mod_tot)

# R^2 è alto ma non troppo (0.4793) quindi deduco che il modello spiega approssimativamente ma non in
# modo completo le Y. 
# nel summary è espresso anche un elenco di test t che mi illustrano l'indipendenza condizionata delle variabili.
# nel caso in cui un test t sia significativo, allora potrei confidentemente rifiutare l'ipotesi nulla, ovvero
# dedurre che quella variabile influenza in modo considerevole il risultato delle Y.

# il summary mi illustra anche il test F che mi da queste informazioni:
# il test F confronta i due seguenti modelli:
# M1: E(Y|X) = b0 + b1*dt$1 + b2*dt$2 + b3*dt$3 + b4*dt$4
# M0: E(Y|X) = b0
# quindi questo test F verifica la seguente ipotesi: H0: b1=b2=b3=b4=0 contro H1: almeno una delle b != 0
# il che significa la confidenza per cui tutte le variabili non spieghino la variabile di risposta.
# siccome il valore di F è particolarmente elevato posso tranqiillamente rifiutare questa ipotesi H0.
# (info fornita anche dal p-value della statistica F, se >0.05 il test è significativo)

# voglio ora fare un test delle ipotesi per cui meccanica e vettori sono uguali a zero.
# per farlo devo prima stimare il modello risultante dalle altre due variabili
mod_a = lm(dt$statistics ~ dt$algebra + dt$analysis, data = dt)
summary(mod_a)
# in seguito eseguo il seguente il comando per trovare la differenza tra il modello globale e quello parziale:
anova(mod_tot, mod_a, test = "F")
# il p-value risulta 0.9376, che è alto, e quindi non posso rifiutare l'ipotesi nulla del test F, ovvero il test
# che voleva verificare se dt$mechanics e dt$vectors fossero uguali a 0.


