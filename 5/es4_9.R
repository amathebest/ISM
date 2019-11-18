#libraries
require("dplyr")
library("dplyr")

# file input e estrazione della dataframe
file_name = "C:/Users/amato/Documents/R/datasets/forbes.csv"
data <- read.csv(file_name)
dt = data.frame(data)

dt["tempC"] = ((dt$TempF-32)*5)/9
dt["press_std"] = dt$Pressure*25.4

mod = lm(dt$press_std ~ dt$tempC, data = dt)
summary(mod)
summary(mod)$r.squared

# se il valore di summary(linearMod)$r.squared è abbastanza grande, possiamo essere abbastanza sicuri
# che il modello spieghi sufficientemente bene il fenomeno.
# altro modo per capire se posso rifiutare l'ipotesi che dice che beta è 0, ovvero che non ci sia 
# correlazione tra X e Y, è il p-value --) se sufficientemente piccolo (0.05) allora posso rifiutare l'ipotesi.
# in questo caso risulta essere di molto inferiore a 0.05, quindi posso tranquillamente rifiutare l'ipotesi 
# che afferma che non ci sia correlazione tra X e Y.

dt["res"] = residuals(mod)
dt["fit"] = fitted(mod)
plot(dt$fit, dt$res)

mod2 = lm(dt$press_std ~ I(dt$tempC^2), data = dt)
summary(mod2)
