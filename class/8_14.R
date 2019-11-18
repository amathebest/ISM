library(ggplot2)

# data input:
file_name = "C:/Users/amato/Documents/R/datasets/gasolio.txt"
data <- read.table(file_name, header = TRUE, sep = "", dec = ".")
plot(data)

data["consumo"] = data$Gas * 28.3168466
mean(data$consumo[data$Insul=="Before"])
mean(data$consumo[data$Insul=="After"])

mod_wrong = lm(data$consumo ~ data$Insul, data = data)
summary(mod_wrong)
# questo modello è incorretto perché ho la variabile Insul che è di tipo stringa
# e non dicotomica. Effettuo quindi la trasformazione e ottengo:
data$ins_d[data$Insul == "After"] = 1
data$ins_d[data$Insul == "Before"] = 0

mod1 = lm(data$consumo ~ data$ins_d, data = data)
summary(mod1)
# questo modello è corretto e mi dimostra che, essendo il coefficiente di ins negativo,
# il consumo dopo il trattamento è più basso.

# aggiungo al modello di regressione anche la temperatura:
mean(data$Temp[data$Insul=="Before"])
mean(data$Temp[data$Insul=="After"])
mod2 = lm(data$consumo ~ data$ins_d + data$Temp, data = data)
summary(mod2)
data["res"] = residuals(mod2)
data["fit"] = fitted(mod2)

# noto che la temperatura ha come coefficiente -9.5, che mi dice che, essendo negativo,
# a parità di consumo, se la temperatura scende allora il consumo aumenta e viceversa.
# si ha quindi che, a parità di temperatura, prima e dopo il trattamento il consumo 
# di riscaldamento è inferiore, e la differenza è pari a beta.
# il valore -9.5 mi indica che, nel caso in cui la temperatura aumenti di 1 grado, il consumo
# diminuisce di 9.5 in media.
# il valore -44.3 mi indica che il consumo medio da prima a dopo il trattamento è ridotto di
# 44.3 litri.
# la differenza tra il valore dell'intercetta (185.51, ovvero alpha_hat, il valore del consumo
# medio in corrispondenza del valore di temperatura 0) e il valore di beta (-44.32, ovvero 
# la variazione media di consumo da prima a dopo il trattamento) è 141.19.

# plot che rappresenta la retta di regressione del modello mod3
ggplot(data, aes(y = data$consumo, x = data$Temp, colour = data$ins_d)) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE)
# plot che rappresenta le rette di regressione per ognuna delle due popolazioni distinte dei valori
# di ins_d (0 e 1):
ggplot(data, aes(y = data$consumo, x = data$Temp, colour = data$ins_d, shape = data$Insul)) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE)
# questo plot rappresenta con colori diversi le due porzioni della popolazione. nella fattispecie
# rappresenta in nero la popolazione prima del trattamento, e in azzurro dopo il trattamento.
# vengono rappresentate anche le due rette di regressione relative a prima e dopo il trattamento.
# noto che le rette non sono parallele, il che significa che l'effetto della temperatura
# sul consumo del riscaldamento prima e dopo il trattamento è diverso!

data["xz"] = data$ins_d * data$Temp
mod3 = lm(data$consumo ~ data$ins_d + data$Temp + data$xz, data = data)
summary(mod3)
# il coefficente associato a xz mi indica la differenza dell'impatto della variazione della temperatura
# tra prima e dopo il trattamento.
anova(mod3, mod2, test = "F")

