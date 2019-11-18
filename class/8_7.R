# libraries
library("dplyr")
library("plotly")
library("readxl")

# file input e estrazione della dataframe
file_name = "C:/Users/amato/Documents/R/datasets/tab8_7.xlsx"
data <- read_excel(file_name)
dt = data.frame(data)

# matrice di correlazione e plot
cor(dt)
plot(dt)

# modello di regressione lineare di Y in relazione a x1, x2, x3, x4
mod = lm(dt$y ~ dt$x1 + dt$x2 + dt$x3 + dt$x4, data = dt)
summary(mod)
mod0 = lm(dt$y ~ 1)

anova(mod0, mod, test = "F")
# il test F ottenuto da qui è un test che mi verifica se il valore atteso di Y è uguale alla sua media,
# ovvero nel caso in cui tutti i coefficienti di beta siano uguali a 0, ovvero il test congiunto.
# se il p-value del test congiunto è bassissimo, rifiuto l'ipotesi nulla, che afferma che tutti i 
# coefficienti siano nulli.

mod1 = lm(dt$y ~ dt$x1 + dt$x2, data = dt)
summary(mod1)
