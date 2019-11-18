library(ggplot2) # plots

# data input 
file_name = "C:/Users/amato/Documents/R/datasets/caschool.csv"
data = read.csv(file_name, header = TRUE, sep = ",")
dt = data.frame(data)
head(dt)

# studiare la relazione tra il reddito medio del distretto dove si trova la scuola e il punteggio al test.

plot(dt$avginc, dt$testscr)
# chiaramente la relazione tra le due variabili non è lineare, è chiaramente esponenziale
# genero quindi le variabili trasformate nel loro logaritmo:
dt = data.frame(dt, log(dt$avginc))
dt = data.frame(dt, log(dt$testscr))

mod1 = lm(dt$log.dt.testscr. ~ dt$log.dt.avginc., data = dt)
summary(mod1)

# stimo i valori previsti:
df["fit"] = fitted(mod1)

# provo a creare un modello log-normale trasformando solamente la variabile y e non la x e li plotto:
mod2 = lm(dt$log.dt.testscr. ~ dt$avginc, data = dt)
summary(mod2)
# di base il beta nel primo modello sta a rappresentare l'elasticità. il valore di beta nel secondo modello
# indica la variazione del log del voto sulla base di una variazione unitaria del reddito.
plot(dt$avginc, dt$testscr)
abline(mod2, col = "red")
abline(mod1, col = "blue")

ggplot(dt, aes(avginc,testscr)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, formula = dt$log.dt.testscr. ~ dt$log.dt.avginc.) + 
  geom_smooth(method = "lm", se = FALSE, formula = dt$log.dt.testscr. ~ dt$avginc)

# proviamo ora anche ad adattare un modello polinomiale con un termine quadratico:
mod3 = lm(dt$testscr ~ dt$avginc + I(dt$avginc^2), data = dt)
summary(mod3)
