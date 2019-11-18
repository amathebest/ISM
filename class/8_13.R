library(plotly)
library(dplyr)

# data input
file_name = "C:/Users/amato/Documents/R/datasets/colesterolo.csv"
data = read.csv(file_name, header = TRUE, sep = ",")
dt = data.frame(data)
names(dt) = c("state", "col", "age")
plot(dt)

# state = 0 --) Iowa
# state = 1 --) Nebraska

mod1 = lm(col ~ age, data = dt)
summary(mod1)

mod2 = lm(col ~ age + state, data = dt)
summary(mod2)

dt["xz"] = dt$age * dt$state
mod_tot = lm(col ~ age + state + xz, data = dt)
summary(mod_tot)

anova(mod_tot, mod1, test = "F") 
# qui per effettuare il test che provi che i coeffcienti di dt$state e dt$xz sono entrambi contemporaneamente
# uguali a zero, pongo il test anova con il modello completo confrontato con il modello che contiene solo la 
# variabile dt$age. Se il test risulta significativo allora posso rifiutare l'ipotesi nulla, e dimostro che 
# c'è evidenza empirica che mi dice che dt$state e dt$xz non sono entrambe zero. nel caso in cui non risulti
# significativo posso anche semplicemente adottare il sottomodello.


# plot
plot(dt$age, dt$col)
abline(coef(mod_tot)[[1]], coef(mod_tot)[[2]], col = "blue")
a = coef(mod_tot)[[1]]+coef(mod_tot)[[3]]
b = coef(mod_tot)[[2]]+coef(mod_tot)[[4]]
abline(a, b, col = "red")
abline(coef(mod1)[[1]], coef(mod1)[[2]])
