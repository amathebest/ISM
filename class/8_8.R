library("plotly")

dev = function(x) sum((x - mean(x))^2)

# data
X = c(15, 16, 17, 18, 19, 20, 21, 22, 23)
Y = c(90, 90.9, 90.7, 87.9, 86.4, 82.5, 80.0, 76.0, 70.0)
df = data.frame(X, Y)

# plot
plot(x = X, y = Y, xlab = "X", ylab = "Y")


mod = lm(df$Y ~ df$X + I(df$X^2), data = df)
summary(mod)
# qui sto analizzando un modello in cui Y è definito da X e da X^2.
# voglio fare un test per capire se il termine quadratico è influente per la definizione di Y.
# in questo caso verifico che il p-value sia basso per il termine df$Z = X^2, e lo è.
# quindi deduco che X^2 è significativo per Y.

df["pre"] = fitted(mod)
df["res"] = residuals(mod)
mod0 = lm(df$Y ~ 1)

anova(mod, mod0, test = "F")
# questo test esprime il concetto di voler verificare se il modello in cui tutti i coefficienti 
# non sono influenti per spiegare le Y. In questo caso la Pr(>F) viene molto bassa, quindi il test
# è significativo e rifiuto l'ipotesi nulla --> i parametri sono significativi!

# per ottenere manualmente il valore della statistica F si fa:
f = (dev(df$Y)/2)*((nrow(df)-3)/dev(df$res))

plot(x = df$Y, y = df$res)
abline(a = 0, b = 0)

# tento di verificare cosa succede effettuando un modello lineare.
# noto che il R^2 viene più basso di prima, e lo standard error più alto.
# poi provo a plottare Y in relazione ai residui del modello appena stimato:
# noto che i residui hanno un andamento a parabola, quindi comprendo che serve un termine quadratico:
mod_lin = lm(df$Y ~ df$X, data = df)
summary(mod_lin)
df["res_lin"] = residuals(mod_lin)

plot(x = df$Y, y = df$res_lin)
abline(a = 0, b = 0)
