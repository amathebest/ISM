X = c(1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000)
Y = c(75.995, 91.972, 105.711, 123.203, 131.669, 150.697, 179.323, 203.212, 226.505, 249.633, 281.422)

dev = function(x) sum((x - mean(x))^2)

# a: grafico con sulle ascisse il tempo (x) e sulle ordinate la popolazione (y)
plot(x = X, y = X, xlab = "Year", ylab = "Population")

# b: adattare un modello lineare e un modello quadratico e poi farne il grafico
mod_l = lm(Y ~ X)
summary(mod_l)

mod_q = lm(Y ~ X + I(X^2))
summary(mod_q)

plot(X, Y, xlab = "Year", ylab = "Population")
abline(mod_l, col = "red")

plot(I(X^2), Y, xlab = "Year", ylab = "Population")
abline(mod_q, col = "blue")

# c: valutare osservando i residui quale dei due modelli risulta più appropriato
res_l = residuals(mod_l)
res_q = residuals(mod_q)
df = data.frame(X, Y, res_l, res_q)
dev(res_l) # 875.60
dev(res_q) # 793.31
# noto che i residui in entrambi i modelli risultano abbastanza simili. Osservando la devianza
# dei due residui (lineare e quadratico) noto però che quella del modello quadratico risulta
# leggermente inferiore. posso dedurre quindi, anche dal fatto che il valore del coefficiente
# R^2 nel caso del secondo modello risulta leggermente più alto, che il secondo modello è leggermente
# migliore del primo.

# d: adattare un polinomio di grado 8 e riportarlo sul grafico
mod_8 = lm(Y ~ I(X^8))
summary(mod_8)

# e: determinare la popolazione stimata nel 2020 utilizzando il modello quadratico e quello polinomiale
p1 = predict(mod_q, newdata = data.frame(X = 2020), interval = "prediction", se.fit = TRUE, level = 0.95)
p2 = predict(mod_8, newdata = data.frame(X = 2020), interval = "prediction", se.fit = TRUE, level = 0.95)

# per p1 si ha: valore_previsto = 309.32, SE = 6.96, IP = [282.88, 335.75]
# per p2 si ha: valore_previsto = 322.79, SE = 5.28, IP = [303.60, 341.99]
# noto quindi che a livello teorico, il modello con polinomio di grado 8 ha uno standard error più basso,
# e quindi leggermente più affidabile.

