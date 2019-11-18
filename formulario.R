# librerie utili:
library(dplyr) # df management
library(plotly) # plots
library(vcd) # oddsratio
library(readxl) # input excel
library(binom) # IC per binomiali


##### INPUT #####
# csv:
file_name = "C:/Users/amato/Documents/R/datasets/fuel2001.csv"
data = read.csv(file_name, header = TRUE, sep = ",")
dt = data.frame(data)

# excel:
file_name = "C:/Users/amato/Documents/R/datasets/scarpe.xlsx"
data <- read_excel(file_name)
dt = data.frame(data)

# txt:
file_name = "C:/Users/amato/Documents/R/datasets/occ.txt"
data <- read.table(file_name, header = TRUE, sep = "", dec = ".")


##### UTILITIES #####
# varianza di una popolazione:
var(y)
# deviazione standard di una popolazione:
sd(y)
# media di una popolazione:
mean(y)

sd(y) = sqrt(var_y)
sd(y)^2 = var_y

# probabilità che un valore sia <= x in una normale:
pnorm(x, mean = media, sd = sd)

# varianza campionaria:
s^2 = sum((y-mean_y)^2) / n-1
# standard error di una popolazione normale:
stde = function(x, n) sd(x) / sqrt(n)

# correlazione tra x e y non avendo le distribuzioni:
E(X) = 40
E(Y) = 1.28
E(X^2) = 1700
E(Y^2) = 3.06
E(XY) = 56.4
var_x = E(X^2) - E(X)^2 = 1700 - 40^2 = 100
var_y = E(Y^2) - E(Y)^2 = 3.06 - 1.28^2 = 1.42
cov_xy = E(XY) - E(X)*E(Y) = 56.4 - 40*1.28 = 5.2
corr_xy = cov_xy / sqrt(var_x * var_y) = 5.2 / sqrt(100 * 1.42)

# intervallo di confidenza per la differenza tra medie per dist. normali:
lower = diff_mean - qnorm(0.95, df = n-1) * std_err
higher = diff_mean + qnorm(0.95, df = n-1) * std_err

# calcolo della statistica T per binomiali:
p_stima = successi / n
h0 = valore_da_verificare
t_stat = (p_stima - h0) / std_err

# p.value per la statistica test:
# a. se il test è unilaterale con > (ad esempio, mean > 0):
p.val = 1-pt(t_stat, df = n-2)
# b. se il test è unilaterale con < (ad esempio, mean < 0):
p.val = pt(t_stat, df = n-2)
# c. se il test è bilaterale con = (ad esempio, mean = 500):
p.val = 2*pt(abs(t_stat), df = n-2, lower.tail = FALSE)

# test T per l'alternativa unilaterale <, > oppure bilaterale !=:
altern = "less"
altern = "greater"
altern = "two.sided"
t.test(y, mu = value, alternative = altern)

# standard error di una popolazione binomiale:
stde = function(p, n) sqrt((p*(1-p))/n)

# intervallo di confidenza per una binomiale:
binom.confint(successi, totale, conf.level = 0.95, method = "asymptotic")

##### CONFRONTO TRA DUE MEDIE #####
# non avendo le popolazioni:
# varianza combinata di due popolazioni:
comb_var = (1/(n1+n2-2)) * ((n1-1)*(sd(x))^2 + ((n2-1)*(sd(y))^2))
# calcolo dell'errore standard:
std_err = sqrt(comb_var) * sqrt(1/n1 + 1/n2)

# calcolo della statistica T per la differenza tra medie per dist. normali:
t_stat = diff_m / std_err

# calcolo di un intervallo di confidenza per la differenza tra medie:
lower = diff_m - qt(1-0.05/2, df = (n1+n2)-2) * std_err
higher = diff_m + qt(1-0.05/2, df = (n1+n2)-2) * std_err
c(lower, higher)

# avendo le popolazioni:
# y1 e y2 le due popolazioni, mu la media, var.equal = TRUE se omoschedasticità.
# fornisce anche l'intervallo di confidenza e il p.value relativo all'ipotesi nulla:
t.test(y1, y2, mu = 0, alternative = "two.sided", var.equal = TRUE, conf.level = 0.95)

##### CONFRONTO DI DUE PROPORZIONI #####
# se si ha una tabella x del tipo:
#                 malato    non-malato    totale
# non-trattato    115       201229        201344
# trattato        33        200745        200778
# totale          148       401826        402122
# ----------------------------------------------
#                 malato    non-malato    totale
# non-trattato    R1        n1-R1         n1
# trattato        R2        n2-R2         n2
# totale          R1+R2     n-R1-R2       n1+n2
x = matrix(c(115, 201229, 33, 200745), 2, 2, byrow = TRUE)

# test per la differenza tra proporzioni:
# nb: il p-value fornito è quello relativo all'ipotesi che p1-p2=0.
prop.test(x[,1], x[,2], alternative = "two.sided", correct = FALSE, conf.level = 0.95)

# test su una proporzione per verificare l'effettività di un valore di probabilità in una binomiale:
prop.test(successi, totale, p = prob, conf.level = 0.95, correct = FALSE)

# probabilità data la matrice x:
p1 = x[1,1] / (x[1,2]+x[1,1])
p2 = x[2,1] / (x[2,2]+x[2,1])
# logit:
logit = log((successi + (1/2)) / (totale - successi + (1/2)))
# rischio relativo:
RR = p1 / p2
# l'oddsratio è dato da:
or = (x[1,1]*x[2,2]) / (x[2,1]*x[1,2])
or = oddsratio(x)
# logOR:
logOR = log(p1/(1-p1)) - log(p2/(1-p2))
# l'intervallo di confidenza dell'oddsratio è dato da:
confint(or)
# Asyntotical Standard Error per il logOR:
ase = sqrt((1/x[1,1])+(1/x[1,2])+(1/x[2,1])+(1/x[2,2]))


##### REGRESSIONE LINEARE #####
# calcolo della devianza di una variabile:
dev = function(x) sum((x - mean(x))^2)

# stima dei minimi quadrati:
beta = covarianza/var_x
alpha = mean_y - (beta * mean_x)

# calcolo del modello:
mod = lm(y ~ x, data = dt)
summary(mod)

# intervallo di confidenza dei coefficienti alpha e beta
confint(mod, conf.level = 0.95)

# calcolo della correlazione:
# se la correlazione di due variabili è positiva allora le due variabili dipendono l'una dall'altra
cor(x, y)
cor(dt)

# dato un modello di regressione, i valori previsti yhat dal modello sono dati da:
fit = fitted(mod)
# dato un modello di regressione, i valori residui del modello sono dati da:
res = residuals(mod)

# devianza spiegata:
dev_spie = dev(fit)
# varianza spiegata:
var_spie = dev_spie/(n-2)
# devianza residua:
dev_res = dev(res)
# varianza residua:
var_res = dev_res/(n-2)

# stima dello standard error del parametro:
SE_b = sqrt(var_res)/sqrt(dev(variabile))

# effettuare una predizione per un dato valore di X, dato il modello lineare e una dataframe
# contenente i valori della previsione desiderata. E' possibile anche inserirne multipli:
pred_point = data.frame(param_1 = value_1, ..., param_n = value_n)
predict(mod, newdata = pred_point, interval = "prediction", se.fit = TRUE, level = 0.95)

# calcolo della statistica F nel caso in cui non sia fornito il dataset di partenza:
# numeratore = dev_y è la devianza della variabile di risposta con sotto i suoi gradi di libertà
# denominatore = dev_res è la devianza residua con sotto i suoi graddi di libertà (n - le variabili libere)
f = (dev_y/2)/(dev_res/(n-3))

# nel caso si abbia una variabile dicotomica (x = "a" oppure x = "b") per utilizzarla in un modello di
# regressione devo trasformarla in 0/1 tramite le seguenti istruzioni:
data$new_x[data$x == "a"] = 0
data$new_x[data$x == "b"] = 1

##### PLOTS #####
# plot semplice del modello di regressione tra x e y con retta di regressione:
plot(x, y)
abline(mod)
# plot che rappresenta la retta di regressione del modello:
ggplot(data, aes(y = data$y, x = data$x, colour = data$x_dicot)) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE)
# plot che rappresenta le rette di regressione per ognuna delle due popolazioni distinte dei valori
# di x (0 e 1):
ggplot(data, aes(y = data$y, x = data$x, colour = data$x_dicot, shape = data$x)) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE)
