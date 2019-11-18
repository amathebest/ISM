library(readxl)
library(dplyr)

dev = function(x) sum((x - mean(x))^2)

# input:
file_name = "C:/Users/amato/Documents/R/datasets/salary_alr3.xlsx"
data <- read_excel(file_name)
dt = data.frame(data)
n = nrow(dt)

# a: studiare la dipendenza del salario dal sesso:
mas = dt %>% filter(dt$Sex == 0)
fem = dt %>% filter(dt$Sex == 1)

plot(dt$Year, dt$Salary)
points(dt$Year[dt$Sex==0], dt$Salary[dt$Sex==0], col = "blue")
points(dt$Year[dt$Sex==1], dt$Salary[dt$Sex==1], col = "red")

mod_a = lm(dt$Salary ~ dt$Sex, data = dt)
summary(mod_a)

# come si vede dal grafico in modo evidente, all'interno del dataset in oggetto non esiste una chiara 
# relazione tra il sesso di una persona e il suo stipendio, in quanto entrambe le rette di regressione 
# risultano essere quasi coincidenti.
# inoltre posso dedurre che non sussiste particolare relazione lineare tra le due variabili in quanto 
# all'interno di summary(mod_a) risulta che la variabile dt$Sex risulta marchiata come molto poco 
# significativa.
# un altro fattore importante è il valore di R^2, che risulta essere 0.063, ovvero molto basso.
# ciò indica una scarsa capacità del modello di spiegare la variabilità della variabile di risposta.

# b: studiare il modello di regressione dello stipendio dal sesso e dall'anzianità. 
mod_b = lm(dt$Salary ~ dt$Sex + dt$Year, data = dt)
summary(mod_b)
anova(mod_a, mod_b, test = "F")

# tramite l'analisi del risultato dell'istruzione anova(mod_a, mod_b, test = "F") posso capire che:
# il test F in oggetto è il test dell'ipotesi nulla che afferma che non vi sia relazione tra i parametri
# dt$Year e dt$Sex per la spiegazione del parametro dt$Salary. siccome questo valore risulta essere 
# molto significativo, rifiuto l'ipotesi nulla. affermo inoltre che dt$Year è molto importante per la stima 
# di dt$Salary perché all'interno di summary(mod_b) è marchiata come molto significativa, e il suo p.value risulta
# molto basso. Un'altra differenza sostanziale tra mod_a e mod_b è inoltre il valore di R^2. nel primo caso, R^2
# risultava essere circa 0.063, ovvero una chiara indicazione che il modello non riesce a stimare con precisione la
# variabile di risposta. Nel caso del secondo modello invece questo valore si alza fino a 0.49, comunque non
# molto alto, ma sicuramente molto migliore del caso del primo modello. deduco quindi che dt$Year è una variabile
# esplicativa molto importante per la definizione di dt$Salary.

# c: introdurre anche l'indicatore del dottorato: vi sono differenze?
mod_c = lm(dt$Salary ~ dt$Sex + dt$Year + dt$Degree, data = dt)
summary(mod_c)
anova(mod_b, mod_c, test = "F")

# noto che la nuova variabile esplicativa non è marcata come significativa nel summary. inoltre vedo che il valore
# di R^2 non è cresciuto tanto, quindi posso affermare che non si riscontrano sostanziali differenze con il modello
# precedente.

# d: sottoporre a test delle ipotesi che l'indicatore del sesso e del dottorato siano simultaneamente nulli:
mod_d = lm(dt$Salary ~ dt$Year, data = dt)
summary(mod_d)
anova(mod_c, mod_d, test = "F")



