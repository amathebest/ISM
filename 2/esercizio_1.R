#libraries
require("dplyr")
library("dplyr")

# file input
file_name = "C:/Users/amato/Documents/R/datasets/testingresso.csv"
data <- read.csv(file_name, header = TRUE)

# filtro i dati in modo da ottenere due vettori contenenti i punteggi delle femmine e dei maschi
fem <- data %>% filter(sesso == "F")
mas <- data %>% filter(sesso == "M")
fem_marks = fem$punteggiofinale
mas_marks = mas$punteggiofinale

# differenza tra medie
diff_mean = abs(mean(mas_marks) - mean(fem_marks))

# standard deviation delle due popolazioni
sd_fem = sd(fem_marks)
sd_mas = sd(mas_marks)

# varianza combinata
comb_var = sqrt(1/(length(fem_marks)+length(mas_marks)-2) * ((length(fem_marks)-1)*(sd_fem)^2 + (length(mas_marks)-1)*(sd_mas)^2))

# standard error
std_err = comb_var * sqrt(1/length(fem_marks) + 1/length(mas_marks))

# la statistica test è quindi la seguente
tst = (diff_mean - 0) / std_err 
# Il numeratore e il denominatore della statistica t di Student sono i seguenti:
# T = 1.63 / 0.231 = 7.048

# richiamo la funzione t.test per effettuare il confronto tra le medie
confronto <- t.test(fem_marks, mas_marks)
my_pvalue <- confronto$p.value

# se il p.value risulta inferiore a 0.05 posso essere nella posizione di rifiutare l'ipotesi nulla:
# my_pvalue = 3.07 x 10^-12
# dato che risulta my_pvalue < 0.05 rifiuto l'ipotesi nulla.




