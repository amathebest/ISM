#libraries
require("dplyr")
library("dplyr")

# file input
file_name = "C:/Users/amato/Documents/R/datasets/occ.txt"
data <- read.table(file_name, header = TRUE, sep = "", dec = ".")

# filtro i dati in modo da ottenere due vettori contenenti i diversi salari delle due tipologie di lavoro
typeA <- data %>% filter(OCC == "a")
typeB <- data %>% filter(OCC == "b")
typeA_sal = typeA$SALARIO
typeB_sal = typeB$SALARIO

# differenza tra medie
diff_mean = abs(mean(typeA_sal) - mean(typeB_sal))

# standard deviation delle due popolazioni
sd_tA = sd(typeA_sal)
sd_tB = sd(typeB_sal)

# varianza combinata 
comb_var = sqrt(1/(length(typeA_sal)+length(typeB_sal)-2) * ((length(typeA_sal)-1)*(sd_tA)^2 + (length(typeB_sal)-1)*(sd_tB)^2))

# standard error
std_err = comb_var * sqrt(1/length(typeA_sal) + 1/length(typeB_sal))

# la statistica test è quindi la seguente
tst = (diff_mean - 0) / std_err 
# Il numeratore e il denominatore della statistica t di Student sono i seguenti:
# T = 2407.736 / 1130.408 = 2.13


# richiamo la funzione t.test per effettuare il confronto tra le medie
confronto <- t.test(typeA_sal, typeB_sal)
my_pvalue <- confronto$p.value

# se il p.value risulta inferiore a 0.05 posso essere nella posizione di rifiutare l'ipotesi nulla:
# my_pvalue = 0.0349
# dato che risulta my_pvalue < 0.05 rifiuto l'ipotesi nulla.

