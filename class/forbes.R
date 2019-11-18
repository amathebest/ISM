#libraries
require("dplyr")
require("readxl")

library("dplyr")
library("readxl")

# file input e estrazione della dataframe
file_name = "C:/Users/amato/Documents/R/datasets/forbes.csv"
data <- read.csv(file_name)
dt = data.frame(data)

dt["tempC"] = ((dt$TempF-32)*5)/9
dt["press_std"] = dt$Pressure*25.4

linearMod = lm(dt$press_std ~ dt$tempC, data = dt)

# Coefficients:
#                 Estimate    Std. Error   t value   Pr(>|t|)    
#   (Intercept)   -1634012    43906        -37.22    3.41e-16 ***
#   dt$tempC      23907       462          51.74     < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 5914 on 15 degrees of freedom
# Multiple R-squared:  0.9944,	Adjusted R-squared:  0.9941 
# F-statistic:  2677 on 1 and 15 DF,  p-value: < 2.2e-16

# se il valore di summary(linearMod)$r.squared è abbastanza grande, possiamo essere abbastanza sicuri
# che il modello spieghi sufficientemente bene il fenomeno.
# altro modo per capire se posso rifiutare l'ipotesi che dice che beta è 0, ovvero che non ci sia 
# correlazione tra X e Y, è il p-value --) se sufficientemente piccolo (0.05) allora posso rifiutare l'ipotesi.

dt["pred_press"] = fitted(linearMod) # valore previsto della pressione sulla base del modello lineare (previsione)
dt["res"] = residuals(linearMod) # valore di scarto tra il valore vero e il valore stimato dal modello (residuo)
