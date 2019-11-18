#libraries
require("dplyr")
library("dplyr")
library("plotly")

# file input e estrazione della dataframe
file_name = "C:/Users/amato/Documents/R/datasets/derived.txt"
data <- read.table(file_name, header = TRUE, sep = "", dec = ".")

# nuova colonna BMI
data["BMI"] = data$Wei/(data$Hei/100)^2

p <- plot_ly(data, x = ~data$BMI, y = ~data$Dia, z = ~data$Age, colors = c("blue", "red", "green")) %>% 
      add_markers() %>% 
      layout(
        scene = list(
          xaxis = list(title = "BMI"),
          yaxis = list(title = "Pressure"),
          zaxis = list(title = "Age")
        )
      )

# regressione lineare tra pressione Dia e BMI
mod1 = lm(data$BMI ~ data$Dia, data = data)

# regressione lineare tra pressione Dia ed età
mod2 = lm(data$BMI ~ data$Age, data = data)

# regressione lineare tra pressione Dia, età e BMI
mod3 = lm(formula = data$BMI ~ data$Dia + data$Age, data = data)
# matrice di correlazione tra BMI, Dia e Age
summary(mod3, correlation = TRUE)$correlation

# residui del modello Y rispetto a X, Y rispetto a Z e poi Y rispetto a XZ
data["resyx"] = residuals(mod2)
data["resyz"] = residuals(mod1)
data["resyxz"] = residuals(mod3)


# intervallo di confidenza per il modello di regressione multivariato:
confint(mod3)



# coeff. reg. e corr. parziali per X su Y tenendo costante Z e per Z su Y tenendo costante X






