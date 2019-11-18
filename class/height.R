# libraries
library("dplyr")
library("plotly")

# file input e estrazione della dataframe
file_name = "C:/Users/amato/Documents/R/datasets/height.csv"
data = read.csv(file_name, header = TRUE, sep = ",")
dt = data.frame(data)

# modello lineare tra altezza della figlia in relazione all'altezza della madre
mod = lm(dt$dheight ~ dt$mheight, data = dt)

