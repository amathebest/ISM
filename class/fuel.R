# libraries
library(dplyr)

# file input e estrazione della dataframe
file_name = "C:/Users/amato/Documents/R/datasets/fuel2001.csv"
data = read.csv(file_name, header = TRUE, sep = ",")
dt = data.frame(data)

