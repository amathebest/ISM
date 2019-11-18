# prezzo in euro = vettore X
# quantità venduta = vettore Y
n = 12
sum_xy = 32709.2
sum_x = 35.9
sum_y = 11090
sum_x_q = 108.13
sum_y_q = 10584612

# ricavo le seguenti quantità
mean_x = sum_x / n
mean_y = sum_y / n
dev_x = sum_x_q - (n*(mean_x)^2)
dev_y = sum_y_q - (n*(mean_y)^2)
codev = sum_xy - n*mean_x*mean_y
  
# a: stimare la retta di regressione:
# le stime dei minimi quadrati dei coefficienti alpha e beta sono dati da:
beta = codev/dev_x # -642.35
alpha = mean_y - beta*mean_x # 2845.87
# la retta di regressione è data quindi da y = 2845.87 - 642.35x
# la pendenza della retta è negativa in quanto si prevede che se il prezzo degli hamburger
# sale, allora la vendita scenda.

# b: trovare la devianza residua e l'errore standard del coefficiente di regressione (beta)
# la devianza residua è data da:
# dev(res) = dev(y) - dev(yhat) dove dev(yhat) è la devianza spiegata dal modello.
# non avendola la ricavo tramite:
dev_yhat = beta^2 * dev_x
dev_res = dev_y - dev_yhat
# la stima della varianza di errore è S^2 = dev(res)/n-2:
s_quadro = dev_res/(n-2)
# l'errore standard della del coefficiente di regressione è dato da:
SE = sqrt(s_quadro/dev_x)

# c: trovare l'intervallo di confidenza al 95% per la pendenza:
# l'intervallo di confidenza per la pendenza è dato da:
low = beta - qt(0.975, df = n-2) * SE
high = beta + qt(0.975, df = n-2) * SE
c(low, high)



