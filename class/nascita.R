n1 = 12
n2 = 12
n = n1 + n2
mean_x = 3024
mean_y = 2911.333
SE_x = 284.2189
SE_y = 280.4423

# a: sottoporre a test l'ipotesi di uguaglianza delle medie:
comb_var = (1/(n1+n2-2)) * ((n1-1)*(SE_x)^2 + ((n2-1)*(SE_y)^2)) # 79714.13
std_err = sqrt(comb_var) * sqrt(1/n1 + 1/n2) # 115.26
diff_m = mean_x - mean_y # 112.667
test_t = diff_m / std_err # 0.9775 con 24-2=22 gradi di libertà
# il p.value è quindi dato da:
p.val = 2*pt(abs(t_stat), df = n-2, lower.tail = FALSE) # 0.33
# il p.value risulta non significativo, quindi non posso rifiutare l'ipotesi nulla
# di uguaglianza delle medie, ovvero che la differenza del peso alla nascita per
# maschi e femmine non è significativa.


# b: costruire un intervallo di confidenza al 95% per la differenza tra medie:
lower = diff_m - qt(p = 0.975, df = n-2) * std_err
higher = diff_m + qt(p = 0.975, df = n-2) * std_err
c(lower, higher) # [-126.375, 351.709]
