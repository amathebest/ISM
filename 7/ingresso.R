# data input
file_name = "C:/Users/amato/Documents/R/datasets/testingresso.csv"
data = read.csv(file_name, header = TRUE, sep = ",")
dt = data.frame(data)

dt$sex[dt$sesso == "M"] = 0
dt$sex[dt$sesso == "F"] = 1

mod1 = lm(dt$punteggiofinale ~ dt$sex + dt$votomat, data = dt)
summary(mod1)
# noto che tutte e due le variabili esplicative sono identificate come significative nel 
# modello. le due rette di regressione identificate dal modello sono le seguenti:
# prima retta di regressione, ovvero in corrispondenza di dt$sex = 0, risulta:
# Y = 6.68 + 0.098Z
# seconda retta di regressione, ovvero in corrispondenza di dt$sex = 1, risulta:
# Y = (6.68 + (-2.05)) + 0.098Z = 4.22 + 0.098Z
# questo modello assume che la pendenza delle due rette di regressione sia uguale, ovvero che
# l'effetto del voto di maturità sul punteggio finale sia lo stesso sia nel caso dei
# maschi che nel caso delle femmine.

dt["xz"] = dt$sex * dt$votomat

mod2 = lm(dt$punteggiofinale ~ dt$sex + dt$votomat + dt$xz, data = dt)
summary(mod2)

# in seguito all'aggiunta dell'interazione nel modello l'assunzione di prima cambia. si ha infatti
# che in mod2 non si assume costante l'effetto del voto di maturità sul punteggio finale del test,
# ma si assume che le due rette abbiano pendenza diversa, ovvero che l'effetto sul punteggio finale
# sia diverso per i maschi e per le femmine. Notiamo però che il p.value in corrispondenza delle
# due variabili dt$sex e dt$xz (la variabile di interazione) risultano non significativi. Questi sono
# i p.value relativi ai test parziali.
# le due rette di regressione così stimate risultano:
# prima retta di regressione, dt$sex = 0:
# Y = 6.88 + 0.095Z
# seconda retta di regressione, dt$sex = 1:
# Y = (6.88 + (-2.47)) + (0.095 + 0.0053)Z = 4.42 + 0.101Z
# ciò mi dice che effettivamente risulta esserci, seppur molto piccola, una differenza nelle pendenze
# delle due rette di regressione, ovvero nell'effetto del voto di maturità sul punteggio del test a
# seconda che il candidato sia maschio o femmina.
# nonostante questo il valore di R^2 risulta molto basso per entrambi i modelli stimati. 
# questo mi fa capire che probabilmente il modello non studia bene il fenomeno, e che potrebbero esserci
# dei parametri importanti non presi in considerazione.
