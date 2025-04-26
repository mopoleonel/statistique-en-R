library(dplyr)
library(TTR)
library(zoo)

# 📌 Étape 2 : Création du dataframe des ventes
values <- data.frame(
  annee = rep(2023:2024, each = 4),  # Répartition des années sur les trimestres
  trimestre = rep(1:4, times = 2),  # Attribution des trimestres
  ventes = c(20,30,60,25,22,33,66,27)  # Ventes trimestrielles
)
print(head(values))  # Vérification des données

# 📌 Étape 3 : Ajout d'une colonne 'time' avec les dates réelles
values$time <- seq.Date(from = as.Date("2023-01-01"), by = "quarter", length.out = nrow(values))
print(head(values))  # Vérification des dates

# 📌 Étape 4 : Transformation en série temporelle trimestrielle
serie_cl <- ts(values$ventes, start = c(2023, 1), frequency = 4)
plot(serie_cl, main = "Série trimestrielle des ventes", col = "blue", ylab = "Ventes")

# 📌 Étape 5 : Estimation de la tendance avec régression linéaire
temps <- 1:length(serie_cl)
model_tendance <- lm(serie_cl ~ temps)
summary(model_tendance)  # Résumé du modèle

# 📌 Étape 6 : Tracer la série avec la tendance
plot(serie_cl, main = "Série avec tendance", ylab = "Ventes", col = "blue")
abline(model_tendance, col = "red", lwd = 2)

# 📌 Étape 7 : Analyse des résidus
residues <- residuals(model_tendance)
print(head(residues))  # Vérification des premiers résidus

# 📌 Étape 8 : Visualisation des résidus
plot(residues, type = "o", col = "purple", main = "Résidus de la tendance", ylab = "Résidus", xlab = "Trimestre")
abline(h = 0, lty = 2, col = "gray")

# 📌 Étape 9 : Décomposition additive de la série
decompe <- decompose(serie_cl, type = "additive")
plot(decompe)  # Visualisation de la décomposition (tendance, saisonnalité, résidus)

# 📌 Étape 10 : Désaisonnalisation de la série
serie_desaisonnalisee <- serie_cl - decompe$seasonal
plot(serie_desaisonnalisee, main = "Série désaisonnalisée", col = "darkgreen", ylab = "Ventes")
print(head(serie_desaisonnalisee))  # Vérification des premières valeurs désaisonnalisées

# 📌 Étape 11 : Lissage avec moyenne mobile
serie_lissee <- rollmean(serie_cl, k = 2, fill = NA)
plot(serie_cl, main = "Lissage des ventes (Moyenne mobile)", col = "blue", ylab = "Ventes")
lines(serie_lissee, col = "red", lwd = 2)  # Ajout du lissage en rouge

# 📌 Étape 12 : Lissage exponentiel
serie_exp_smooth <- EMA(serie_cl, n = 3)
plot(serie_cl, main = "Lissage exponentiel des ventes", col = "blue", ylab = "Ventes")
lines(serie_exp_smooth, col = "green", lwd = 2)  # Ajout du lissage en vert

# 📌 Étape 13 : Prévision des ventes pour T1 et T2 2025
temps_futur <- data.frame(temps = 9:10)
previsions_tendance <- predict(model_tendance, newdata = temps_futur)

# 📌 Étape 14 : Ajout des composantes saisonnières pour T1 et T2
coef_sais <- decompe$seasonal
saisons <- tapply(decompe$seasonal, cycle(serie_cl), mean)  # Calcul de la moyenne saisonnière

sais_T1 <- saisons[1]
sais_T2 <- saisons[2]

# 📌 Étape 15 : Calcul des prévisions ajustées avec la saisonnalité
previsions_T1_2025 <- previsions_tendance[1] + sais_T1
previsions_T2_2025 <- previsions_tendance[2] + sais_T2

print(previsions_T1_2025)  # Affichage de la prévision pour T1 2025
print(previsions_T2_2025)  # Affichage de la prévision pour T2 2025

# 📌 Étape 16 : Visualisation des prévisions
cl_future <- ts(c(serie_cl, previsions_T1_2025, previsions_T2_2025), start = c(2023,1), frequency = 4)
plot(cl_future, type = "o", col = "blue", main = "Série avec prévisions T1 & T2 2025")
abline(v = c(2025), col = "red", lty = 2)
