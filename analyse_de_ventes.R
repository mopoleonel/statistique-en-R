library(dplyr)
library(ggplot2)

donnees <- data.frame(
  annee = rep(2022:2024, each = 4),
  trimestre = rep(1:4, times = 3),
  ventes = c(120,150,170,130,125,155,175,135,130,160,180,140)
)

head(donnees)

donnees$time <-seq.Date(from = as.Date("2022-01-01"), by= "quarter", length.out = nrow(donnees))

head(donnees)

serie_ts <- ts(donnees$ventes, start = c(2022,1),frequency = 4)

plot(serie_ts, main = "serie trimestrielle des ventes", col = "blue")

plot(seri_ts, main = "serie trimestrielle des ventes", ylab= "ventes(en milliers)", col = "red")



# Convertir les données en dataframe
df <- data.frame(Date = time(seri_ts), Ventes = as.numeric(seri_ts))

# Création du graphe avec ggplot2
ggplot(df, aes(x = Date, y = Ventes)) +
  geom_line(color = "red", size = 1.2) +  # Ligne plus épaisse et colorée
  geom_point(color = "darkred", size = 3) +  # Points pour rendre le graphique plus lisible
  theme_minimal() +  # Thème épuré et élégant
  labs(title = "📊 Série Trimestrielle des Ventes",
       subtitle = "Analyse des ventes en milliers d'unités",
       x = "Date",
       y = "Ventes (en milliers)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Titre centré et stylisé
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12))

temps <- 1: length(serie_ts)

modele_tendance <- lm(serie_ts ~ temps)

summary(modele_tendance)

beta0 <- coef(modele_tendance)[1] 
beta1 <- coef(modele_tendance)[2]  

print(paste("β₀ =", beta0, " | β₁ =", beta1))

plot(serie_ts, main = "serie avec tendance", ylab = "ventes", xlab ="temp", col ="blue")
abline(modele_tendance, col = "blue", lwd = 2)

plot(serie_ts, main = "ventes et dendance", ylab = "ventes", col = "darkgray")
abline(modele_tendance, col = "red", lwd = 2)


residue <- residuals(modele_tendance)
residue

plot(residue, type = "o", col = "purple", main = "residue de la tendance", ylab = "Residu", xlab = "Trimestre")
abline(h = 0, lty = 2, col = "gray")

decomp <- decompose(serie_ts, type = "additive")
plot(decomp)

serie_desaisonnakise <- serie_ts - decomp$seasonal

plot(serie_desaisonnakise, main = "serie desoaisonnalisee", col = "darkgreen", ylab = "ventrs")

head(serie_desaisonnakise)

valeurs_attendues <- decomp$trend + decomp$seasonal

valide <- !is.na(valeurs_attendues)
plot(serie_ts, type = "o", col = "blue", main = "serie observee vs attendue", ylab="ventes", xlab ="temps")

lines(time(serie_ts)[valide], valeurs_attendues[valide], type ="o", col ="red")

legend("topleft", legend = c("Observee", "Reconstituee"), col = c("blue", "red"),lty = 1)

head(valeurs_attendues)


