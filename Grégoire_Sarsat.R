#1. Chargement des bibliothèques
library(readxl)                # Lecture de fichiers Excel
library(xts)                   # Manipulation des séries temporelles
library(PerformanceAnalytics)  # Analyse des rendements financiers
library(zoo)                   # Séries temporelles indexées par date
library(fBasics)               # Statistiques descriptives
library(forecast)              # Analyse ARIMA et séries temporelles
library(rugarch)               # Modélisation GARCH
library(lmtest)                # Tests statistiques
library(FinTS)                 # Tests spécifiques pour les séries temporelles
library(parallel)


axa <- read_excel("C:/Users/12312950/Downloads/AXA.xlsx")
engie <- read_excel("C:/Users/12312950/Downloads/ENGIE.xlsx")
base<-merge(axa,engie, by = "Date")
td <- as.Date(base$Date, format = "%d/%m/%y")
AXA <- xts(base$AXA, order.by = td)
ENGIE <- xts(base$ENGIE, order.by = td)


summary(base)       # Vérifie les statistiques de base
anyNA(base)         # Vérifie la présence de NA
base <- na.omit(base)               # Supprime les NA
CAC <- xts(base$dj, order.by = td) #zoo xts est un data.frame indexé par une série temporelle
#                                             Question 1. Calcul des rendements
AXA_r <- CalculateReturns(AXA, method = "discrete")[-1, ] #choisir discrete si l'on veut des rendements (pt/pt-1)-1
ENGIE_r <- CalculateReturns(ENGIE, method = "discrete")[-1, ]
base<-merge(AXA_r,ENGIE_r, by = "Date")

#visualisation
AXA_ENGIE_r <- merge(AXA_r, ENGIE_r)
colnames(AXA_ENGIE_r) <- c("AXA", "ENGIE")
plot.zoo(AXA_ENGIE_r, main = "Rendements AXA vs ENGIE", plot.type = "single", col = c("blue", "red"))
legend("bottomleft", legend = c("AXA", "ENGIE"), col = c("blue", "red"), lwd = 2)
#la base de donnée va de 2006 à 2025, entre ces deux années, le CAC40 a enregistré de fortes baissent 
#en 2008 lors de la crise des subprimes et en 2020 lors de la crise sanitaire


#                              Question 2 calculer les statistiques descriptives 
basicStats(AXA_r) # Statistiques descriptives
basicStats(ENGIE_r) # Statistiques descriptives
#pour AXA, la distribution des rendements affiche Skewness 0.64 et un coefficient de Kurtosis 13.26, 
#Variance 0.000524 et moyenne de 0,0319%
#pour ENgie, la distribution des rendements affiche Skewness 0.23 et un coefficient de Kurtosis 15.21
#Variance 0.000308 et moyenne de 0.0252%

#                                              Question 3 comparaison des séries 
normalTest(AXA_r, method = 'jb')  # Test Jarque-Bera pour la normalité de la courbe
normalTest(ENGIE_r, method = 'jb')  
d1 <- density(AXA_r)
d2 <- density(ENGIE_r)
#affichage de la densité des rendements AXA
plot(d1$x, d1$y, type = "l", xlab = "Rendements", ylab = "Densité", main = "Densité AXA")
plot(d1, col = "blue", main = "Densité et Histogramme des rendements AXA", xlab = "Rendements", ylab = "Densité")
hist(AXA_r, prob = TRUE, breaks = 100, add = TRUE)
legend("topright", legend = c("Densité AXA"), col = "blue", lty = 1, lwd = 2)
#affichage de la densité des rendements ENGIE
plot(d2$x, d2$y, type = "l", xlab = "Rendements", ylab = "Densité", main = "Densité ENGIE")
hist(ENGIE_r, prob = TRUE, breaks = 100, add = TRUE)
legend("topright", legend = c("Densité ENGIE"), col = "blue", lty = 1, lwd = 2)
#Identifier les séries avec des rendements ou écarts types maximaux
basicStats(AXA_r) #st dev0.022896 Maximum 0.218274 Minimum -0.183653
basicStats(ENGIE_r)#Stdev 0.017557 Maximum 0.250440 Minimum -0.171569

#                                             Question 4 Calcule de la VAR et ES
position = 1000000
#AXA
# Comparaison des méthodes pour la VaR et l'ES à 99%
VaR_historical_axa <- VaR(AXA_r, p = 0.99, method = "historical")*position
VaR_gaussian_axa <- VaR(AXA_r, p = 0.99, method = "gaussian")*position
VaR_modified_axa <- VaR(AXA_r, p = 0.99, method = "modified")*position

ES_historical_axa <- ES(AXA_r, p = 0.99, method = "historical")*position
ES_gaussian_axa <- ES(AXA_r, p = 0.99, method = "gaussian")*position
ES_modified_axa <- ES(AXA_r, p = 0.99, method = "modified")*position
# Affichage des résultats AXA
results_axa <- data.frame(
  Method = c("Historical", "Gaussian", "Modified"),
  VaR = c(VaR_historical_axa, VaR_gaussian_axa, VaR_modified_axa),
  ES = c(ES_historical_axa, ES_gaussian_axa, ES_modified_axa)
)
print(results_axa)
#Method        VaR         ES
# Historical  -66295.39  -90928.91
#   Gaussian  -52944.15  -60702.07
#   Modified -109507.08 -109507.08

#ENGIE
# Comparaison des méthodes pour la VaR et l'ES à 99%
VaR_historical_engie <- VaR(ENGIE_r, p = 0.99, method = "historical")*position
VaR_gaussian_engie <- VaR(ENGIE_r, p = 0.99, method = "gaussian")*position
VaR_modified_engie <- VaR(ENGIE_r, p = 0.99, method = "modified")*position

ES_historical_engie <- ES(ENGIE_r, p = 0.99, method = "historical")*position
ES_gaussian_engie <- ES(ENGIE_r, p = 0.99, method = "gaussian")*position
ES_modified_engie <- ES(ENGIE_r, p = 0.99, method = "modified")*position
# Affichage des résultats ENGIE
results_engie <- data.frame(
  Method = c("Historical", "Gaussian", "Modified"),
  VaR = c(VaR_historical_engie, VaR_gaussian_engie, VaR_modified_engie),
  ES = c(ES_historical_engie, ES_gaussian_engie, ES_modified_engie)
)
print(results_engie)
#      Method       VaR        ES
# Historical -48639.58 -68486.19
#   Gaussian -40776.76 -46725.64
#   Modified -99970.03 -99970.03



#                        Question 5 En déduire le var sur un horizon de 15 jours 

horizon <- 15
#AXA
VaR_historical_axa_15 <- sqrt(horizon) * VaR_historical_axa
cat("VaR pour 15 jours historique :", VaR_historical_axa_15, "\n")
#VaR pour 15 jours historique : -256760.9 
VaR_gaussian_axa_15 <- sqrt(horizon) * VaR_gaussian_axa
cat("VaR pour 15 jours normale :", VaR_gaussian_axa_15, "\n")
#VaR pour 15 jours normale : -205051.8 
VaR_modified_axa_15 <- sqrt(horizon) * VaR_modified_axa
cat("VaR pour 15 jours Cornish-Fisher:", VaR_modified_axa_15, "\n")
#VaR pour 15 jours Cornish-Fisher: -424119.1 

#ENGIE
VaR_historical_engie_15 <- sqrt(horizon) * VaR_historical_engie
cat("VaR pour 15 jours historique :", VaR_historical_engie_15, "\n")
#VaR pour 15 jours historique : -188380.3 
VaR_gaussian_engie_15 <- sqrt(horizon) * VaR_gaussian_engie
cat("VaR pour 15 jours normale :", VaR_gaussian_engie_15, "\n")
#VaR pour 15 jours normale : -157927.7 
VaR_modified_engie_15 <- sqrt(horizon) * VaR_modified_engie
cat("VaR pour 15 jours Cornish-Fisher :", VaR_modified_engie_15, "\n")
#VaR pour 15 jours Cornish-Fisher : -387182.3 

#Backtesting 
#                                             Question 6 visualisation de la VAR
# Paramètres pour la VaR
n.obs <- nrow(AXA_r)
FE <- 1000  # Fenêtre d'estimation
FT <- n.obs - FE  # Fenêtre de test
alpha <- 0.99  # Niveau de confiance

# Backtesting de la VaR
backTestVar <- function(x, p = 0.99) {
  normal.var <- as.numeric(VaR(x, p = p, method = "gaussian"))
  historical.var <- as.numeric(VaR(x, p = p, method = "historical"))
  modified.var <- as.numeric(VaR(x, p = p, method = "modified"))
  ans <- c(normal.var, historical.var, modified.var)
  names(ans) <- c("Normal", "HS", "Modified")
  return(ans)
}

# Estimation de la VaR
var.results <- rollapply(
  data = as.zoo(AXA_r), 
  width = FE,
  FUN = backTestVar, 
  p = alpha, 
  by.column = FALSE, 
  align = "right"
)

# Décalage pour aligner les données
var.results <- lag(var.results, -1)

# Graphique des résultats
chart.TimeSeries(merge(AXA_r, var.results), legend.loc = "topright")

# Création de la matrice de violations
violations.mat <- matrix(0, 3, 5)  # Matrice vide
rownames(violations.mat) <- c("Normal", "HS", "Modified")  # Nommer les lignes
colnames(violations.mat) <- c("En1", "ni", "1-alpha", "Percent", "VR")  # Nommer les colonnes

violations.mat[, "En1"] <- (1 - alpha) * FT
violations.mat[, "1-alpha"] <- 1 - alpha

# Calcul des violations pour la VaR normale
normalVaR.violations <- as.zoo(AXA_r[index(var.results), ]) < var.results[, "Normal"]
violation.dates <- index(normalVaR.violations[which(normalVaR.violations)])

# Représentation graphique des violations
plot(as.zoo(AXA_r[index(var.results), ]), col = "blue", ylab = "Return", main = "Violations de la VaR AXA_r")
abline(h = 0, col = "gray", lty = 2)
lines(var.results[, "Normal"], col = "black", lwd = 2)
points(as.zoo(AXA_r[violation.dates, ]), col = "red", pch = 16, lwd = 2)
legend("topright", legend = c("Rendements", "VaR Normale", "Violations"), col = c("blue", "black", "red"), lty = c(1, 1, NA), pch = c(NA, NA, 16))



# Calcul des statistiques des violations
for (i in colnames(var.results)) {
  var.violations <- as.zoo(AXA_r[index(var.results), ]) < var.results[, i]
  violations.mat[i, "ni"] <- sum(var.violations)
  violations.mat[i, "Percent"] <- sum(var.violations) / FT
  violations.mat[i, "VR"] <- violations.mat[i, "ni"] / violations.mat[i, "En1"]
}


#                        question 7 affichage de la matrice de violation de la VAR
violations.mat
#la matrice de violation pour AXA est de            
#          En1 ni 1-alpha     Percent        VR
#Normal   38.52 44    0.01 0.011422638 1.1422638
#HS       38.52 32    0.01 0.008307373 0.8307373
#Modified 38.52 12    0.01 0.003115265 0.3115265

# Calcul des violations pour la VaR normale
normalVaR.violations <- as.zoo(ENGIE_r[index(var.results), ]) < var.results[, "Normal"]
violation.dates <- index(normalVaR.violations[which(normalVaR.violations)])

# Représentation graphique des violations ENGIE_r
plot(as.zoo(ENGIE_r[index(var.results), ]), col = "blue", ylab = "Return", main = "Violations de la VaR ENGIE_r")
abline(h = 0, col = "gray", lty = 2)
lines(var.results[, "Normal"], col = "black", lwd = 2)
points(as.zoo(ENGIE_r[violation.dates, ]), col = "red", pch = 16, lwd = 2)
legend("topright", legend = c("Rendements", "VaR Normale", "Violations"), col = c("blue", "black", "red"), lty = c(1, 1, NA), pch = c(NA, NA, 16))

# Calcul des statistiques des violations pour ENGIE_r
for (i in colnames(var.results)) {
  var.violations <- as.zoo(ENGIE_r[index(var.results), ]) < var.results[, i]
  violations.mat[i, "ni"] <- sum(var.violations)
  violations.mat[i, "Percent"] <- sum(var.violations) / FT
  violations.mat[i, "VR"] <- violations.mat[i, "ni"] / violations.mat[i, "En1"]
}

violations.mat
#la matrice de violation pour ENGIE est : 
#           En1 ni 1-alpha     Percent        VR
#Normal   38.52 37    0.01 0.009605400 0.9605400
#HS       38.52 24    0.01 0.006230530 0.6230530
#Modified 38.52  7    0.01 0.001817238 0.1817238


#PORTEFEUILLE 
#On considere un portefeuille A composé de 60% des actions AXA et 40% des actions ENGie
returns <- merge(AXA_r, ENGIE_r)
colnames(returns) <- c("AXA", "ENGIE")
# Poids du portefeuille
weights <- c(0.6, 0.4)

#                    Question 9 Calculer le rendement moyen et le risque (ecart type) du portefeuille.

portfolio_mean_return <- Return.portfolio(returns, weights = weights, geometric = FALSE)
mean_portfolio <- mean(portfolio_mean_return)
cat(" le rendement moyen et le risque (ecart type) du portefeuille.:", mean_portfolio, "\n")


#                    QUestion 10 Calculer la contribution de chaque actif au risque de portefeuille

cov_matrix <- cov(returns)  # Matrice de covariance
portfolio_std_dev <- sqrt(t(weights) %*% cov_matrix %*% weights)  # Écart-type du portefeuille
# Contribution au risque
beta_AXA <- weights[1] * cov_matrix[1, ] %*% weights / portfolio_std_dev
beta_ENGIE <- weights[2] * cov_matrix[2, ] %*% weights / portfolio_std_dev
contribution_AXA <- beta_AXA * portfolio_std_dev
contribution_ENGIE <- beta_ENGIE * portfolio_std_dev

#                Question 11 calculer la VAR er ES du portefeuille en utilisant 
#la methode de Cornish fischer. Commenter les résultats. Quelle méthode conseilleriez vous?

#Fusionner les rendements pour créer un portefeuille
VaR_CF <- VaR(portfolio_mean_return, p = 0.95, method = "modified")
ES_CF <- ES(portfolio_mean_return, p = 0.95, method = "modified")

# Résultats finaux
cat("Rendement moyen du portefeuille :", mean_portfolio, "\n")
cat("Risque (écart-type) du portefeuille :", portfolio_std_dev, "\n")
cat("Contribution au risque (AXA) :", contribution_AXA, "\n")
cat("Contribution au risque (ENGIE) :", contribution_ENGIE, "\n")
cat("VaR (Cornish-Fisher) :", VaR_CF, "\n")
cat("ES (Cornish-Fisher) :", ES_CF, "\n")

# interprétation des résultats 
#Je conseillerai la méthode de Cornish Fisher à mon institution car elle parait 
#plus capter les risques extrèmes en proposant des VAR et de ES plus importants, 
#La réglementation BALE 3 sécurise le risque systémique en régulant le capital 
#des banques au statut d'institution en multipliant leur ES. 
# Prendre l'estimation qui donne la pire situation garantit une réserve efficace 
#pour prévenir la faillite de la banque. 


