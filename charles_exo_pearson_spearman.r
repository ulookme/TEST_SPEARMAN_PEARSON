require("Amelia") 
require("caret")
require(graphics)
require(dgof)
library(ggplot2)
library(dplyr)
#Importer la base de données
#Source : http://dimension.usherbrooke.ca/voute/libertes2012.csv
#2. Supprimer les pays dont la population est inférieure à 1M d’habitants

#3. Sélectionner les variables utiles à cette analyse
#4. Supprimer les pays pour lesquels il y a des valeurs manquantes
#5. Faire un test de Pearson sur deux variables à la fois
#6. Calculer la matrice de corrélation de toutes les variables
#7. Effectuer un test de corrélation sur l’ensemble des variables en une seule fois
#a. En utilisant le test de Pearson
#b. En utilisant le test Spearman
#8. Réaliser une visualisation de la corrélation des variables.
#Utiliser la fonction corrplot

base=read.csv2("http://dimension.usherbrooke.ca/voute/libertes2012.csv")
#on peut transformer la base de donné des le debart en numérique si c'est souhaite avec ->
#base[sapply(base, is.numeric)]
head(base)
dim(base)
names(base)
str(base)
n = nrow(base)
n
# Compter le nombre de valeurs manquantes dans chacune des colonnes
sapply(base, function(base) sum(is.na(base)))
sapply(base, function(base) (sum(is.na(base))/n)*100)
# Dans ce cas la part de valeurs manquantes est négligeable donc je décide de les supprimer

base <- na.omit(base)
n1 <- nrow(base)
n - n1 # nombre de valeurs manquantes supprimées
head(base)
x<-base$Corruption
y<-base$Financiere
r<-cor(x,y, method = "pearson")
cor.test(x,y, method = c("pearson"))
# il ya pas de corrélation entre c'est deux donné ou elle est tres faibre a verivier avec le graphique nuage de point
#ci-dessous.
ggplot(base, aes(x=x, y=y)) + geom_point()
# on voie effectivement que celle si ne sont pas cooré ou le sont d'un facon lineaire

#Calculer la matrice de corrélation de toutes les variables
# transformer la base en matrix rcorr(as.matrix(base), type=c("pearson"))

mcor <- cor(as.matrix(base[,0:-2]), method = c("pearson", "kendall", "spearman"))
mpearson <- cor(as.matrix(base[,0:-2]), method = "pearson")
mpearson
mkendall <- cor(as.matrix(base[,0:-2]), method = "kendall")
mkendall
mspearman <- cor(as.matrix(base[,0:-2]), method = "spearman")
mspearman
# Visualisation des corrÈlations
library(corrplot)
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)
corrplot(mpearson, type="upper", order="hclust", tl.col="black", tl.srt=45)
corrplot(mspearman, type="upper", order="hclust", tl.col="black", tl.srt=45)

corrplot(mkendall, type="upper", order="hclust", tl.col="black", tl.srt=45)

library(PerformanceAnalytics)
chart.Correlation(mcor, histogram=TRUE, pch=19)
