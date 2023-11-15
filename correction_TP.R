#############################################################################
# Exercice 1
#############################################################################

data(InsectSprays)
summary(InsectSprays)

library(tidyverse)
ggplot(InsectSprays, aes(x=count, y=spray)) + 
  geom_boxplot(fill=c("blue","green","red","violet","brown","yellow"), 
               outlier.colour="red") + 
  xlab("Nombre d'insectes") + ylab("Traitement") 

with(InsectSprays, tapply(count,spray,shapiro.test))

# Autre manière
modele <- aov(count ~ spray, data=InsectSprays)
shapiro.test(modele$residuals)

# L'hypothese de normalite n'est pas respectee : faire un test non parametrique

library(ggpubr)
ggqqplot(InsectSprays, x = "count", color="spray", 
         palette=c("blue","green","red","violet","brown","yellow")) +
  ylab("Nombre d'insectes") + facet_wrap(~ spray, nrow= 1)


ggplot(InsectSprays, aes(x=count, fill=spray)) + 
  geom_density(alpha=.4) + xlab("Nombre d'insectes") + 
  ylab("Densite")

# On applique un test non parametrique
kruskal.test(count ~ spray, data=InsectSprays)
# Le nombre d'insectes varie en fonction du type d'insecticide (seuil de 5%)
# outliers C and D


#############################################################################
# Exercice 2
#############################################################################

data(iris)
# Normalite
with(iris, tapply(Sepal.Length,Species,shapiro.test))

# Pas homogeneite des variance
library(car)
leveneTest(Sepal.Length ~ Species, data=iris)


# ANOVA
(modeleiris <- oneway.test(Sepal.Length ~ Species, var.equal = FALSE, data=iris))

# Comparaison 2 à 2 
pairwise.t.test(iris$Sepal.Length,iris$Species,p.adjust.method = "BH", 
                pool.sd=FALSE)

# Graphique
library(ggstatsplot)
ggbetweenstats(
  data = iris,
  x = Species,
  y = Sepal.Length,
  var.equal=FALSE,
p.adjust.method="BH",
  xlab="Espèce",
  ylab="Longueur des sépales"
)

#############################################################################
# Exercice 3
#############################################################################

data(ToothGrowth)
# La longueur des dents augmente-t-elle si on donne du jus d'orange 
# aux cochons d'inde au risque de 5% ? 
summary(ToothGrowth)
?ToothGrowth

ggplot(ToothGrowth, aes(x=len,y=supp, fill=supp)) + 
  geom_boxplot() + xlab("Longueur des dents") + ylab("Complément")

with(ToothGrowth, tapply(len,supp,shapiro.test))
# Le test de normalite n'est pas respecte

ggplot(ToothGrowth, aes(x=len, fill=supp)) + 
  geom_density(alpha=.4) + xlab("Longueur de dents") + 
  ylab("Densite")

#Utiliser un test non parametrique
wilcox.test(len ~ supp, data=ToothGrowth, alternative="greater", exact=FALSE)


#############################################################################
# Exercice 4
#############################################################################

library(multcomp)
data(cholesterol)

?cholesterol
with(cholesterol, tapply(response,trt,shapiro.test))
# Normalite

leveneTest(response ~ trt, data=cholesterol)
# Homogeneite des variances

modelcholesterol <- aov(response ~ trt, data=cholesterol)

shapiro.test(modelcholesterol$residuals)

plot(modelcholesterol,1)
plot(modelcholesterol,2)

summary(modelcholesterol)
# Le traitement est significatif au seuil de 5%

TukeyHSD(modelcholesterol)

ggbetweenstats(
  data = cholesterol,
  x = trt,
  y = response,
  pairwise.display="significant",
  p.adjust.method="BH",
  xlab="Traitement",
  ylab="Reduction de cholesterol"
)

#############################################################################
# Exercice 5
#############################################################################

data(sleep)
?sleep
summary(sleep)
# Les donnees sont appariees

group1 <- sleep[sleep$group==1,]
group2 <- sleep[sleep$group==2,]
diff <- group1$extra-group2$extra
shapiro.test(diff)

ggplot(data.frame(diff),aes(x=diff))+ geom_density(alpha=.2, fill="pink")

# Appliquer un test de Wilcoxon
wilcox.test(group1$extra,group2$extra, alternative="two.sided", paired=TRUE)
# Conclusion : on rejette H0

#############################################################################
# Exercice 6
#############################################################################

data(trees)
shapiro.test(trees$Height)
shapiro.test(trees$Volume)

cor.test(trees$Height,trees$Volume,method="kendall")
