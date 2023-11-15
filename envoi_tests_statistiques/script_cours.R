
# ATELIER TESTS STATISTIQUES AVEC R ---------------------------------------


## Chapitre 3 : Tester la normalite et l'homogeneite des variances ---------


### Le test de normalite ----------------------------------------------------

data(iris)
with(iris, tapply(Petal.Length, Species, shapiro.test))

# On charge la bibliotheque tidyverse dans 
# laquelle se trouve le package ggplot2
library(tidyverse)

ggplot(iris, aes(sample=Petal.Length, color=Species)) + 
  stat_qq() + stat_qq_line() + 
  facet_wrap(~ Species) + theme_classic()

# Autre maniere en utilisant le package ggpubr
library(ggpubr)
ggqqplot(iris, x = "Petal.Length", color="Species", 
         palette=c("blue","green","red")) +
  facet_wrap(~ Species)


library(nortest)
with(iris, tapply(Petal.Length, Species, ad.test))

library(fBasics)
with(iris, tapply(Petal.Length, Species, dagoTest))


### L'homogeneite des variances ---------------------------------------------


# On charge le package car pour effectuer le test de Levene
library(car)
leveneTest(extra ~ group, data=sleep)

bartlett.test(extra ~ group, data=sleep)

data1 <- sleep$extra[which(sleep$group==1)]
data2 <- sleep$extra[which(sleep$group==2)]
var.test(data1,data2)

var.test(extra ~ group, data=sleep)

## Chapitre 4 : Les tests parametriques ------------------------------------


### Comparaison de moyennes -------------------------------------------------

#### Comparaison de moyennes de deux echantillons independants : le test de Student et le test de Welch

indice <- which(iris$Species=="virginica" | iris$Species=="versicolor")
datairis <- iris[indice,]
datairis$Species <- factor(datairis$Species,exclude=NULL)

ggplot(datairis, aes(x=Species, y=Petal.Length)) + 
  geom_boxplot(fill=c("lightblue","pink"), outlier.colour="red") + 
  xlab("Espece") + ylab("Longueur des petales") + theme_classic()


#verification de la normalite
with(datairis, tapply(Petal.Length, Species, shapiro.test))
ggplot(datairis, aes(sample=Petal.Length, color=Species)) + 
  stat_qq() + stat_qq_line() + 
  facet_wrap(~ Species) + theme_classic()

# Homogeneite des variances
bartlett.test(Petal.Length~Species, data = datairis)
var.test(Petal.Length~Species, data = datairis)

t.test(Petal.Length~Species, data=datairis, 
       alternative="less", paired=FALSE, var.equal=TRUE)

ggplot(datairis, aes(x=Species, y=Petal.Length)) + 
  geom_boxplot(fill=c("lightblue","pink"), outlier.colour="red") + 
  xlab("Espece") + ylab("Longueur des petales") +
  geom_segment(x=0.9, xend=1.7, y=6.5, yend=6.5, col="black") +
  geom_segment(x=0.9, xend=0.9, y=6.5, yend=6.3, col="black") +
  geom_segment(x=1.7, xend=1.7, y=6.5, yend=6.3, col="black") + 
  annotate("text", x = 1.3, y = 6.72, label = "test t de Student,
           ***") + theme_classic()

t.test(Petal.Length~Species, data=datairis, 
       alternative="less", paired=FALSE, var.equal=TRUE)


library(effectsize)
#Calcul de l'indice de Cohen en utilisant le package effsize
indice <-
  cohens_d(Petal.Length ~ Species, 
           data=datairis, paired=FALSE)$Cohens_d
library(pwr)
#Calcul de la puissance en utilisant le package pwr
(puissance <- pwr.t.test(n=50, d=indice, sig.level=0.05, 
                         type="two.sample", 
                         alternative="two.sided"))



pwr.t.test(d=0.3, sig.level=0.05, power=0.8,
           type="two.sample", 
           alternative="two.sided")


library(ggstatsplot)
ggbetweenstats(
  data = datairis,
  x = Species,
  y = Petal.Length,
  xlab="Espece",
  ylab="Longueur des petales",
  type="parametric",
  var.equal=TRUE,
  results.subtitle = TRUE,
  title = "Distribution de la longueur des petales selon l'espece"
)


#### Comparaison de moyennes de deux echantillons apparies : le test de Student apparie et le test de Welch apparie

library(MASS)
data(anorexia)

# Normalite
diff <- anorexia$Postwt-anorexia$Prewt
shapiro.test(diff)

t.test(anorexia$Postwt,anorexia$Prewt,paired = TRUE, alternative="two.sided")

#Calcul de l'indice de Cohen (package effectsize)
valeurs <- c(anorexia$Prewt,anorexia$Postwt)
groupes <- c(rep("Prewt",72),rep("Postwt",72))
anorex <- data.frame(groupes,valeurs)
indice <- hedges_g(valeurs~groupes, paired=TRUE, data=anorex)$Hedges_g

#Calcul de la puissance (package pwr)
(puissance <- pwr.t.test(n=72, d=indice,type="paired", 
                         alternative = "two.sided"))
plot(puissance)


### Comparaison de proportions d'echantillons independants ------------------


#### Comparaison de proportions observees ------------------

grippe<-matrix(c(34,50,23,61),2)
dimnames(grippe) <- list(c("homme","femme") ,c("malade","non malade"))
print(grippe)

prop.test(grippe, alternative="two.sided")
prop.test(c(34,50),c(57,111),correct=T)

# Calcul de l'effet taille h pour 2 proportions (package pwr)
h<-ES.h(0.60,0.45)
# Calcul de la puissance (package pwr)
(puissance <- pwr.2p2n.test(h, n1=57,n2=111, 
                            sig.level=0.05, 
                            alternative="two.sided"))
plot(puissance)


####  Le test d'independance : le test du $\chi_2$----------------------------

transfotitanic <- data.frame(Titanic)
# On charge le package reshape pour modifier les 
#donnees avec la fonction untable
library(reshape)
titanic <- data.frame(untable(transfotitanic[,c(1,2,3,4)],
                              num=transfotitanic[,5]))
# creation du tableau de contingence
tabletest <- table(titanic$Survived,titanic$Class)
(test <- chisq.test(tabletest))
test$observed
round(test$expected,0)

# Calcul de la taille
w <- ES.w2(prop.table(tabletest))
# Calcul de puissance
(puissance <- pwr.chisq.test(w, df=3, N=2201, 
                             sig.level=0.05))
plot(puissance)



## Chapitre 5 : Comparaison de plusieurs moyennes : l'ANOVA un facteur --------

# Verification de la normalite
with(PlantGrowth, tapply(weight,group,shapiro.test))
# Homogeneite des variances
leveneTest(weight ~ group, data=PlantGrowth)

# ANOVA


# calcul de puissance
# Calculer la taille de l'effet
library(effectsize)
f = effectsize(testanova, type="eta")
library(pwr)
pwr.anova.test(k=3,n=50,f=0.2, sig.level=0.05, power=NULL)

# Tests post hoc
pairwise.t.test(PlantGrowth$weight, PlantGrowth$group, 
                p.adjust.method = "bonferroni")
TukeyHSD(testanova)

comparaisons <- list( c("ctrl","trt1"),
                      c("trt1","trt2"), 
                      c("trt2", "ctrl"))
ggboxplot(PlantGrowth, x = "group", y = "weight", fill = "group",
          palette = "npg", ylab="Poids", xlab="Groupe")+
  stat_compare_means(comparisons = comparaisons, label = "p.signif")+ 
  stat_compare_means(label.y = 8, method="anova")  


ggbetweenstats(
  data = PlantGrowth,
  x = group,
  y = weight,
  pairwise.display="non-significant",
  p.adjust.method="BH",
  xlab="Condition de traitement",
  ylab="Poids des plantes"
)


## Chapitre 6 : tests non parametriques  -----------------------------------

### Comparaisons de medianes de deux echantillons independants : le test de Wilcoxon  --------

# Utilisation du package tidyverse (en particulier dplyr)
airquality$Month <- factor(airquality$Month, labels=month.name[5:9])

indice <- which(airquality$Month=="May"| airquality$Month=="September")
datawilcoxon <- airquality[indice,]

datawilcoxon$Month <- factor(datawilcoxon$Month, exclude = NULL)

datawilcoxon <- na.omit(datawilcoxon)

ggplot(datawilcoxon, aes(x=Month, y=Ozone)) + 
  geom_boxplot(fill=c("lightblue","pink"), outlier.colour="red") + 
  xlab("Mois") + ylab("Concentration moyenne d'ozone") 

# verification de la normalite si on effectue un test parametrique
with(datawilcoxon, tapply(Ozone, Month, shapiro.test))

# Densite
ggplot(datawilcoxon, aes(x=Ozone, fill=Month)) + 
  geom_density(alpha=.4) + xlab("Concentration moyenne d'ozone") + 
  ylab("Densite") + theme_bw()

wilcox.test(Ozone~Month, data=datawilcoxon)


### Comparaisons de medianes de deux echantillons apparies : le test des signes de Wilcoxon

poids <- c(100,90,98,101,103,105,89,90,90, 
           100,85,96,100,95, 100,85,90,85)
traitement <- as.factor(c(rep("T1",9), rep("T2",9)))
datasignewilcoxon <- data.frame(poids,traitement)

#Non normalite du poids de chacun des groupes
with(datasignewilcoxon, tapply(poids, traitement, shapiro.test))

dataT1 <- datasignewilcoxon[which(traitement=="T1"),] 
dataT2 <- datasignewilcoxon[which(traitement=="T2"),]
diff <- data.frame(diff=dataT1$poids-dataT2$poids)
ggplot(diff,aes(x=diff))+ geom_density(alpha=.2, fill="pink") + theme_bw()


library(DescTools)
SignTest(dataT1$poids,dataT2$poids)

wilcox.test(poids~traitement, data=datasignewilcoxon, paired=TRUE, exact=FALSE)


### Comparaisons de k echantillons independants: le test de Kruskal-Wallis-----

datakruskal <- airquality %>% 
  mutate(Month = factor(Month,labels = month.abb[5:9])) %>%
  drop_na()
datakruskal <- na.omit(datakruskal)

ggplot(datakruskal, aes(x=Month, y=Ozone)) + 
  geom_boxplot(fill=c("lightblue","pink","grey", "purple","green"), 
               outlier.colour="red") +
  xlab("Mois") + ylab("Concentration moyenne d'ozone") + theme_bw()

ggplot(datakruskal, aes(x=Ozone, fill=Month)) + 
  geom_density(alpha=.4) + xlab("Concentration moyenne d'ozone") + 
  ylab("Densit?") + theme_bw()

kruskal.test(Ozone~Month, data=datakruskal)


### le test exact binomial

binom.test(x=180, n=300, alternative="greater")

### Comparaison de proportions : Le test exact de Fisher

TeaTasting <-
  matrix(c(3, 1, 1, 3), nrow = 2, 
         dimnames = list(Guess = c("Milk", "Tea"),
                         Truth = c("Milk", "Tea")))
fisher.test(TeaTasting, alternative = "two.sided")

## Chapitre 7 : tests de permutation ---------------------------------------

library(coin)

oneway_test(Petal.Length~Species, data=datairis, 
            alternative="less", paired=FALSE, var.equal=TRUE,
            distribution = approximate(nresample = 10000))

wilcox_test(Ozone~Month, data = datawilcoxon, 
            distribution = approximate(10000))

kruskal_test(Ozone~Month, data=datakruskal, 
             distribution = approximate(nresample = 10000))


## Chapitre 8 : Tester l'association entre 2 variables quantitatives --------


### Le test de Pearson ------------------------------------------------------

ggplot(mtcars, aes(x = mpg, y = wt)) +
  geom_point(colour = "darkblue") +
  xlab("Consommation (Miles/gallon)") +
  ylab("Poids") +
  geom_smooth(method="lm") +
  theme_minimal()

shapiro.test(mtcars$mpg)

shapiro.test(mtcars$wt)

cor.test(mtcars$mpg, mtcars$wt, method="pearson", alternative = "two.sided")


### Tests non paramétriques : le test de Kendall et le test de Spearman ---------

ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(colour = "darkblue") +
  xlab("Longueur des petales") +
  ylab("Largeur des petales") +
  geom_smooth(method="lm") +
  theme_minimal()

shapiro.test(iris$Petal.Length)

shapiro.test(iris$Petal.Width)

cor.test(iris$Petal.Length, iris$Petal.Width, method="kendall", 
         alternative = "two.sided")

cor.test(iris$Petal.Length, iris$Petal.Width, method="spearman", 
         alternative = "two.sided")