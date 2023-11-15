##########################################################################
# Chapitre 2 : Statistique descriptive univariee
##########################################################################

# Jeu de donnees iris
data(iris)
# La longueur des sépales (*Sepal.Length*) : variable quantitative
# La largeur des sépales (*Sepal.Width*) : variable quantitative
# La longueur des pétales (*Petal.Length*) : variable quantitative
# La largeur des pétales (*Petal.Width*) : variable quantitative
# L'espèce (*Species*) : variable qualitative

summary(iris)


library(ade4)
data(corvus)
head(corvus)

# Description des variables
# 28 especes de corbeaux ont ete mesurees. Il y a 4 variables :
# La variable wing : la longueur d'aile en mm (variable quantitative)
# La variable bill : la longueur du bec en mm (variable quantitative)
# La variable habitat : type d'habitat (ouvert ou ferme) (variable qualitative)
# La variable phylog : groupe phylogeographique (variable qualitative)

summary(corvus)
library(e1071)
skewness(corvus$wing)
skewness(corvus$bill)
kurtosis(corvus$wing)
kurtosis(corvus$bill)

##############################################################################
# Chapitre 3 : Représentations graphiques et statistique descriptive univariée
##############################################################################

# Representations graphiques

# Les variables Time et Chick ChickWeight

data(ChickWeight)
library(tidyverse)

ggplot(data=ChickWeight, aes(Time)) +
  geom_histogram(color="black", fill="lightblue", binwidth=10) +
  xlab("Nombre de jours") + ylab("Effectifs") + theme_bw()
ggplot(data=ChickWeight, aes(y=Time)) +
  geom_boxplot(fill="lightblue", outlier.colour="red") +
  ylab("Nombre de jours") + theme_bw()


ggplot(data=ChickWeight, aes(x=Chick)) +
  geom_point(stat="count", pch=15, col="blue") +
  xlab("ID") + ylab("Effectifs") + coord_flip()

# Les variables habitat et phylog de corvus

# Diagramme en baton

ggplot(data=corvus, aes(x=habitat)) +
  geom_bar(fill="lightblue") + xlab("Type d'habitat") +
  ylab("Effectifs") + theme_bw()

ggplot(data=corvus, aes(x=phylog)) +
  geom_bar(fill="lightblue") + xlab("Groupe phylogeographique") +
  ylab("Effectifs") + theme_bw()


datapie <- corvus %>%
  count(habitat) %>%
  mutate(prop = prop.table(n))%>%
  mutate(pourc=prop*100)%>%
arrange(desc(habitat)) %>%
  mutate(labpos = cumsum(pourc) - 0.5*pourc)

datapie2 <- corvus %>%
  count(phylog) %>%
  mutate(prop = prop.table(n))%>%
  mutate(pourc=prop*100)%>%
  arrange(desc(phylog)) %>%
  mutate(labpos = cumsum(pourc) - 0.5*pourc)

ggplot(data = datapie, aes(x = "", y = pourc, fill = habitat)) +
  geom_bar(stat = "identity") + coord_polar("y", start = 0) +
  labs(fill="Type d'habitat", x=NULL, y=NULL) +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(), axis.text.x=element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill="white")) +
  geom_text(aes(y = labpos,label = paste(round(pourc,2),"%"))) 


ggplot(data = datapie2, aes(x = "", y = pourc, fill = phylog)) +
  geom_bar(stat = "identity") + coord_polar("y", start = 0) +
  labs(fill="Groupe phylogeographique", x=NULL, y=NULL) +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(), axis.text.x=element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill="white")) +
  geom_text(aes(y = labpos,label = paste(round(pourc,2),"%"))) 

# Représenter chacune des variables des données iris

# La variable Sepal.Length
# Courbe de densité (noyau gaussien)
dens <- density(iris$Sepal.Length, bw=1)
plot(dens, xlab="Longueur des sépales", main="", col="red", xlim=c(4,8))
# Boxplot
ggplot(data=iris, aes(y=Sepal.Length)) +
  geom_boxplot(fill="lightblue", outlier.colour="red") +
  ylab("Longueur des sépales") + theme_classic()

# La variable Sepal.Width
dens <- density(iris$Sepal.Width, bw=1)
plot(dens, xlab="Largeur des sépales", main="", col="red", xlim=c(2,4))
# Boxplot
ggplot(data=iris, aes(y=Sepal.Width)) +
  geom_boxplot(fill="lightblue", outlier.colour="red") +
  ylab("Largeur des sépales") + theme_classic()

# La variable Petal.Length
dens <- density(iris$Petal.Length, bw=1)
plot(dens, xlab="Longueur des pétales", main="", col="red", xlim=c(1,7))
# Boxplot
ggplot(data=iris, aes(y=Petal.Length)) +
  geom_boxplot(fill="lightblue", outlier.colour="red") +
  ylab("Longueur des pétales") + theme_classic()

# La variable Petal.Width
dens <- density(iris$Petal.Width, bw=0.1)
plot(dens, xlab="Largeur des pétales", main="", col="red", xlim=c(0.1,2.5))
# Boxplot
ggplot(data=iris, aes(y=Petal.Width)) +
  geom_boxplot(fill="lightblue", outlier.colour="red") +
  ylab("Largeur des pétales") + theme_classic()

# La variable Species
datapie <- iris %>% count(Species) %>% 
  mutate(prop = prop.table(n)) %>% 
  mutate(pourc=round(prop*100,2)) %>% 
  arrange(desc(Species)) %>% 
  mutate(labpos = c(15,50,90))
ggplot(data = datapie, aes(x = "", y = pourc, fill = Species)) +
  geom_bar(stat = "identity") + coord_polar("y", start = 0) +
  labs(fill="Type de régime", x=NULL, y=NULL)+
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x=element_blank(),
        panel.background = element_rect(fill="white")) + 
  geom_text(aes(y = labpos,label = paste("n=", n)))


##############################################################################
# Chapitre 4 : Résumé statistique à l’aide de packages R
##############################################################################

library(DescTools)
print(Desc(corvus))

library(summarytools)
descr(corvus)

##############################################################################
# Chapitre 5 : Statistiques descriptives bivariées
##############################################################################

data(banque)
table(banque$csp,banque$age)
ggplot(data=banque, aes(x=csp, fill=age)) +
  geom_bar(width=0.7) + ylab("Effectif") +
  xlab("categorie socio-professionnelle") + coord_flip() + theme_bw()

data(InsectSprays)
# Boxplot
ggplot(data=InsectSprays, aes(x=spray, y=count)) +
  geom_boxplot(fill="lightblue", outlier.colour="red") +
  xlab("Type d'insecticide") +
  ylab("") + theme_bw()

ggplot(InsectSprays, aes(x=count, fill=spray)) +
  geom_density(alpha=.4) + xlab("Effectif") +
  ylab("Densité") + theme_bw()

# Diagramme de Cleveland
ggplot(InsectSprays, aes(x = count, y = spray)) +
  geom_point(pch = 15, aes(color = spray)) +
  ylab("Type d'insecticide") + xlab("Effectif") + theme_bw()

##############################################################################
# Chapitre 6 : Pour aller plus loin : l’analyse en composantes principales
##############################################################################

library(ade4)
library(FactoMineR)
library(factoextra)

data(decathlon)

# Utilisation du package ade4
datadecathlon <- decathlon[,1:10]

cor(datadecathlon)
plot(datadecathlon)

# Réalisation de l'ACP avec le package ade4
ACP <- dudi.pca(datadecathlon, center=TRUE, scale=TRUE, scannf=FALSE)

# Valeurs propres (variance expliquée ou inertie)
ACP$eig
# Variance expliquée (en %)
(ACP$eig/sum(ACP$eig))*100
# Somme cumulée de la variance expliquée (en %)
cumsum((ACP$eig/sum(ACP$eig))*100)
# Eboulis des valeurs propres
fviz_eig(ACP, addlabels = TRUE, 
         main="Eboulis des valeurs propres", xlab="Axes", 
         ylab="Variance expliquée (%)")

# Contribution des variables
inertia.dudi(ACP, col.inertia = T)$col.abs
# Qualité de représentation des variables
inertia.dudi(ACP, col.inertia = T)$col.rel

# Coordonnées des variables
ACP$co

s.corcircle(ACP$co, xax=1, yax=2)

# Contribution des individus
inertia.dudi(ACP, row.inertia = T)$row.abs
# Qualité de représentation des variables
inertia.dudi(ACP, row.inertia = T)$row.rel
# Coordonnées des variables
ACP$li

# Représentation des individus (composantes 1 et 2)
s.label(ACP$li, xax=1, yax=2)

# Représentation simultanée des individus et des 
# variables (composantes 1 et 2)
fviz_pca_biplot(ACP, title="")

# Utilisation du package FactoMineR
ACP2 <- PCA(decathlon, quali.sup=13, graph=F)
summary(ACP2)

# Représentation des individus
plot.PCA(ACP2, axes=c(1,2),choix="ind",  habillage=13, cex=0.7, title="")
fviz_pca_ind(ACP2, title="",
             habillage=13, 
             addEllipses=TRUE,
             ellipse.level=c(0.5))

# Représentation des variables
plot.PCA(ACP2, axes=c(1,2),choix="var", title="")

# Représentation simultanée des individus et des variables (composantes 1 et 2)
fviz_pca_biplot(ACP2, title="",label="var", 
                habillage=decathlon$Competition, 
                addEllipses=TRUE, ellipse.level=c(0.7),
                ellipse.alpha = 0.3, 
                palette = c("red", "orange"))


