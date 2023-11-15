# CHAPITRE 1 : Quelques rappels et description des jeux de donnees  -------

data("ChickWeight")
head(ChickWeight)
dim(ChickWeight)
View(ChickWeight)
help("corvus",package="ade4")
head(corvus)
dim(corvus)
View(corvus)

data(iris)
help("iris")
dim(iris)
View(iris)


# CHAPITRE 2 : Statistique descriptive univariee --------------------------

## Cas d'une variable quantitative --------------------------------

serie <- c(4, 5, 7, 34, 6, 2, 1, 1, 8, 8, 8, 8, 8, 4, 5, 4, 2)

length(serie)

### effectifs de chaque valeur --------------------------------
tableau <- table(serie)
tableau
print(tableau)

### effectifs cumules --------------------------------
cumsum(serie)

### tableau des frequences de chacune des valeurs --------------------------------
prop.table(serie)

### Moyenne --------------------------------
mean(serie)

### Mediane --------------------------------
median(serie)

### Mode --------------------------------
max(tableau)
which(tableau==max(tableau))
mode <- names(which(tableau==max(tableau)))
print(mode)

### Maximum --------------------------------
max(serie)

### Minimum --------------------------------
min(serie)

### Variance --------------------------------
var(serie)

### Ecart type --------------------------------
sd(serie)

### Quantile --------------------------------
quantile(serie)

### La fonction range() donne le minimum et le maximum de la serie --------------------------------
range(serie)
etendue <- diff(range(serie))
print(etendue)

### Ecart interquartile --------------------------------
IQR(serie)

### Coefficient de variation --------------------------------
sd(serie)/mean(serie)

### Resume statistique --------------------------------
summary(serie)

### Indices de forme --------------------------------

library(e1071)

### coefficient d'asymetrie --------------------------------
skewness(serie)

### coefficient d'aplatissement --------------------------------
kurtosis(serie)


## Cas d'une variable qualitative --------------------------------

couleur <- c("bleu", "rouge", "rouge","rouge", "vert","vert","bleu",
             "rouge")
couleur <- as.factor(couleur)

summary(couleur)

View(ChickWeight)

summary(ChickWeight)

# CHAPITRE 3 : Representations graphiques et statistique descriptive univariee --------

## Cas d'une variable quantitative --------------------------------

### Visualiser une distribution --------------------------------

#### Histogramme et effectifs --------------------------------
hist(ChickWeight$weight, xlab="Poids du poussin (g)", ylab="effectifs", 
     col="lightblue", main="Histogramme du poids des poussins", freq=T, 
     xlim=c(25,300))
hist(ChickWeight$weight, xlab="Poids du poussin (g)", ylab="effectifs", 
     col="lightblue", main="Histogramme du poids des poussins", freq=T, 
     breaks=seq(1,400,10), xlim=c(25,300))

#### Courbe de densite (noyau gaussien) --------------------------------
dens <- density(ChickWeight$weight, bw=10) 
plot(dens, xlab="Poids du poussin (g)", main="", col="red", xlim=c(25,300)) 

#### La boite à moustaches --------------------------------
boxplot(ChickWeight$weight, 
        main="Boxplot de la variable poids", col="lightblue")
boxplot(ChickWeight$weight, 
        main="Boxplot moustache de la variable poids", col="lightblue", 
        notch=TRUE)
boxplot.stats(ChickWeight$weight)$out

#### Diagramme quantile-quantile normal --------------------------------
qqnorm(ChickWeight$weight, pch=18)
qqline(ChickWeight$weight, col="red")

### Visualiser une dispersion --------------------------------

#### Strip chart --------------------------------

stripchart(ChickWeight$weight, pch=15, cex=0.3, col="blue", method="stack")
stripchart(ChickWeight$weight, pch=15, cex=0.3, col="blue", method="jitter")

#### Diagramme tige-feuilles (histogramme de Tukey) --------------------------------
stem(ChickWeight$weight, scale=3)

## Cas d'une variable qualitative --------------------------------

### Visualiser une quantite --------------------------------

#### Diagramme en baton --------------------------------

tableau_effec <- table(ChickWeight$Diet)
plot(tableau_effec, ylab="Effectif")

library(lattice)
barchart(xtabs( ~ Diet,data=ChickWeight), col="lightblue", 
         ylab="Type de regime", xlab="Effectif")

barplot(xtabs( ~ Diet,data=ChickWeight), col="lightblue", 
        ylab="Effectif", xlab="Type de regime")

#### Diagramme de Cleveland --------------------------------

dotchart(as.matrix(table(sort(ChickWeight$Diet)))[,1], col="blue", pch=15, 
         xlab="Effectif", ylab="Type de regime")

# Le diagramme de Cleveland avec l'utilisation de la fonction 
# dotplot() du package lattice
dotplot(xtabs( ~ Diet,data=ChickWeight), col="blue", 
        ylab="Type de regime", xlab="Effectif")

### Visualiser une proportion --------------------------------

#### Diagramme circulaire --------------------------------

pie(tableau_effec)

# Ajouter les pourcentages 
(pourcentage <- (tableau_effec/length(ChickWeight$Diet))*100)
# Arrondir
(pourcentage <- round(pourcentage,2))
# Ajouter le symbole %
(pourcentage <- paste(pourcentage, "%"))

pie(tableau_effec, labels=pourcentage)

library(RColorBrewer)
# display.brewer.all() pour voir les palettes
pie(tableau_effec, labels=pourcentage, col=brewer.pal(n = 4, name = "Dark2"))

legend("top", levels(ChickWeight$Diet), inset = c(0.1, 0.95), 
       x.intersp=0.3, y.intersp=0.7, cex=0.9, horiz=TRUE, xpd = TRUE, 
       title="Type de regime", fill=brewer.pal(n = 4, name = "Dark2"))

## Utilisation du package tidyverse (package ggplot2) --------------------------------

library(tidyverse)

### Cas d'une variable quantitative --------------------------------

# Histogramme 
ggplot(data=ChickWeight, aes(weight)) + 
  geom_histogram(color="black", fill="lightblue", binwidth=10) + 
  xlim(25,300) + xlab("Poids du poussin (g)") + ylab("Effectifs") + 
  ggtitle('Histogramme du poids des poussins') + theme_classic()

# Courbe de distribution
ggplot(data=ChickWeight, aes(weight)) + 
  geom_density(alpha=.3, color="black", fill="lightblue") + 
  xlim(25,300) +
  xlab("Poids du poussin (g)") +  ylab("Densité") + 
  ggtitle('Histogramme du poids des poussins') + theme_classic()

# Boxplot
ggplot(data=ChickWeight, aes(y=weight)) + 
  geom_boxplot(fill="lightblue", outlier.colour="red") +
  ylab("Poids des poussins (g)") + theme_classic()

# Graphique quantile-quantile
ggplot(ChickWeight, aes(sample = weight)) + stat_qq() + 
  stat_qq_line(col="red") + theme_classic()


### Cas d'une variable qualitative --------------------------------

#### Diagramme en baton --------------------------------

ggplot(data=ChickWeight, aes(x=Diet)) + 
  geom_bar(fill="lightblue") + xlab("Type de regime") + ylab("Effectifs") + 
  theme_classic()

#### Diagramme de Cleveland --------------------------------

ggplot(data=ChickWeight, aes(x=Diet)) + 
  geom_point(stat="count", pch=15, col="blue") + 
  xlab("Type de regime") + ylab("Effectifs") + coord_flip() + theme_classic()

#### Diagramme circulaire --------------------------------

datapie <- ChickWeight %>% count(Diet)
datapie <- datapie %>% mutate(prop = prop.table(n)) %>% mutate(pourc=prop*100)
datapie <- datapie %>% arrange(desc(Diet))

# On trace le diagramme circulaire
ggplot(data = datapie, aes(x = "", y = pourc, fill = Diet)) + 
  geom_bar(stat = "identity") + coord_polar("y", start = 0) + 
  labs(fill="Type de regime",  x=NULL, y=NULL) 

# On enleve les elements en gris
ggplot(data = datapie, aes(x = "", y = pourc, fill = Diet)) + 
  geom_bar(stat = "identity") + coord_polar("y", start = 0) + 
  labs(fill="Type de regime",  x=NULL, y=NULL) + 
  theme(panel.grid = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text.x=element_blank(), 
        panel.background = element_rect(fill="white")) 

# On cree la colonne labpos qui correspond à la position de chaque pourcentage 
datapie <- datapie %>% mutate(labpos = c(10,31,52,81))

ggplot(data = datapie, aes(x = "", y = pourc, fill = Diet)) + 
  geom_bar(stat = "identity") + coord_polar("y", start = 0) + 
  scale_fill_brewer(palette = "Dark2") + 
  labs(fill="Type de regime",  x=NULL, y=NULL) + 
  theme(panel.grid = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text.x=element_blank(), 
        panel.background = element_rect(fill="white")) + 
  geom_text(aes(y = labpos,label = paste(round(pourc,2),"%"))) 


# CHAPITRE 4 : Resume statistique à l'aide de packages R ------------------

library(DescTools)
print(Desc(ChickWeight))

library(summarytools)
descr(ChickWeight)
view(dfSummary(ChickWeight))


# CHAPITRE 5 : Statistiques descriptives bivariees ------------------------

## Cas de deux variables qualitatives --------------------------------

# Reprendre le jeu de donnees corvus

# Tableau de contingence
contingence <- table(corvus$habitat, corvus$phylog)
# Somme des lignes
margin.table(contingence,1)
# Somme des colonnes
margin.table(contingence,2)
# Somme totale
margin.table(contingence)
addmargins(contingence)
# Distribution conditionnelle
prop.table(contingence)

# Tableau de contingence_bad
contingence <- table(corvus$habitat, corvus$phylog)
barplot(contingence, col=c("pink","lightblue"), beside = F, las=1, 
        cex.axis=0.7, xlab="Phylogeographic stock")
legend("topleft", legend=row.names(contingence), title="Habitat", 
       fill=c("pink", "lightblue"))

# Tableau de contingence
contingence <- table(corvus$habitat, corvus$phylog)
barplot(contingence, col=c("pink","lightblue"), beside = T, las=1, 
        cex.axis=0.7, xlab="Phylogeographic stock")
legend("topleft", legend=row.names(contingence), title="Habitat", 
       fill=c("pink", "lightblue"))

## Cas d'une variable qualitative et d'une variable quantitative --------------------------------

tapply(corvus$wing , corvus$habitat , mean)
tapply(corvus$wing , corvus$habitat , median)
tapply(corvus$wing , corvus$habitat , sd)

### Visualisation de distributions --------------------------------

#### Boxplot --------------------------------

boxplot(corvus$wing ~ corvus$habitat, col="lightblue", 
        xlab="Type d'habitat", ylab="Longueur de l'aile (mm)")

#### Histogrammes --------------------------------

histogram( ~ corvus$wing | corvus$habitat, type="density",
           xlab="Longueur de l'aile (mm)", ylab="", col="lightblue")

#### Courbes de densite ----NICE------------------------

# Densite de la longueur d'aile en fonction de l'habitat
densityplot( ~ corvus$wing, groups=corvus$habitat, type="density",
             xlab="Longueur de l'aile (mm)", ylab="Densit?", auto.key=TRUE)

### Visualisation de quantites --------------------------------

couleur <- c("darkred", "darkblue")
# Reordonner les donnees selon la variable wing
corvus2 <- corvus[order(corvus$wing), ]
dotchart(corvus2$wing, labels = row.names(corvus2), 
         gcolor = couleur, color = couleur[corvus2$habitat],
         groups = corvus2$habitat, cex=0.6, pch = 15, 
         xlab = "Longueur d'aile (mm)")

## Cas de deux variables quantitatives --------------------------------

### Le nuage de points --------------------------------

plot(iris$Sepal.Length,iris$Petal.Length, xlab="Longueur des sepales",
     ylab="Longueur des petales", pch=20, col="blue")

couleur <- brewer.pal(n = 3, name = "Dark2")
plot(iris$Sepal.Length,iris$Petal.Length, pch=20, 
     xlab="Longueur des sepales", ylab="Longueur des petales", 
     col=couleur[iris$Species])
legend("top", legend=levels(iris$Species), pch = 20, col=couleur, 
       cex=0.8, horiz = T, x.intersp = 0.5, y.intersp = 0.01)

cor(ChickWeight$Time, ChickWeight$weight)

## Quelques graphiques avec le package tidyverse (package ggplot2) --------------------------------

### Cas de deux variables qualitatives --------------------------------

# Mosaic plot
library(ggmosaic)
ggplot(corvus)+
  geom_mosaic(aes(x=product(habitat,phylog), fill=habitat))+
  xlab("Phylogeographic stock") + ylab("") + theme_classic()

# Diagramme en barre
ggplot(data=corvus, aes(x=habitat, fill=phylog)) + 
  geom_bar(width=0.7, alpha=0.5) + ylab("Effectif") + 
  xlab("Habitat")  + ylab("") + coord_flip() + 
  scale_fill_discrete("Phylogeographic stock") + 
  theme_classic()


### Cas d'une variable qualitative et d'une variable quantitative --------------------------------

# Boxplot
ggplot(data=corvus, aes(x=habitat, y=wing)) + 
  geom_boxplot(fill="lightblue", outlier.colour="red") + 
  xlab("Habitat") + ylab("Longueur d'aile (mm)") + 
  theme_classic()

# Violin plot
ggplot(data=corvus, aes(x=habitat, y=wing)) + 
  geom_violin(fill="lightblue") +
  geom_boxplot(width=0.2, alpha=0.4) +
  ylab("Longueur d'aile (mm)") +
  guides(fill="none") +
  theme_classic()

# Densit?
ggplot(corvus, aes(x=wing, fill=habitat)) + 
  geom_density(alpha=.4) + 
  xlab("Longueur d'aile (mm)") + ylab("Densite") + xlim(200,500) + 
  theme_classic()

# Diagramme de Cleveland
ggplot(corvus2, aes(x = wing, y = rownames(corvus2), habitat)) + 
  xlab("Longueur d'aile (mm)") +
  geom_point(pch = 15, aes(color = habitat)) + 
  ylab("Espece de corbeau")  + theme_classic()

### Cas de plusieurs variables quantitatives --------------------------------

#### Nuage de points --------------------------------

ggplot(data=iris, aes(x=Sepal.Length, y=Petal.Length)) + 
  geom_point(pch=20, col="blue") + 
  xlab("Longueur des sepales") + 
  ylab("Longueur des petales") +
  theme_classic()

#### Bubble graph --------------------------------

ggplot(data=iris, aes(x=Sepal.Length, y=Petal.Length)) +
  geom_point(alpha=0.5, aes(size=Petal.Width, color=Species)) +
  xlab("Longueur des sepales") + ylab("Longueur des petales") +
  scale_size(name="Largeur des petales", range = c(1, 7)) + 
  scale_color_discrete(name="Espece") +
  theme_bw() 


#  Zoom sur le package plotly -----------------------------------------


## Diagramme en barre --------------------------------
bar_graph <- corvus %>% 
  count(habitat, phylog) %>%
  plot_ly(y = ~habitat, x = ~n, color = ~phylog) %>% 
  add_bars() %>% layout(barmode = "stack", xaxis=list(title="Effectif"), 
                        yaxis=list(title="Habitat"))

bar_graph

## Boxplot et violin plot --------------------------------
# Boxplot
box_graph <- plot_ly(corvus, y = ~wing, color = ~habitat, type = "box") 
# Violin plot
violin_graph <- plot_ly(corvus, x = ~habitat, y = ~wing, color= ~habitat, 
                        type = 'violin', box = list(visible = T))

box_graph

## Histogramme --------------------------------
histo_graph <- plot_ly(corvus, x = ~wing, color = ~habitat) %>%
  add_histogram()

histo_graph

## Densité --------------------------------

densite1 <- density(corvus$wing[corvus$habitat=="clos"])
densite2 <- density(corvus$wing[corvus$habitat=="open"])
densite_graph <- plot_ly(x = ~densite1$x, y = ~densite1$y, type = 'scatter', 
                         mode = 'lines', fill = 'tozeroy', name="clos") %>% 
  add_trace(x = ~densite2$x, y = ~densite2$y, fill = 'tozeroy', name="open") %>% 
  layout(xaxis = list(title = 'Wing'), yaxis = list(title = 'Densité'))

densite_graph

## Bubble graph --------------------------------

bubble <- plot_ly(iris, x = ~Sepal.Length, y = ~Petal.Length, type = 'scatter', 
                  mode = 'markers', size = ~Petal.Width, color = ~Species, 
                  colors = 'Paired', marker = list(opacity = 0.7))

bubble

# CHAPITRE 6 : Pour aller plus loin : l'analyse en composantes principales --------


library(ade4)
library(FactoMineR)
library(factoextra)

## Les jeux de donnees --------------------------------

### Le jeu de donnees doubs --------------------------------
data(doubs, package="ade4")
datadoubs <- doubs$env[,5:11]

# Le jeu de donnees tortues du package ade4 --------------------------------
data(tortues, package = "ade4")

## Premiere application : le jeu de donnees datadoubs --------------------------------

### Le package ade4 --------------------------------

cor(datadoubs)
plot(datadoubs)

ACP <- dudi.pca(datadoubs, center=TRUE, scale=TRUE, scannf=FALSE)

# Valeurs propres (variance expliquee ou inertie)
ACP$eig
# Variance expliquee (en %)
(ACP$eig/sum(ACP$eig))*100
# Somme cumulee de la variance expliquee (en %)
cumsum((ACP$eig/sum(ACP$eig))*100)
# Eboulis des valeurs propres
screeplot(ACP, main="Eboulis des valeurs propres")
# Autre solution
barplot((ACP$eig/sum(ACP$eig))*100, 
        names.arg=as.character(c(1:7)), col="lightblue", 
        xlab="Axes", ylab="Variance expliquée (%)")
lines(x=c(1:7), y=(ACP$eig/sum(ACP$eig))*100, col="red")
# Autre solution avec le package factoextra
fviz_eig(ACP, addlabels = TRUE, 
         main="Eboulis des valeurs propres", xlab="Axes", 
         ylab="Variance expliqu?e (%)")

#### Representation graphique des variables  --------------------------------

# Contribution des variables
inertia.dudi(ACP, col.inertia = T)$col.abs
# Contribution relative des variables (qualite de representation)
inertia.dudi(ACP, col.inertia = T)$col.rel
# Coordonnees des variables
ACP$co

s.corcircle(ACP$co, xax=1, yax=2)
fviz_pca_var(ACP, col.var = "cos2", 
             gradient.cols=c("purple","red","green")) + 
  labs(title="") 
fviz_pca_var(ACP, col.var = "contrib", 
             gradient.cols=c("purple","red","green")) + 
  labs(title="") 

#### Representation graphique des individus --------------------------------

# Contribution des individus
inertia.dudi(ACP, row.inertia = T)$row.contrib
# Qualite de representation des individus
inertia.dudi(ACP, row.inertia = T)$row.rel
# Coordonnees des individus
ACP$li
# Representation des individus (composantes 1 et 2)
s.label(ACP$li, xax=1, yax=2)
fviz_pca_ind(ACP, pointsize="cos2", pointshape=21, fill="red") + 
  labs(title="", x="Axe 1", y="Axe 2") 
fviz_pca_ind(ACP, pointsize="contrib", pointshape=21, fill="red") + 
  labs(title="", x="Axe 1", y="Axe 2") 

#### Representation simultanee des individus et des variables --------------------------------

# Representation simultanee des individus et des 
# variables (composantes 1 et 2)
fviz_pca_biplot(ACP, title="")


### Le package FactoMineR--------------------------------

ACP2 <- PCA(datadoubs , graph=F, scale.unit = TRUE)
summary(ACP2)
# Representation des individus
plot.PCA(ACP2, axes=c(1,2),choix="ind", title="")
fviz_pca_ind(ACP2, col.ind="contrib") + 
  labs(title="", x="Axe 1", y="Axe 2") 
# Representation des variables
plot.PCA(ACP2, axes=c(1,2),choix="var")
fviz_pca_var(ACP2, col.var = "contrib") + 
  labs(title="", x="Axe 1", y="Axe 2")
# Representation simultanee des individus et des 
#variables (composantes 1 et 2)
fviz_pca_biplot(ACP2, title="")

## Deuxieme application : le jeu de donnees tortues --------------------------------

cor(tortues[,-4])

library(GGally)
ggpairs(tortues, columns = 1:3, mapping= ggplot2::aes(color=sexe))

### La package ade4 --------------------------------

ACPtortues = dudi.pca(tortues[,-4],scannf=F)

fviz_eig(ACPtortues, addlabels = TRUE, 
         main="Eboulis des valeurs propres", xlab="Axes", 
         ylab="Variance expliquee (%)")

s.class(ACPtortues$li, tortues$sexe, col=c("blue","red"))

scatter(ACPtortues, posieig = "none", clab.row = 0)
s.class(ACPtortues$li, tortues$sexe, col = c("blue","red"), 
        add.plot = TRUE, cstar = 0, cellipse = 0, clabel = 0)

### La package factomineR --------------------------------

ACPtortues2 <- PCA(tortues, quali.sup=4, graph=F, scale.unit = TRUE)
summary(ACPtortues2)

plot.PCA(ACPtortues2, axes=c(1,2),choix="ind",  
         habillage=4, cex=0.7, title="", palette = c("blue","red"))

biplot <- fviz_pca_biplot(ACPtortues2, title="", 
                          label="var", habillage=4,
                          addEllipses=TRUE, 
                          ellipse.level=c(0.7))
biplot + 
  scale_color_manual(values=c("blue", "red"))+
  scale_fill_manual(values=c("blue", "red"))



