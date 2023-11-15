# Introduction ------------------------------------------------------------

## Le package *tidyverse*  ---------------------------------

library(tidyverse)

## Citer **R** et le package *tidyverse* ---------------------------------

# Citation de R
citation()
# Citation du package ggplot2
citation(package="tidyverse")


## Le jeu de données *airquality* ---------------------------------

data(airquality)
# informations sur le jeu de données
?airquality


# L'importation de données : les packages **readr** et **readxl** ---------

## Exemples -----------------------

## Exemple 1 : importation d'un fichier Excel (package readxl)-----------------------

library(readxl)
exemple1 <- read_excel("exempleexcel.xlsx", na = "noobserv", col_names=TRUE)
head(exemple1)

## Exemple 2 : importation d'un fichier .csv (package readr)-----------------------

exemple2 <- read_delim("exempleexcel.csv", na = "noobserv", 
                       delim = ";", show_col_types = FALSE)
head(exemple2)


# Mise en forme et manipulation des données -----------------------

## Qu'est ce qu'un tibble ? -----------------------

head(tibble(airquality))

## Quelques opérations élémentaires avec le package dplyr-----------------------

### La commande pipe %>% -----------------------

airquality %>% head()

### Création d'une nouvelle variable : mutate() -----------------------

airquality %>% mutate(mois=month.name[Month]) %>% head()

airquality %>% mutate(Year=rep(1973,153)) %>% head()

### Retour sur le pipe %>% -----------------------

ordre_mois<-c("May","June","July","August","September")
dataairquality  <- airquality %>% 
  as_tibble() %>% 
  mutate(mois=month.name[Month]) %>% 
  mutate(mois=factor(x=mois, levels=ordre_mois)) %>%
  mutate(Annee=rep(1973,153)) %>%
  drop_na() 
# On enlève les lignes où il y a une donnée manquante.

### Changer le nom d'une variable : rename() -----------------------

(dataairquality  <- dataairquality %>% rename(Mois=mois)) 

### Sélectionner une ou plusieurs variable(s) : select() -----------------------

# par le nom des variables
dataairquality %>% select(Ozone)
dataairquality %>% select(Ozone, Wind)
# par le numéro de colonne
dataairquality %>% select(c(1,3))
# On peut également enlever une variable
dataairquality %>% select(-Month)
# Sélectionner les colonnes dont le nom commence par M
dataairquality %>% select(starts_with('M'))
# Sélectionner les colonnes dont le nom commence par M et finit par s
dataairquality %>% select(starts_with('M') & ends_with('s'))

### Création d'un sous-ensemble de données : filter() -----------------------

dataairquality %>% filter(Mois=="May")
dataairquality %>% filter(Wind >20)
dataairquality %>% filter(Wind <20 & Wind >10)
dataairquality %>% filter(Wind >20 | Wind <3)

### Sélectionner des lignes en utilisant leur position : slice() -----------------------

dataairquality %>% slice(c(1,3))

### Ordonner les lignes selon une (ou des) variable(s) : arrange() -----------------------

dataairquality %>% arrange(Wind)
dataairquality %>% arrange(Wind, Temp)
# Par ordre décroissant
dataairquality %>% arrange(desc(Wind))

### Calculer le nombre d'observations par groupe : count() -----------------------

dataairquality %>% count(Mois)

### Grouper : group_by() -----------------------

dataairquality %>% group_by(Mois)
dataairquality %>% group_by(Mois) %>% slice(1)

### Résumer des variables : summarise() -----------------------

# Moyenne d'une variable
dataairquality %>% summarise(moyWind=mean(Wind))
# Calcul de la moyenne de chaque variable quantitative (sans les NA)
dataairquality %>% select(-Mois) %>% summarise_all(funs(mean), na.rm=TRUE)
# calculer effectifs / mois
dataairquality %>% group_by(Mois) %>% summarize(n=n())
# Moyenne, variance, écart-type, médiane, min, max et IQR
dataairquality %>% 
  summarise(Moy_wind = mean(Wind), var_wind=var(Wind), 
            SD_wind=sd(Wind), mediane_Wind=median(Wind),
            min_wind=min(Wind),max_wind=max(Wind),
            IQR_wind=IQR(Wind))

## Dégrouper : ungroup() -----------------------

dataairquality %>% group_by(Mois) %>% 
  count(Mois) %>% ungroup() %>%
  count(Mois)

## Appliquer une fonction à chacun des groupes : group_map()  -----------------------

dataairquality %>% group_by(Mois) %>%
  group_map(~quantile(.x$Ozone, 
                      probs = c(0.25, 0.5, 0.75)))

## Les lignes uniques d'un tableau de données : distinct() -----------------------

dataairquality %>% distinct(Mois)
dataairquality %>% distinct(Mois, Month)

## Jointure de deux tableaux -----------------------

tableau1 <- tibble(x=c(1:nrow(dataairquality)), Mois=c(rep("Jul",15), rep(month.abb[1:12],8)))
# Inclure toutes les lignes
dataairquality %>% inner_join(tableau1)
# Inclure toutes les lignes
dataairquality %>% left_join(tableau1)
# Inclure toutes les lignes
dataairquality %>% right_join(tableau1)
# Inclure toutes les lignes dans tableau1 ou dataairquality
dataairquality %>% full_join(tableau1)


## Transformation de données : Le package **tidyr** -----------------------

### Enlever les données manquantes : drop_na() -----------------------

airquality %>% drop_na() %>% head()

### Rassembler les colonnes en lignes -----------------------

pivot_longer(dataairquality, cols=1:4, names_to="Variable", 
             values_to="Valeur")

### Répartir les lignes en colonnes -----------------------

dataairquality %>% spread(Mois, Day)

## Créer des listes de tableaux de données : nest()

dataairquality %>% group_by(Mois) %>% nest()

## Aplatir en colonnes régulières : unnest()

dataairquality %>% group_by(Mois) %>% 
  nest() %>% 
  unnest(c(1,2))

# Les graphiques avec ggplot2 ---------------------------------------------

## Exemple d'un nuage de points ---------------------------------------------

ggplot(dataairquality,aes(x=Temp,y=Ozone)) + geom_point()

### Traçons les points en rouge en ajoutant une propriété esthétique ---------------------------------------------

ggplot(dataairquality,aes(x=Temp,y=Ozone)) + geom_point(color="darkred")

### Changeons la forme des points en carré ---------------------------------------------

ggplot(dataairquality,aes(x=Temp,y=Ozone)) + geom_point(pch=15)

### tracer les carrés en rouge ---------------------------------------------

ggplot(dataairquality,aes(x=Temp,y=Ozone)) + geom_point(colour="darkred", 
                                                        pch=15)

### transparence des points ---------------------------------------------

ggplot(dataairquality,aes(x=Temp,y=Ozone)) + geom_point(colour="darkred", 
                                                        alpha=0.3, pch=15)

### titre de l'axe des abscisses et des ordonnées --------------------------------------------- 

ggplot(dataairquality,aes(x=Temp,y=Ozone)) + geom_point(colour="darkred", 
                                                        pch=15) + 
  xlab("Température (F)") + ylab("Ozone (ppb)")

### Taille du titre des axes --------------------------------------------- 

ggplot(dataairquality, aes(x=Temp, y=Ozone)) + 
  geom_point(colour="darkred", pch=15) + 
  xlab("Température (F)") + ylab("Taux d'ozone (ppb)") + 
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

### Taille des axes --------------------------------------------- 

ggplot(dataairquality, aes(x=Temp, y=Ozone)) + 
  geom_point(colour="darkred", pch=15) + 
  xlab("Température (F)") + ylab("Taux d'ozone (ppb)") + 
  theme(axis.text = element_text(size = 15))

### graphique pour chaque mois ---------------------------------------------

ggplot(dataairquality,aes(x=Temp,y=Ozone)) + 
  geom_point(colour="darkred", pch=15) + 
  xlab("Température (F)") + ylab("Ozone (ppb)") + facet_wrap(~Mois)

### ajouter un titre au graphique ---------------------------------------------

ggplot(dataairquality, aes(x=Temp, y=Ozone)) + 
  geom_point(colour="darkred", pch=15) + 
  xlab("Température (F)") + ylab("Taux d'ozone (ppb)") + 
  ggtitle("Taux d'ozone en fonction de la température")

### themes ---------------------------------------------

ggplot(dataairquality,aes(x=Temp,y=Ozone)) + 
  geom_point(colour="darkred", pch=15) + 
  xlab("Température (F)") + ylab("Ozone (ppb)") + theme_classic()


ggplot(dataairquality,aes(x=Temp,y=Ozone)) + 
  geom_point(colour="darkred", pch=15) + xlab("Température (F)") +
  ylab("Ozone (ppb)") + theme_minimal()

ggplot(dataairquality,aes(x=Temp,y=Ozone)) + 
  geom_point(colour="darkred", pch=15) + xlab("Température (degrees F)") + 
  ylab("Ozone (ppb)") + theme_bw()


ggplot(dataairquality,aes(x=Temp,y=Ozone)) + 
  geom_point(colour="pink", pch=15) + 
  xlab("Température (F)") + ylab("Ozone (ppb)") + theme_dark()

ggplot(dataairquality, aes(x=Temp, y=Ozone)) + 
  geom_point(colour="black", pch=15) + 
  xlab("Température (F)") + ylab("Taux d'ozone (ppb)") + 
  theme(panel.background = element_rect(fill="lightgreen"),
        panel.border = element_rect(colour="brown", fill=NA, linewidth=3),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

### colorier chaque point en fonction du mois qui est une variable qualitative. ---------------------------------------------

ggplot(dataairquality,aes(x=Temp,y=Ozone)) + geom_point(pch=15, aes(colour=Mois)) + 
  xlab("Température (F)") + ylab("Ozone (ppb)") + theme_minimal()

### palette ---------------------------------------------

ggplot(dataairquality, aes(x=Temp, y=Ozone)) + 
  geom_point(pch=15, aes(colour=Mois))+ 
  xlab("Température (F)") + ylab("Taux d'ozone (ppb)") + 
  theme_minimal() +
  scale_color_brewer(palette="Dark2")

ggplot(dataairquality, aes(x=Temp, y=Ozone)) + 
  geom_point(pch=15, aes(colour=Mois))+ 
  xlab("Température (F)") + ylab("Taux d'ozone (ppb)") + 
  theme_minimal() +
  scale_color_viridis_d()

# Traçons le taux d'ozone en fonction de la vitesse du vent en coloriant les points selon l'intensité de la température (variable quantitative)
ggplot(dataairquality, aes(x=Wind, y=Ozone)) + 
  geom_point(aes(colour=Temp)) + 
  scale_colour_gradient(low="pink", high="darkred") +
  xlab("Vitesse du vent (mph)") + ylab("Taux d'ozone (ppb)") + 
  theme_minimal() +
  labs(colour="Température (F)")

# le package ggsci
library(ggsci)
ggplot(dataairquality, aes(x=Temp, y=Ozone)) + 
  geom_point(pch=15, aes(colour=Mois))+ 
  xlab("Température (F)") + ylab("Taux d'ozone (ppb)") + 
  theme_minimal() +
  scale_color_lancet()

# Librairie khroma
library(khroma)
ggplot(dataairquality, aes(x=Temp, y=Ozone)) + 
  geom_point(pch=15, aes(colour=Mois))+ 
  xlab("Température (F)") + ylab("Taux d'ozone (ppb)") + 
  theme_minimal() +
  scale_colour_bright()

### legende ---------------------------------------------

# Changer le texte de la légende

ggplot(dataairquality, aes(x=Temp, y=Ozone)) + 
  geom_point(pch=15, aes(colour=Mois))+ 
  xlab("Température (F)") + ylab("Taux d'ozone (ppb)") + 
  theme_minimal() +
  scale_colour_discrete(labels= 
                          c("Mai", "Juin", "Juillet","Août","Septembre"))

# aspect de la légende

ggplot(dataairquality, aes(x=Temp, y=Ozone)) + 
  geom_point(pch=15, aes(colour=Mois))+ 
  xlab("Température (F)") + ylab("Taux d'ozone (ppb)") + 
  theme_minimal() +
  scale_colour_discrete(labels=
                          c("Mai", "Juin", "Juillet","Août","Septembre")) +
  theme(legend.background = element_rect(fill="lightgrey"))

# position de la légende

ggplot(dataairquality, aes(x=Temp, y=Ozone)) + 
  geom_point(pch=15, aes(colour=Mois))+ 
  xlab("Température (F)") + ylab("Taux d'ozone (ppb)") + 
  theme_minimal() +
  scale_colour_discrete(labels=
                          c("Mai", "Juin", "Juillet","Août","Septembre")) +
  theme(legend.position = "top", 
        legend.background = element_rect(fill="lightgrey"))

ggplot(dataairquality, aes(x=Temp, y=Ozone)) + 
  geom_point(pch=15, aes(colour=Mois))+ 
  xlab("Température (F)") + ylab("Taux d'ozone (ppb)") + 
  theme_minimal() +
  scale_colour_discrete(labels=
                          c("Mai", "Juin", "Juillet","Août","Septembre")) +
  theme(legend.position = c(.2,.75), 
        legend.background = element_rect(fill="lightgrey"))


### forme selon le mois. ---------------------------------------------

ggplot(dataairquality, aes(x=Temp, y=Ozone)) + 
  geom_point(aes(shape=Mois))+ 
  xlab("Température (F)") + ylab("Taux d'ozone (ppb)") + 
  theme_minimal() +
  scale_shape_discrete(labels = 
                         c("Mai", "Juin", "Juillet","Août","Septembre"))

### variable temporelle. Il faut au préalable charger le package *lubridate* ---------------------------------------------

library(lubridate)
dataairquality2 <- dataairquality %>% 
  mutate(Date = dmy(paste(Day, Month, Annee, sep = "/")))

ggplot(dataairquality2, aes(x=Date,y=Ozone)) + geom_line(colour="darkred") + 
  xlab("") +  theme_bw()


### manipulation des données avant graphique  ---------------------------------------------

# toutes les mesures sur le même graphique

dataairquality_allmes <- pivot_longer(dataairquality2, cols=1:4, 
                                      names_to = "Mesure", 
                                      values_to = "Valeur")
ggplot(dataairquality_allmes, aes(x=Date, y=Valeur)) + 
  geom_line(aes(colour=Mesure)) + 
  xlab("") +  theme_bw()


## Ajouter des éléments statistiques  ---------------------------------------------

### Ajouter une droite de régression  ---------------------------------------------

ggplot(dataairquality,aes(x=Temp,y=Ozone)) + 
  geom_point(colour="darkred", pch=15) + geom_smooth(method="lm", formula=y~x) + 
  xlab("Température (F)") + 
  ylab("Ozone (ppb)") + 
  theme_minimal()

ggplot(dataairquality,aes(x=Temp,y=Ozone)) + 
  geom_point(colour="darkred", pch=15) + 
  geom_smooth(method="loess", formula=y~x) + xlab("Température (F)") + 
  ylab("Ozone (ppb)") + 
  theme_minimal()

ggplot(dataairquality,aes(x=Temp,y=Ozone)) + 
  geom_point(colour="darkred",  pch=15) + geom_smooth(method="lm", 
                                                      formula=y~poly(x,3)) + 
  xlab("Température (F)") + ylab("Ozone (ppb)") + 
  theme_minimal()

ggplot(dataairquality,aes(x=Temp,y=Ozone, colour=Mois)) + geom_point() + 
  geom_smooth(method="lm", formula=y~x, se=FALSE) +
  xlab("Température (F)") + ylab("Ozone (ppb)") + theme_minimal() +
  scale_colour_discrete(labels= c("Mai", "Juin", "Juillet","Août","Septembre"))


### Ajouter une annotation ---------------------------------------------

data_ozone_sup_100 <- dataairquality %>% filter(Ozone>100)
ggplot(dataairquality,aes(x=Temp,y=Ozone)) + 
  geom_point(pch=15, aes(colour=Mois))+ 
  geom_text(aes(label=Day, vjust="right"), data=data_ozone_sup_100) +
  xlab("Température (F)") + ylab("Ozone (ppb)") + theme_minimal() +
  scale_colour_discrete(labels= c("Mai", "Juin", "Juillet","Août","Septembre"))


## Construction d'un histogramme et d'une courbe de densité  ---------------------------------------------

ggplot(dataairquality, aes(x=Ozone)) + 
  geom_histogram(color="darkblue",fill="lightblue") + theme_minimal()

ggplot(dataairquality, aes(x=Ozone)) + 
  geom_histogram(alpha=0.2, bins=10, aes(colour=Mois, fill=Mois)) + 
  theme_minimal()

ggplot(dataairquality, aes(x=Ozone)) + 
  geom_density() + theme_minimal()

ggplot(dataairquality, aes(x=Ozone)) + 
    stat_density(aes(colour=Mois), geom="line",position="identity") +
  theme_minimal() +
  scale_colour_discrete(labels= c("Mai", "Juin", "Juillet","Août","Septembre"))

## Construction d'une boîte à moustaches ---------------------------------------------

ggplot(dataairquality, aes(x= Mois, Ozone)) + 
  geom_boxplot(alpha=0.2,aes(colour=Mois, fill=Mois), show.legend = FALSE) +
  theme_minimal()


ggplot(dataairquality, aes(x= Mois, Ozone)) + 
  geom_boxplot(alpha=0.2,aes(colour=Mois, fill=Mois), show.legend = FALSE) +
  stat_summary(fun=mean,geom="point") +
  theme_minimal()


ggplot(dataairquality, aes(x=Ozone, y=Mois)) + 
  geom_violin(alpha=0.5, aes(fill=Mois), show.legend = FALSE) + 
  geom_boxplot(width=0.1, aes(fill=Mois), show.legend = FALSE) + 
  theme_minimal()

## Construction d'un diagramme en barres ---------------------------------------------

ggplot(dataairquality, aes(y=Ozone, x=Mois)) + 
  geom_bar(stat="identity", aes(fill=Mois), show.legend=FALSE) + xlab("Month") + 
  theme_minimal()

dataresume <- dataairquality %>% group_by(Mois)  %>% 
  summarise(n=n(),Moy_Ozone = mean(Ozone, na.rm=TRUE), 
            SE_Ozone=sd(Ozone, na.rm=TRUE)/sqrt(n))

ggplot(dataresume, aes(y=Moy_Ozone, x=Mois)) + 
  geom_bar(stat="identity", aes(fill=Mois), show.legend=FALSE) + xlab("Month") + 
  geom_errorbar(aes(ymin=Moy_Ozone-SE_Ozone, ymax=Moy_Ozone+SE_Ozone)) + 
  theme_minimal()


# Autres packages ---------------------------------------------------------

library(ggplotAssist)
library(esquisse)


