---
title: "Biking Project"
author: "Abdelhadi CHAJIA , Mustapha EL IDRISSI"
date: "1/16/2022"
output: html_notebook
---



```{r}
#  Clean the global environment
rm(list = ls())
```


```{r}
# définir le répertoire de travail
setwd("C:/Users/lenovo/Desktop/EHTP/Data engineering/Statistique/R/Biking project")
```


```{r}
# chargement des données et visualisation des premières lignes

mydata <- read.table("day.csv", sep = ',', header = TRUE)

# visualiser les 5 premiers ligne pour avoir une idée

head(mydata,5)
```

```{r}
# II Description sommaire des données
## 2.1 Donner le nombre de variables et le nombre d'observations
dim(mydata)

# On a 731 observations et 16 variables
```


```{r}
# 2.2 Déterminer les variables quantitatives et qualitatives
str(mydata)

# instant : indice d’enregistrement (variable qualitative)
# dteday : date (variable qualitative)
# season : saison (1:printemps, 2:été, 3:automne, 4:hiver) (variable qualitative)
# yr : année (0:2011, 1:2012) (variable qualitative)
# mnth : mois ( 1 à 12) (variable qualitative)
# hr : heure (0 à 23) (variable qualitative)
# holiday : le jour est férié ou non (variable qualitative)
# weekday : jour de la semaine (variable qualitative)
# workingday : si le jour n’est ni week-end ni jour férié est 1, sinon est 0. (variable qualitative)
# Météo :
## 1 : Clair, Peu nuageux, Partiellement nuageux (variable quantitative)
## 2 : Brume + Nuageux, Brume + Nuages épars, Brume + Quelques nuages, Brume (variable qualitative)
## 3: Neige Légère, Pluie Légère + Orage + Nuages épars, Pluie Légère + Nuages épars (variable qualitative)
## 4: Forte Pluie + Glace + Orage + Brume, Neige + Brouillard (variable qualitative)
# temp : Température normalisée en Celsius. Les valeurs sont divisées en 41 (max) (variable quantitative)
# atemp : Température ressentie normalisée en Celsius. Les valeurs sont divisées en 50 (max) (variable      quantitative)
# hum : Humidité normalisée. Les valeurs sont divisées en 100 (max) (variable quantitative)
# windspeed : Vitesse du vent normalisée. Les valeurs sont divisées en 67 (max) (variable quantitative)
# casual : nombre d’utilisateurs occasionnels (variable quantitative)
# registered : nombre d’utilisateurs enregistrés (variable quantitative)
# cnt: nombre total de vélos de location, y compris les vélos occasionnels et enregistrés (variable quantitative)
```


```{r}
# 2.3 Donner le nom de la variable cible à étudier en relation avec l’objectif de l’étude.

# pour charger la commande select on aura besoin du package "dplyr"
install.packages("dplyr")

# chargement du package
library(dplyr)

# selectionnerla variable cible
select(mydata,cnt)
# la variable cible est "cnt"
```



```{r}
# Pour garder la base initial
dt1 <- mydata

```

```{r}
# Pour verifier s'il ya des NA (données manquantes)

missing_val<-data.frame(apply(dt1,2,function(x){sum(is.na(x))}))
names(missing_val)[1]='missing_val'
missing_val
```

```{r}
### III Préparation des données

# 3.1 garder les variables suivantes :“season”,“ temp”,“ atemp”,“ hum”,“ weathersit” ,“windspeed ”,“yr”,“cnt”.

dt1 <-dplyr::select(dt1, -c("instant", "dteday", "mnth", "holiday", "weekday", "workingday" ,"casual","registered"))
```




```{r}
# 3.2 suppression des lignes où la variable “weathersit” prend les valeurs 3 et 4.

	dt1<-dt1[!(dt1$weathersit=="3" | dt1$weathersit=="4"),]
```




```{r}
# 3.3 Générer 4 variabales à paratir du variable season et le garder on utilisant one_hot

 # on a devise la question en 4 étapes
# la premiére on genere une autre colonne season identique

library(dplyr)

dt1 <- dt1 %>% 
  mutate(season1  = season, .before = yr)

# la deuxieme  pour utiliser one_hot et génrer les 4 colonnes et leurs indicateurs logiques  on aura besoin du package "mltools"

install.packages("mltools")

library(mltools)


# la troisiéme changer la variable en factor
library(data.table)
dt1$season <- as.factor(dt1$season)

# la quatriéme générer les quatres variables

dt1 <- one_hot(as.data.table(dt1, cols = "season1"))

# renomer les variable en nom precis

names(dt1)[names(dt1)=="season1"] <- "season"
names(dt1)[names(dt1)=="season_1"] <- "fall"
names(dt1)[names(dt1)=="season_4"] <- "winter"
names(dt1)[names(dt1)=="season_2"] <- "summer"
names(dt1)[names(dt1)=="season_3"] <- "spring"

# chargement des données

dim(dt1)

# on a 710 opservation et 12 variables

```




```{r}
# IV Répartition de la base de données
  # 4.1 échantillon aléatoire simple en utilisant sample

set.seed(100)

train_set_aléatoire=dt1[FALSE,]
test_set_aléatoire=dt1[FALSE,]

train_index_alétoire <- sample(1:nrow(dt1), 0.75 * nrow(dt1))
test_index_aléatoire <- setdiff(1:nrow(dt1), train_index_alétoire)

subtrain_aléatoire <- dt1[train_index_alétoire ,]
subtest_aléatoire <- dt1[test_index_aléatoire,]

train_set_aléatoire=rbind(train_set_aléatoire,subtrain_aléatoire)
test_set_aléatoire=rbind(test_set_aléatoire,subtest_aléatoire)

```

```{r}
dim(train_set_aléatoire)

# 75% donne un nombre d'observation de 532  en 12 variables
```


```{r}
# 25% donne un nombre d'observation de 178 en 12 variables
dim(test_set_aléatoire)
```
```{r}
# 4.2 échantillon systématique en utilisant fonction 

# cette fonction permet de :

# Tout d'abord, calculez et fixez l'intervalle d'échantillonnage.
# Choisissez un point de départ aléatoire entre 1 et l'intervalle d'échantillonnage.
# Enfin, répétez l'intervalle d'échantillonnage pour choisir les candidats à mettre dans la partition adéquate.


get_sys = function(N,n){
  k = ceiling(N/n)
  r = sample(1:k, 1)
  seq(r, r + k*(n-1), k)
}



test_set_systématique=dt1[FALSE,]
train_set_systématique=dt1[FALSE,]

test_index_systématique <- get_sys(nrow(dt1), 0.25*(nrow(dt1)))
train_index_systématique <- setdiff(1:nrow(dt1), test_index_systématique)

subtest_systematic <- dt1[test_index_systématique,]
subtrain_systematic <- dt1[train_index_systématique,]

test_set_systématique=rbind(test_set_systématique,subtest_systematic)
train_set_systématique=rbind(train_set_systématique,subtrain_systematic)


# etant donné que si on utilise le 75% comme "n" par rapport "N" on va generer un pas trés reduit ce qui result à des NA aprés donc on va utiliser le 25% pour arrivé à couvré tout la population selon le r qui est le pas



```

```{r}
dim(test_set_systématique)
```
```{r}
dim(train_set_systématique)
```

```{r}
# échantillonnage stratifié 
# pour utiliser la commande stratify on aura besoin du package suivant:
install.packages("rsample")
library(rsample)
library(dplyr)
```


```{r}
# tout d'abord pous s'assurer du percentage prise dans les variables par rapport à le weather sit

# c'est quoi le proportion de 1 et 2 dans la varibale meteo (weathersit)
 
dt1 %>% 
    group_by(weathersit,season) %>%
    summarise(freq = n()) %>%
    mutate(prop = freq / nrow(dt1))

# 463 obs sur 710 equivalent à 65% represente  l'etat de la meteo: Clair, Peu nuageux, Partiellement nuageux
# 247 obs sur 710 equivalent à 35% represente l'etat de la meteo : Brume + Nuageux, Brume + Nuages épars, Brume + Quelques nuages, Brume

```


```{r}

set.seed(100)

# prendre 

stratify <- initial_split(dt1, 
                          prop = 0.75, 
                          strata = weathersit,season)

# verifier le resultat

stratify

# train/test/Total>
# 532/178/710>
```


```{r}
# création du Training set
train_set_stratifié <- training(stratify)

# verifier le nombre d'observation prise
train_set_stratifié %>% nrow()

# 532 en Training
```


```{r}
# on va verifier la proprtion de 1 et 2 dans la partie training pour voir s'il respecte les proportions de 65% en 1 et 35% en 2
train_set_stratifié %>% 
  group_by(weathersit,season) %>% 
  summarise(freq=n()) %>% 
  mutate(prop=freq/nrow(train_set_stratifié))

# oui effectivement :
# 65% represente  l'etat de la meteo: Clair, Peu nuageux, Partiellement nuageux
# 35% represente l'etat de la meteo : Brume + Nuageux, Brume + Nuages épars, Brume + Quelques nuages, Brume
```

```{r}
# création du Test set
test_set_stratifié <- testing(stratify)

# verifier le nombre d'observation prise
test_set_stratifié %>% nrow()
```


```{r}
# on va verifier la proprtion de 1 et 2 dans la partie training pour voir s'il respecte les proportions de 65% en 1 et 35% en 2
test_set_stratifié %>% 
  group_by(weathersit,season) %>% 
  summarise(freq=n()) %>% 
  mutate(prop=freq/nrow(test_set_stratifié))

# oui effectivement :
# 65% represente  l'etat de la meteo: Clair, Peu nuageux, Partiellement nuageux
# 35% represente l'etat de la meteo : Brume + Nuageux, Brume + Nuages épars, Brume + Quelques nuages, Brume
```


```{r}
# 4 Analyse univariée
attach(dt1)
# 4.1 analyse appropriée à la base des variables 
# Pour une variable quantitative, les statistiques de base qu'on peut calculer sont le minimum, 
# le maximum, la moyenne, la variance et l'écart type, la médiane et les autres quantiles 

min(cnt) # le minimum nombre de location de vélo c'est 431
max(cnt) # le maximum nombre de location de vélo c'est 8714
range(cnt) # constation du min  et du max valeur
mean(cnt)  # au moyenne on a 4584 de location de velo en 2 années 2011 et 2012
var(cnt)   #  3598064 la dipsersion est  trés loin de la moyenne 
sd(cnt)    # l'ecart-type = 1896 càd que au mayenne le nbr de location est à 1896 point de la  moyenne de                   location
median(cnt) # on peut dire que 4585.5 c'est le nombre qui partage la serie en 2 partie
quantile(cnt)
quantile(cnt, probs = 0.99) #pour le percentile d'ordre 99
quantile(cnt, probs = c(0.01, 0.1, 0.9, 0.99))

# on a la moyenne = mediane donc la distribution suit une loi normal

```

```{r}
# pour représenter la distribution d'une variable quantitative on utilise l'histogramme :

hist(dt1$cnt, breaks = 20, ylab = 'fréquence de location', xlab = 'Total de location vélo', main = 'Distribution de location de vélo', col = 'brown' )

# on peut dire que la distribution suis une loi normale moyenne = mediane
```
```{r}
boxplot(cnt, horizontal = TRUE, outline = TRUE)
```
```{r}
dts <- density(dt1$cnt) 
plot(dts)
polygon(dts, col="brown", border="blue")
```


```{r}
################################################"
# 4. Analyse bivariée
#################################################
# 4.1 Variables quantitatives
library(dplyr)
dt1.au <- dt1 %>%
  group_by(season) %>%
  summarise(
    temp.min = min(temp),
    temp.max = max(temp),
    temp.var = var(temp),
    temp.med = median(temp),
    temp.stdev = sd(temp),
    temp.mean = mean(temp), 
    count = n())
```

```{r}
# charger dt1.au
dt1.au
# la temperature minimale se trouve en saison printemps "spring" ce qui explique la reduction au niveau de location des vélo

```


```{r}
# representation graphique des données en haut
boxplot(cnt ~ season,
        data = dt1,
        xlab = "Season",
        ylab = "cnt",
        main = "Number of rentals by season",
        col = "brown")
# Comme le montre le graphique ci-dessus, la saison qui enregistre une reduction ou niveau du location de velo c'est le "spring" = printembe par contre fall enregistre une hausse au niveau des locations
```



```{r}
boxplot(cnt~ weathersit,
         data = dt1,
         main = "Bike Rents by Weather Condition",
         xlab = "Weather Condition",
         ylab = "Number of rentals",
         col = "brown")

#le boxplot a démontrer que le nombre de location de vélo augmente 
# lorsque le weathersit = 1 et diminue lorsque le weathersit = 2
# 1 = Clair, Peu nuageux, Partiellement nuageux 
# 2 = Brume + Nuageux, Brume + Nuages épars, Brume + Quelques nuages, Brume

```

```{r}
library(corrr)
dt1 %>% correlate()
# La corrélation est une méthode statistique pour mesurer la relation entre  deux variables quantitatives.
# Les valeurs positives dans ce tableau indiquent la relation positive et vice versa. Plus la valeur absolue de   est élevée, plus la corrélation est forte. Si la valeur de r est 0, cela indique qu'il n'y a pas de relation    entre les deux variables. 


```



```{r}
# pour visualiser graphiquement les données en haut pour avoir une idée sur les variables qui represent une correlation 
library(corrplot)
plot(dt1)
```


```{r}
corrplot(cor(dt1),
  method = "number",
  type = "upper" # show only upper side
)
# on remarque dans ce tableau que les valeurs en bleu represente une correlation positive et les valeurs en rouges represente une correlation negative 
```


```{r}
# pour bien preciser  la correlation de la variable dependant (cnt) par rapport aux autres, en utilise focus du package ("corrr")
install.packages("corrr")

library(corrr)

dt1 %>% correlate() %>% focus(cnt)

# On voie ici que la correlation se trouve importante entre (cnt et les variables (fall, yr, temp, atemp))

```
```{r}

library(ggplot2)

dt1 %>% correlate() %>% focus(cnt) %>%
  ggplot(aes(x = term, y =cnt )) +
    geom_bar(stat = "identity") +
    ylab("Correlation with cnt") +
    xlab("Variable")



```

```{r}
# pour avoir une idée sur l'influence de la varibale qui represente un forte correlation qui est atemp on a crée un model linear simple par type d'echantionnage
model_alétoir <- lm(cnt ~ atemp, data = train_set_aléatoire)
summary(model_alétoir)


# en analysant les coefficients de correlation de chaque variable on décidé de predire la varibale cnt en fonction de atemp qui est à la foie = à temp+windspeed+umidité
# Équation estimée de la régression peut s'ecrit comme ça pour l'echantillon aléatoire  cnt=989.9+7561.2*atemp
```
```{r}
model_systematic <- lm(cnt ~ atemp, data = train_set_systématique)
summary(model_systematic)
```
```{r}
model_stratified <- lm(cnt ~ atemp, data = train_set_stratifié)
summary(model_stratified)
```


```{r}
# visualiser les coefficients de correlation en utilisant la méthode  pearson

cor(dt1[, c('cnt', 'temp', 'atemp', 'yr', 'fall')])

```


```{r}
# plot
install.packages('ggside')
  library(ggstatsplot)
ggscatterstats(data = dt1, x = cnt, y = temp)
```
```{r}
cor.test(dt1$cnt, dt1$temp, method = "pearson")
```
```{r}
# plot
install.packages('ggside')
  library(ggstatsplot)
ggscatterstats(data = dt1, x = atemp, y = cnt)
```
```{r}

# calculer  coefficient de pearson pour temp et atemp

cor.test(dt1$cnt, dt1$atemp, method = "pearson")
```


```{r}
# plot
install.packages('ggside')
  library(ggstatsplot)
ggscatterstats(data = dt1, x = fall, y = cnt)
```
```{r}

cor.test(dt1$cnt, dt1$fall, method = "pearson")
```



```{r}
# plot
install.packages('ggside')
  library(ggstatsplot)
ggscatterstats(data = dt1, x = yr, y = cnt)
```

```{r}
cor.test(dt1$cnt, dt1$yr, method = "pearson")
```
```{r}
# les attributs qui contiennent des informations redondantes qui peuvent être remplacées par des attributs dérivés c'est que on peut remplacer temp avec atemp puisqu'il déjà inclus
```

```{r}
# 5.2) A partir des résultats de la question (5.1), formuler une première hypothèse quant à la prédiction du nombre de locations de vélos à pas quotidien en fonction des paramètres environnementaux et saisonniers à la base des variables étudiées

# l'hypothése c'est que tout les variables sont necessaire à la prédiction  du cnt
```


```{r}
library(dplyr)
install.packages("leaps")
library("leaps")
ordinal_reg_aléatoire <- regsubsets(cnt~season+yr+temp+atemp+hum+windspeed , data=train_set_aléatoire, nbest = 1 , method = "exhaustive")
summary(ordinal_reg_aléatoire)

# le best model en utilisant 

# pour la ligne 1 le varibale "temp" est la variable predicteur pour ce model
# pour la ligne 2 les variables "yr et atemp" sont les variables prédicteurs
# pour la ligne 3 les variables "season et yr et atemp" sont les variables predicteurs
# pour la ligne 4 les variables "season et yr et atemp et hum" sont les variables predicteurs
# pour la ligne 5 les variables "season et yr et atemp et hum et windspeed" sont les variables predicteurs
# pour la ligne 5 les variables "season et yr et temp et atemp et hum et windspeed et " sont les variables predicteurs
```
```{r}
# Le R-carré ajusté compare le pouvoir explicatif des modèles de régression qui contiennent différents nombres de prédicteurs(variables).

# c'est à dire  que si on compare un modèle à 12 prédicteurs avec un R au carré plus élevé à un modèle à 9 prédicteur. Le modèle à 12 prédicteurs a-t-il un R au carré plus élevé parce qu'il est meilleur ? Ou le R au carré est-il plus élevé parce qu'il a plus de prédicteurs ? donc pour evité ça on Comparez simplement les valeurs R au carré ajustées pour le savoir 

summary(ordinal_reg_aléatoire)$adjr2
```


```{r}
max(summary(ordinal_reg_aléatoire)$adjr2)
```
```{r}
oridnal_reg_systematic <- regsubsets(cnt~season+yr+temp+atemp+hum+windspeed , data=train_set_systématique, nbest = 1 , method = "forward")
summary(oridnal_reg_systematic)
```
```{r}
summary(oridnal_reg_systematic)$adjr2
```
```{r}
max(summary(oridnal_reg_systematic)$adjr2)
```

```{r}
ordinal_reg_stratify <- regsubsets(cnt~season+yr+temp+atemp+hum+windspeed , data=train_set_stratifié, nbest = 1 , method = "exhaustive")
summary(ordinal_reg_stratify)

```

```{r}
summary(ordinal_reg_stratify)$adjr2

# suivant la logique du fait que le meilleur moldel est celui qui detien le R² Ajusté on peut deduit le model de l'echantillon stratifié ordinal est le meilleur model
```
```{r}
max(summary(ordinal_reg_stratify)$adjr2)
```


```{r}
hot_reg_aléatoire <- regsubsets(cnt~spring+fall+summer+winter+yr+temp+atemp+hum+windspeed , data=train_set_aléatoire, nbest = 1 , method = "exhaustive")

summary(hot_reg_aléatoire)
```
```{r}
summary(hot_reg_aléatoire)$adjr2
```
```{r}
max(summary(hot_reg_aléatoire)$adjr2)
```

```{r}
hot_reg_systemtic <- regsubsets(cnt~spring+fall+summer+winter+yr+temp+atemp+hum+windspeed , data=train_set_stratifié, nbest = 1 , method = "exhaustive")
summary(hot_reg_systemtic)
```
```{r}
summary(hot_reg_systemtic)$adjr2
```
```{r}
max(summary(hot_reg_systemtic)$adjr2)

# meilleur moldel est celui qui detien le R² Ajusté on peut deduit que le model de l'echantillon stratifié encoding est le meilleur model
```

```{r}
hot_reg_stratify <- regsubsets(cnt~spring+fall+summer+winter+yr+temp+atemp+hum+windspeed, data=train_set_stratifié, nbest = 1 , method = "exhaustive")
summary(hot_reg_stratify)
```
```{r}
summary(hot_reg_stratify)$adjr2
```
```{r}
max(summary(hot_reg_stratify)$adjr2)
```


```{r}
# ordinal encoding
# Elaboration du meileur model de regression multiple en utilisant le train data 
ordinal_mdl_aléatoir <- lm(cnt ~ yr + season + temp+atemp + hum + windspeed, data = train_set_aléatoire)
summary(ordinal_mdl_aléatoir)


# on peut voir que la valeur p de la statistique F est < 2,2e-16, ce qui est hautement significatif. Cela signifie qu'au moins une des variables prédictives est significativement liée à la variable de résultat.

# Pour un prédicteur donné, la statistique t évalue s'il existe ou non une association significative entre le  prédicteur et la variable de résultat, c'est-à-dire si le coefficient bêta du prédicteur est significativement  différent de zéro.

# On peut voir que l'évolution les variables "yr, season"  sont associée de manière significative à l'évolution de cnt , tandis que l'évolution de hum et windspeed ne sont pas associées de manières significative aux ventes. 

```

```{r}
resid_al<-residuals.lm(ordinal_mdl_aléatoir)
summary(resid_al)
```


```{r}
hist(resid(ordinal_mdl_aléatoir))
```

```{r}
ordinal_mdl_systématique <- lm(cnt ~ yr + season + temp +  atemp + hum + windspeed, data = train_set_systématique)
summary(ordinal_mdl_systématique)
```

```{r}
ordinal_mdl_stratifié <- lm(cnt ~ yr + season + temp +  atemp + hum + windspeed, data = train_set_stratifié)
summary(ordinal_mdl_stratifié)
```

```{r}
# one_hot_encoding
#Nous voulons construire un modèle pour estimer la location de vélo en fonction d'autres variables "yr, saison, temp, atemp, hum, vitesse du vent") 

# You can compute the model coefficients in R as follow:

hot_mdl_aléatoire <- lm(cnt ~ yr + spring + summer + fall + winter + temp +  atemp + hum + windspeed, data =train_set_aléatoire)
summary(hot_mdl_aléatoire)
```
```{r}
resid_al_hot<-residuals.lm(hot_mdl_aléatoire)
summary(resid_al_hot)
```


```{r}
hist(resid(hot_mdl_aléatoire))
# normalité des residu
```

```{r}
hot_mdl_systématique <- lm(cnt ~ yr + spring + summer + fall + winter + temp +  atemp + hum + windspeed, data =train_set_systématique)
summary(hot_mdl_systématique)
```

```{r}
hot_mdl_stratifié <- lm(cnt ~ yr + spring + summer + fall + winter + temp +  atemp + hum + windspeed, data =train_set_stratifié)
summary(hot_mdl_stratifié)
```


```{r}
install.packages("Metrics")
library(Metrics)
library(dplyr)

```

```{r}
ordinal_Predict_aléatoire <- ordinal_mdl_aléatoir %>% predict(test_set_aléatoire)
rmse(ordinal_Predict_aléatoire, test_set_aléatoire$cnt)
```

```{r}
ordinal_Predict_systématique <- hot_mdl_systématique %>% predict(test_set_systématique)
rmse(ordinal_Predict_systématique, test_set_systématique$cnt)
```

```{r}
ordinal_Predict_stratifié <- ordinal_mdl_stratifié %>% predict(test_set_stratifié)
rmse(ordinal_Predict_stratifié, test_set_stratifié$cnt)
```

```{r}
hot_Predict_aléatoire <- hot_mdl_aléatoire %>% predict(test_set_aléatoire)
rmse(hot_Predict_aléatoire, test_set_aléatoire$cnt)
```
```{r}
hot_Predict_systématique <- hot_mdl_systématique %>% predict(test_set_systématique)
rmse(hot_Predict_systématique, test_set_systématique$cnt)
```

```{r}
hot_Predict_stratifié <- hot_mdl_stratifié %>% predict(test_set_stratifié)
rmse(hot_Predict_stratifié, test_set_stratifié$cnt)
```

```{r}
# L'erreur quadratique moyenne (RMSE) est l'écart type des résidus (erreurs de prédiction). Les résidus sont une mesure de la distance qui sépare les points de données de la ligne de régression ; RMSE est une mesure de la façon dont ces résidus sont répartis. En d'autres termes, il vous indique à quel point les données sont concentrées autour de la ligne de meilleur ajustement

# le model qui represente  

```

```{r}
cor(ordinal_Predict_aléatoire,test_set_aléatoire$cnt)
```
```{r}
cor(ordinal_Predict_systématique,test_set_systématique$cnt)
```
```{r}
cor(ordinal_Predict_stratifié,test_set_stratifié$cnt)
```

```{r}
cor(hot_Predict_aléatoire,test_set_aléatoire$cnt)
```

```{r}
cor(hot_Predict_systématique,test_set_systématique$cnt)
```

```{r}
cor(hot_Predict_stratifié,test_set_stratifié$cnt)
```



