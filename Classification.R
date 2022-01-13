#Classification ####
### Essai de classification des familles monoparentales 
library(FactoMineR)
library(Factoshiny)
library(tidyverse)

Q218 <- select(disp_avignon, "CODGEO", "Q218", "TYM5Q218")
Q218 <- rename(Q218, revenu = Q218)
Q218 <- rename(Q218, revenu_mono = TYM5Q218)
Q2182 <- select(dec_com_avignon, "CODGEO","Q218", "TYM5Q218")
total <- left_join(Q218, Q2182, by="CODGEO")

#revenus médians disponibles, CODGEO, 
#codgeo, revenus médians déclarés, part des prestations, taux de pauvreté de la commune

pauvre <- select(pvr_avignon, "CODGEO", "TP6018")
base_class <- select(total, "CODGEO","TYM5Q218")
ajout <- select(disp_avignon_mono, "CODGEO","TYM5PPSOC18")
base_class <- left_join(base_class, ajout, by="CODGEO")
base_class <- left_join(base_class, pauvre, by="CODGEO")

Factoshiny(base_class)

res.PCA<-PCA(base_class,ncp=Inf, scale.unit=FALSE,quali.sup=c(1),graph=FALSE)
res.HCPC<-HCPC(res.PCA,nb.clust=3,consol=TRUE,graph=FALSE)

plot.HCPC(res.HCPC,choice='tree',title='Arbre hiérarchique')

base_class$clust <- res.HCPC$data.clust$clust

write.csv2(base_class, "classification.csv")

### Suite :

names(base_class)
ajout <- select(disp_avignon_mono, "CODGEO","TYM5GI18", "TYM5Q3_Q1")
base_class2 <- left_join(base_class, ajout, by="CODGEO")

names(base_class2)

## Recodage de base_class2$TYM5GI18 en base_class2$TYM5GI18
base_class2$TYM5GI18 <- cut(base_class2$TYM5GI18,
  include.lowest = FALSE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(0.226, 0.254, 0.265, 0.278, 0.46))

## Recodage de base_class2$TYM5GI18
base_class2$TYM5GI18 <- fct_recode(base_class2$TYM5GI18,
  "1" = "[0.226,0.254)",
  "2" = "[0.254,0.265)",
  "3" = "[0.265,0.278)",
  "4" = "[0.278,0.46)")

ur <- read_excel("Data/grille_densite_2021_agrege.xlsx", sheet = 1)
ur2 <- ur %>% 
  filter(CODGEO %in% uu_avignon$CODGEO)
base_class2$densite <- ur2$densite

base_class2 <- base_class2 %>% drop_na()

Factoshiny(base_class2)
res.PCA<-PCA(base_class2,ncp=Inf, scale.unit=FALSE,quali.sup=c(1,5),graph=FALSE)
res.HCPC<-HCPC(res.PCA,nb.clust=3,consol=TRUE,graph=FALSE)

base_class2$clust <- res.HCPC$data.clust$clust

base_class <- base_class2
###Ajout de la dernière variable de densité :

ur <- read_excel("Data/grille_densite_2021_agrege.xlsx", sheet = 1)
ur <- left_join(comsf_avignon, ur, by="CODGEO")
ur <- st_sf(ur)
class(ur)
mf_map(ur)
mf_map(ur,
      var = "densite",
      type = "typo",
      pal = viridis(3),
      leg_title = "Communes urbaines et rurales",
      add= TRUE,
      border = "white")
summary(ur$densite)


ur2 <- ur %>% 
  filter(CODGEO %in% uu_avignon$CODGEO)

base_class2$densite <- ur$densite


#### Revenus type #

names(disp_avignon)
#PACT18, PPEN18, PPAT18, PPSOC18

data <- disp_avignon %>% select("CODGEO","PACT18", "PPEN18", "PPAT18", "PPSOC18") 

sum(is.na(data)) #68

data <- rename(data, activites = PACT18)
data <- rename(data, pensions = PPEN18)
data <- rename(data, patrimoine = PPAT18)
data <- rename(data, prestasoc = PPSOC18)


Factoshiny(data)
res.PCA<-PCA(data,ncp=Inf, scale.unit=FALSE,quali.sup=c(1),graph=FALSE)
res.HCPC<-HCPC(res.PCA,nb.clust=4,consol=TRUE,graph=FALSE)

data$clust <- res.HCPC$data.clust$clust

write.csv2(data, "Classification_revenus.csv")


#### Revenus type mono#

names(disp_avignon_mono)
#PACT18, PPEN18, PPAT18, PPSOC18

data <- disp_avignon %>% select("CODGEO","TYM5PACT18", "TYM5PPEN18", "TYM5PPAT18", "TYM5PPSOC18")

sum(is.na(data)) #68

data <- rename(data, activites = TYM5PACT18)
data <- rename(data, pensions = TYM5PPEN18)
data <- rename(data, patrimoine = TYM5PPAT18)
data <- rename(data, prestasoc = TYM5PPSOC18)
data$CODGEO <- as.factor(data$CODGEO)
data <- data[-2,]


Factoshiny(data)
res.PCA<-PCA(data,ncp=Inf, scale.unit=FALSE,quali.sup=c(1),graph=FALSE)
res.HCPC<-HCPC(res.PCA,nb.clust=4,consol=TRUE,graph=FALSE)

data$clust <- res.HCPC$data.clust$clust

write.csv2(data, "Classification_revenus.csv")
