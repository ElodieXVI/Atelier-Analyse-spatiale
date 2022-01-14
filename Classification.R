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
ajout <- select(disp_avignon_mono, "CODGEO", "TYM5Q3_Q1")
base_class2 <- left_join(base_class, ajout, by="CODGEO")

ur <- read_excel("Data/grille_densite_2021_agrege.xlsx", sheet = 1)
ur2 <- ur %>% 
  filter(CODGEO %in% uu_avignon$CODGEO)
base_class2$densite <- ur2$densite


Factoshiny(base_class2)
res.PCA<-PCA(base_class2,ncp=Inf, scale.unit=FALSE,quali.sup=c(1),graph=FALSE)
res.HCPC<-HCPC(res.PCA,nb.clust=3,consol=TRUE,graph=FALSE)

base_class2$clust <- res.HCPC$data.clust$clust
write.csv2(base_class2, "classification_complete.csv")


### Et pour tout le monde
pauvre <- select(pvr_avignon, "CODGEO", "TP6018")
base_ens <- disp_avignon %>% select("CODGEO","Q3_Q1", "PPSOC18", "PPAT18", "PPSOC18") 
base_ens <- left_join(base_ens, pauvre, by="CODGEO")
base_ens$densite <- ur2$densite
base_ens <- base_ens[-15,]

Factoshiny(base_ens)
res.PCA<-PCA(base_ens,ncp=Inf, scale.unit=FALSE,quali.sup=c(1),graph=FALSE)
res.HCPC<-HCPC(res.PCA,nb.clust=3,consol=TRUE,graph=FALSE)

base_ens$clust <- res.HCPC$data.clust$clust

write.csv2(base_ens, "classification_complete_ens.csv")

#### Revenus type #

names(disp_avignon)
#PACT18, PPEN18, PPAT18, PPSOC18
pauvre <- select(pvr_avignon, "CODGEO", "TP6018")
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

data_mono <- disp_avignon %>% select("CODGEO","TYM5PACT18", "TYM5PPEN18", "TYM5PPAT18", "TYM5PPSOC18")

sum(is.na(data_mono)) #68

data_mono <- rename(data_mono, activites = TYM5PACT18)
data_mono <- rename(data_mono, pensions = TYM5PPEN18)
data_mono <- rename(data_mono, patrimoine = TYM5PPAT18)
data_mono <- rename(data_mono, prestasoc = TYM5PPSOC18)
data_mono$CODGEO <- as.factor(data_mono$CODGEO)
data_mono <- data_mono[-2,]

Factoshiny(data_mono)
res.PCA<-PCA(data_mono,ncp=Inf, scale.unit=FALSE,quali.sup=c(1),graph=FALSE)
res.HCPC<-HCPC(res.PCA,nb.clust=3,consol=TRUE,graph=FALSE)

data_mono$clust <- res.HCPC$data_mono.clust$clust

write.csv2(data_mono, "Classification_revenus_mono.csv")

### CLassification des familles monoparentales avec tout

data_mono_comp <- left_join(base_class2, data_mono, by="CODGEO")
data_mono_comp$TYM5PPSOC18 <- NULL
data_mono_comp$TYM5GI18 <- NULL

data_mono_comp$gini <- disp_avignon_mono$TYM5GI18
data_mono_comp$gini <- as.character(data_mono_comp$gini)

data_mono_comp <- data_mono_comp %>% drop_na()

Factoshiny(data_mono_comp)




