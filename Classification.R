#Classification ####
### Essai de classification des familles monoparentales 
library(FactoMineR)
library(Factoshiny)
library(tidyverse)

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