library(tidyverse)
library(mgcv)
#remotes::install_github("gavinsimpson/gratia")
library(gratia)
library(nlme)

data_raul <- read.table(
  "C:/Users/au710823/Dropbox/Franca/Posgrado/Raul/TABLA.CON.BASE.DE.DATOS.CARBONO.5.de.JULIO.txt", 
  sep = "\t", header = TRUE) |> filter(CARBONO.ORG.LABIL<5) |> 
  mutate(ANIOS.DE.USO.AGRICOLA=as.numeric(ANIOS.DE.USO.AGRICOLA),
         TEXTURA=as.factor(TEXTURA),
         TEXTURA_gg=as.factor(TEXTURA_gg),
         Suelo_gg=as.factor(Suelo_gg))

colnames(data_raul)

ggplot(data_raul,
       aes(y = CARBONO.ORG.LABIL,
           x = ANIOS.DE.USO.AGRICOLA
           #,col=TEXTURA
           )) +
           geom_point() + 
  geom_smooth(alpha = 0.1) + 
  theme_bw()

# Modelo lineal mixto
mgls <- gls(CARBONO.ORG.LABIL~1+TEXTURA+ANIOS.DE.USO.AGRICOLA+
            TEXTURA:ANIOS.DE.USO.AGRICOLA
         ,weights=varIdent(form=~1|USO.ACTUAL)
         ,method="REML"
         ,na.action=na.omit
         ,data=data_raul)

summary(mgls)

# GAM COS Labil
mr <- gam(formula = CARBONO.ORG.LABIL ~TEXTURA+s(ANIOS.DE.USO.AGRICOLA, by=TEXTURA)+
            s(LATITUD, LONGITUD, bs = "sos")
          ,data=data_raul)

summary(mr)

mr |> draw()

# gam COS particulado
mpar <- gam(formula = COPARTICULADO ~TEXTURA+s(ANIOS.DE.USO.AGRICOLA, by=TEXTURA)+
            s(LATITUD, LONGITUD, bs = "sos")
          ,data=data_raul)

summary(mpar)

mpar |> draw()

# gam estabilidad agregados
mest <- gam(formula = ESTABILIDAD.DE.AGREGADOS ~TEXTURA+s(ANIOS.DE.USO.AGRICOLA, by=TEXTURA)+
              s(LATITUD, LONGITUD, bs = "sos")
            ,data=data_raul)

summary(mest)

mest |> draw()
