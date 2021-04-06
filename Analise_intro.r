#install.packages("tidyverse")
#install.packages("ggthemes")
#install.packages("readxl")
#install.packages("maptools")
#install.packages("sf")
#install.packages("plotly")
#install.packages("tmap")
library(tmap)
library(plotly)
library(tidyverse)
library(ggthemes)
library(readxl)
library(rgdal)
library(maptools)
library(sf)
#shps2020 pegos no portal:https://www.ibge.gov.br/geociencias/organizacao-do-territorio/15774-malhas.html?=&t=downloads
meso <- rgdal::readOGR("SP_Mesorregioes_2020.shp")#mesorregioes
micro <- rgdal::readOGR("SP_Microrregioes_2020.shp")

muni <- rgdal::readOGR("SP_Municipios_2020.shp")


#brmaps r

anual_sp <- data.frame(read_xlsx("Anual-Estado-SP.xlsx"))
#FRV = Furto e Roubo de Veiculos
anual_sp <- anual_sp[,-6]

spdata<- read_csv("ds_SSP_PolicyProductivity_SP-BR_utf8_2001-2020_rev3.csv")


dsp <- read_csv("ds_SSP_MonthlyOccurences_SP-BR_utf8_2001-2020_rev3.csv")

spdata==dsp

###################################
#analisarei a crime_rate
sp <- read_csv("ds_SSP_CrimeRate_SP-BR_utf8_2001-2019.csv",na = c("-", "None"),)
?read_csv
head(sp)
sp <- na.omit(sp)

sp$`Homicídio Doloso por 100 mil habitantes` <- gsub(",", ".", sp$`Homicídio Doloso por 100 mil habitantes`)
#sp$`Homicídio Doloso por 100 mil habitantes` <- gsub("None", NA, sp$`Homicídio Doloso por 100 mil habitantes`)

#sp <- sp[!sp$Ano=="None",]
sp19 <- sp[sp$Ano==2019,]

as=ggplot(data = sp19, mapping = aes(x = as.character(sp19$Regiao), y =as.numeric(sp19$`Homicídio Doloso por 100 mil habitantes`))) +
  geom_col()+coord_flip()+
  labs(title="Homicídio Doloso por 100 mil habitantes no estado de SP em 2019 (por região).",x="Região",y="Homicídios")+theme_few()+ylim(min = 0, max = max(as.numeric(sp$`Homicídio Doloso por 100 mil habitantes`)))
as

#ds_Sp<- read_csv("ds_SSP_PolicyProductivity_SP-BR_utf8_2001-2020_rev3.csv")

#later
names(sp19)[names(sp19) == "Cidade"] <- "NM_MUN"
#sp19$NM_MUNICIP <- toupper(sp19$NM_MUNICIP)

sp_city<-merge(muni,sp19,by="NM_MUN")
sp_city$NM_MUN<-factor(sp_city$NM_MUN)
sp_city <- sp_city[order(sp_city$`Homicídio Doloso por 100 mil habitantes`),] # order the data [very important!]


vcolor=c("#FFFFFF","#00FFF3","#0FBE09","#003AFF","red")
i_vcolor=c("red","#003AFF","#0FBE09","#00FFF3","#FFFFFF")


plot(sp_city,col=sp_city$`Homicídio Doloso por 100 mil habitantes`)
legend("topleft", inset=.05,lty=c(1,1), text.col=seq_along(sp_city$`Homicídio Doloso por 100 mil habitantes`),legend=sp_city$`Homicídio Doloso por 100 mil habitantes`, col=sp_city$`Homicídio Doloso por 100 mil habitantes`)
?plot

#krigagem
