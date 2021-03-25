#install.packages("tidyverse")
#install.packages("ggthemes")
#install.packages("readxl")
#install.packages("maptools")
#install.packages("sf")
#install.packages("plotly")
#install.packages("tmap")
#install.packages("rgdal")
library(tmap)
library(plotly)
library(tidyverse)
library(ggthemes)
library(readxl)
library(rgdal)
library(maptools)
library(sf)
#Taxas
cidades <- rgdal::readOGR("35MUE250GC_SIR.shp")#meu shapefile

sp <- read_csv("ds_SSP_CrimeRate_SP-BR_utf8_2001-2019.csv",na = c("-", "None"),)

sp <- na.omit(sp)

sp$`Homicídio Doloso por 100 mil habitantes` <- gsub(",", ".", sp$`Homicídio Doloso por 100 mil habitantes`)
#sp$`Homicídio Doloso por 100 mil habitantes` <- gsub("None", NA, sp$`Homicídio Doloso por 100 mil habitantes`)
sp$`Furto por 100 mil habitantes` <- as.character(sp$`Furto por 100 mil habitantes`)

sp$`Furto por 100 mil habitantes` <- str_replace(sp$`Furto por 100 mil habitantes`,"1.","1")
sp$`Furto por 100 mil habitantes` <- str_replace(sp$`Furto por 100 mil habitantes`,"2.","2")
sp$`Furto por 100 mil habitantes` <- str_replace(sp$`Furto por 100 mil habitantes`,"3.","3")

sp$`Furto por 100 mil habitantes` <- sub("(.{1})(.*)", "\\1.\\2", sp$`Furto por 100 mil habitantes`)
sp$`Furto por 100 mil habitantes` <- as.numeric(as.character(sp$`Furto por 100 mil habitantes`))

#sp <- sp[!sp$Ano=="None",]
sp19 <- sp[sp$Ano==2019,]

taxa_hom_19=ggplot(data = sp19, mapping = aes(x = as.character(sp19$Regiao), y =as.numeric(sp19$`Homicídio Doloso por 100 mil habitantes`))) +
  geom_col()+coord_flip()+
  labs(title="Homicídio Doloso por 100 mil habitantes no estado de SP em 2019 (por região).",x="Região",y="Homicídios")+theme_few()+ylim(min = 0, max = max(as.numeric(sp$`Homicídio Doloso por 100 mil habitantes`)))
taxa_hom_19

taxa_furto_19=ggplot(data = sp19, mapping = aes(x = as.character(sp19$Regiao), y =as.numeric(sp19$`Furto por 100 mil habitantes`))) +
  geom_col()+coord_flip()+
  labs(title="Furto por 100 mil habitantes no estado de SP em 2019 (por região).",x="Região",y="Homicídios")+theme_few()+ylim(min = 0, max = max(as.numeric(sp$`Homicídio Doloso por 100 mil habitantes`)))
taxa_furto_19



#####################################################################################3

#juntando o dataset de crime rate com o shp
names(sp19)[names(sp19) == "Cidade"] <- "NM_MUNICIP"
sp19$NM_MUNICIP <- toupper(sp19$NM_MUNICIP)

sp_city<-merge(cidades,sp19,by="NM_MUNICIP")
sp_city$NM_MUNICIP<-factor(sp_city$NM_MUNICIP)
sp_city <- sp_city[order(sp_city$`Homicídio Doloso por 100 mil habitantes`),] # order the data [very important!]


vcolor=c("#FFFFFF","#00FFF3","#0FBE09","#003AFF","red")
i_vcolor=c("red","#003AFF","#0FBE09","#00FFF3","#FFFFFF")


plot(sp_city,col=sp_city$`Homicídio Doloso por 100 mil habitantes`)
legend("topleft", inset=.05,lty=c(1,1), text.col=seq_along(sp_city$`Homicídio Doloso por 100 mil habitantes`),legend=sp_city$`Homicídio Doloso por 100 mil habitantes`, col=sp_city$`Homicídio Doloso por 100 mil habitantes`)
?plot

#########################################################################################################################################################################################################################################



#brmaps r

#anual_sp <- data.frame(read_xlsx("Anual-Estado-SP.xlsx"))
#FRV = Furto e Roubo de Veiculos
#anual_sp <- anual_sp[,-6]

#spdata<- read_csv("ds_SSP_PolicyProductivity_SP-BR_utf8_2001-2020_rev3.csv")
