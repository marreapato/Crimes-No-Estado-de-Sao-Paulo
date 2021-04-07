#install.packages("tidyverse")
#install.packages("ggthemes")
#install.packages("readxl")
#install.packages("maptools")
#install.packages("sf")
#install.packages("plotly")
#install.packages("tmap")
#install.packages("geobr")
library(geobr)
library(tmap)
library(plotly)
library(tidyverse)
library(ggthemes)
library(readxl)
library(rgdal)
library(maptools)
library(sf)
#geobr https://cran.r-project.org/web/packages/geobr/vignettes/intro_to_geobr.html
#shps2020 pegos no portal:https://www.ibge.gov.br/geociencias/organizacao-do-territorio/15774-malhas.html?=&t=downloads
#mesorregiões e municipios
#lista: https://pt.wikipedia.org/wiki/Lista_de_mesorregi%C3%B5es_e_microrregi%C3%B5es_de_S%C3%A3o_Paulo#Mesorregi%C3%A3o_Macro_Metropolitana_Paulista

meso <- rgdal::readOGR("SP_Mesorregioes_2020.shp")#mesorregioes
micro <- rgdal::readOGR("SP_Microrregioes_2020.shp")

munis <- rgdal::readOGR("SP_Municipios_2020.shp")
#brmaps r
#geobr
datasets <- list_geobr()

print(datasets, n=21)
muni <- read_municipality(code_muni= "SP", year=2019)


###################################
#analisarei a crime_rate
sp <- read_csv("ds_SSP_CrimeRate_SP-BR_utf8_2001-2019.csv",na = c("-", "None"),)
?read_csv
head(sp)

#Tratamento
sp <- na.omit(sp)

sp$`Homicídio Doloso por 100 mil habitantes` <- gsub(",", ".", sp$`Homicídio Doloso por 100 mil habitantes`)
#sp$`Homicídio Doloso por 100 mil habitantes` <- gsub("None", NA, sp$`Homicídio Doloso por 100 mil habitantes`)

#sp <- sp[!sp$Ano=="None",]
sp19 <- sp[sp$Ano==2019,]

as=ggplot(data = sp19, mapping = aes(x = as.character(sp19$Regiao), y =as.numeric(sp19$`Homicídio Doloso por 100 mil habitantes`))) +
  geom_col()+coord_flip()+
  labs(title="Homicídio Doloso por 100 mil habitantes no estado de SP em 2019 (por região).",x="Região",y="Homicídios")+theme_few()+ylim(min = 0, max = max(as.numeric(sp$`Homicídio Doloso por 100 mil habitantes`)))
as


#juntando databases
# join the databases
sp19$Cidade
muni$name_muni

muni_sp <- dplyr::left_join(muni, sp19, by = c("name_muni" = "Cidade"))





# plot dos municípios de sp
ggplot() +
  geom_sf(data=muni_sp, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="Municipalidades de SP, 2007", size=8) +
  theme_minimal() 

# plot dos municípios de sp
ggplot() +
  geom_sf(data=muni_sp,aes(fill=as.numeric(as.character(muni_sp$`Homicídio Doloso por 100 mil habitantes`))), color="Black", size=.15) +
  scale_fill_continuous()+
  labs(subtitle="Municipalidades de SP", size=8,fill="Mortes por 100 mil") +
  theme_minimal()+theme(legend.position = "right") 

#ds_Sp<- read_csv("ds_SSP_PolicyProductivity_SP-BR_utf8_2001-2020_rev3.csv")

#later
names(sp19)[names(sp19) == "Cidade"] <- "NM_MUN"
#sp19$NM_MUNICIP <- toupper(sp19$NM_MUNICIP)

sp_city<-merge(munis,sp19,by="NM_MUN")
sp_city$NM_MUN<-factor(sp_city$NM_MUN)
sp_city <- sp_city[order(sp_city$`Homicídio Doloso por 100 mil habitantes`),] # order the data [very important!]


vcolor=c("#FFFFFF","#00FFF3","#0FBE09","#003AFF","red")
i_vcolor=c("red","#003AFF","#0FBE09","#00FFF3","#FFFFFF")

sp_city$NM_MUN <- as.factor(sp_city$NM_MUN)
levels(sp_city$NM_MUN)

plot(sp_city,col=sp_city$`Homicídio Doloso por 100 mil habitantes`)
legend("topleft", inset=.05,lty=c(1,1), text.col=seq_along(sp_city$`Homicídio Doloso por 100 mil habitantes`),legend=sp_city$`Homicídio Doloso por 100 mil habitantes`, col=sp_city$`Homicídio Doloso por 100 mil habitantes`)
?plot


#krigagem // kriging
