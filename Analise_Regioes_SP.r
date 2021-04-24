#install.packages("tidyverse")
#install.packages("ggthemes")
#install.packages("readxl")
#install.packages("maptools")
#install.packages("sf")
#install.packages("plotly")
#install.packages("tmap")
#install.packages("geobr")
#install.packages("spdep")
#install.packages("sf")
#install.packages("crul")
#install.packages("rgdal")
library(crul)
library(sf)
library(spdep)
library(geobr)
library(tmap)
library(plotly)
library(tidyverse)
library(ggthemes)
library(readxl)
library(rgdal)
library(maptools)
#geobr https://cran.r-project.org/web/packages/geobr/vignettes/intro_to_geobr.html

#geobr
datasets <- list_geobr()

print(datasets, n=21)
###################################
#analisarei a crime_rate
sp <- read.csv2("https://raw.githubusercontent.com/marreapato/Crimes-No-Estado-de-Sao-Paulo/main/ds_SSP_CrimeRate_SP-BR_utf8_2001-2019.csv",sep = ",")

head(sp)

#commas to dot
for(i in 2:8){
sp[,i] <- as.numeric(gsub(",", ".", gsub("\\.", "", sp[,i])))
}

nrow(as.data.frame(table(sp$Cidade)))#645 municipios

sao_p <- sp[!sp$Ano=="None",]

nrow(as.data.frame(table(sao_p$Cidade)))#645 municipios porém n há dados para
#todos

sp19 <- sao_p[sao_p$Ano==2019,]

nrow(as.data.frame(table(sp19$Cidade)))#645 municipios porém n há dados para

as=ggplot(data = sp19, mapping = aes(x = as.character(sp19$Cidade), y =as.numeric(as.character(sp19$Homicídio.Doloso.por.100.mil.habitantes)))) +
  geom_col()+coord_flip()+
  labs(title="Homicídio Doloso por 100 mil habitantes no estado de SP em 2019 (por região).",x="Região",y="Homicídios")+theme_few()+ylim(min = 0, max = max(as.numeric(as.character(sp19$Homicídio.Doloso.por.100.mil.habitantes))))
as


########################################

#Mesorregioes#geo_br
mesos <- read_intermediate_region(code_intermediate = "SP",year=2019)
mesos$name_intermediate
table(sp19$Regiao)

# plot dos municípios de sp
ggplot() +
  geom_sf(data=mesos,aes(fill=mesos$name_intermediate), size=.15) +
  labs(subtitle="Mesorregiões de SP, 2019", size=8,fill="Regiões") +
  theme_minimal() + scale_fill_stata()

#mesorregioes
#https://www.saopaulo.sp.gov.br/spnoticias/governo-do-estado-atualiza-classificacao-do-plano-sp-sem-regressao-de-regioes/
#https://pt.wikipedia.org/wiki/Lista_de_mesorregi%C3%B5es_e_microrregi%C3%B5es_de_S%C3%A3o_Paulo#Mesorregi%C3%A3o_de_Bauru
marilia <- c("Arco-Íris",
             "Bastos",
             "Herculândia",
             "Iacri",
             "Queiroz",
             "Quintana",
             "Tupã","Álvaro de Carvalho",
              "Alvinlândia",
              "Echaporã",
              "Fernão",
              "Gália",
              "Garça",
              "Lupércio",
             "Marília",
              "Ocauçu",
              "Oriente",
              "Oscar Bressane",
              "Pompeia",
              "Vera Cruz","Manduri",
             "Óleo",
             "Ourinhos",
             "Piraju",
             "Ribeirão do Sul",
             "Salto Grande",
             "Santa Cruz do Rio Pardo",
             "São Pedro do Turvo",
             "Sarutaiá",
             "Taguaí",
             "Tejupá",
             "Timburi")



#mudando regiao de marilia
where <- match(sp19$Cidade,marilia)
sp19$Cidade[!is.na(where)]
sp19$Regiao[!is.na(where)] <-"Marília"

sp19$Regiao[sp19$Cidade=="Araraquara"]="Araraquara"
 
sp19$Regiao[sp19$Cidade=="São Carlos"]="Araraquara"

sp19$Regiao[sp19$Regiao=="Capital"]="São Paulo"  

sp19$Regiao[sp19$Regiao=="Santos"]="São Paulo"  

sp19$Regiao[sp19$Regiao=="Piracicaba"]="Campinas"#mapa wiki

sp19$Regiao[sp19$Regiao=="Grande São Paulo (exclui a Capital)"]="São Paulo" 

df=as.data.frame(table(sp19$Regiao))#igual
df$Var1
mesos_sp$name_intermediate

soma_homi_100mil <- aggregate(as.numeric(sp19$Homicídio.Doloso.por.100.mil.habitantes),by=list(sp19$Regiao),sum)
df$soma_homi_100mil <- soma_homi_100mil$x
furto <- aggregate(as.numeric(sp19$Furto.por.100.mil.habitantes),by=list(sp19$Regiao),sum)
df$furto <- furto$x
furto <- aggregate(as.numeric(sp19$Roubo.por.100.mil.habitantes),by=list(sp19$Regiao),sum)
df$roubo <- furto$x
furto <- aggregate(as.numeric(sp19$Furto.e.Roubo.de.Veículo.por.100.mil.habitantes),by=list(sp19$Regiao),sum)
df$furto_roubo_veic_habi <- furto$x
furto <- aggregate(as.numeric(sp19$Furto.por.100.mil.veículos),by=list(sp19$Regiao),sum)
df$furto_100mil_veic <- furto$x
furto <- aggregate(as.numeric(sp19$Roubo.por.100.mil.veículos),by=list(sp19$Regiao),sum)
df$roubo_100mil_veic <- furto$x
furto <- aggregate(as.numeric(sp19$Furto.e.Roubo.de.Veículo.por.100.mil.veículos),by=list(sp19$Regiao),sum)
df$furto_roubo_100mil_veic <- furto$x

mesos_sp <- dplyr::left_join(mesos, df, by = c("name_intermediate" = "Var1"))

#sf to sp
mesos_sp_sp <- as(mesos_sp,Class = "Spatial")
centroids.df <- as.data.frame(coordinates(mesos_sp_sp))

ggplot() +
  geom_sf(data=mesos_sp,aes(fill=as.numeric(as.character(mesos_sp$soma_homi_100mil))), color="Black", size=.15) +
  scale_fill_continuous()+
  labs(subtitle="Mesorregiões de SP", size=8,fill="Mortes por 100 mil") +
  theme_minimal()+theme(legend.position = "right")  +
  geom_text(aes(label = mesos_sp$name_intermediate, x = centroids.df$V1, y = centroids.df$V2))



ggplot() +
  geom_sf(data=mesos_sp,aes(fill=as.numeric(as.character(mesos_sp$furto))), color="Black", size=.15) +
  scale_fill_continuous()+
  labs(subtitle="Mesorregiões de SP", size=8,fill="Furtos por 100 mil") +
  theme_minimal()+theme(legend.position = "right")  +
  geom_text(aes(label = mesos_sp$name_intermediate, x = centroids.df$V1, y = centroids.df$V2))

ggplot() +
  geom_sf(data=mesos_sp,aes(fill=as.numeric(as.character(mesos_sp$roubo))), color="Black", size=.15) +
  scale_fill_continuous()+
  labs(subtitle="Mesorregiões de SP", size=8,fill="Roubos por 100 mil") +
  theme_minimal()+theme(legend.position = "right")  +
  geom_text(aes(label = mesos_sp$name_intermediate, x = centroids.df$V1, y = centroids.df$V2))


ggplot() +
  geom_sf(data=mesos_sp,aes(fill=as.numeric(as.character(mesos_sp$furto_roubo_veic_habi))), color="Black", size=.15) +
  scale_fill_continuous()+
  labs(subtitle="Mesorregiões de SP", size=8,fill="Furto e Roubos de Veículos por 100 mil") +
  theme_minimal()+theme(legend.position = "right")  +
  geom_text(aes(label = mesos_sp$name_intermediate, x = centroids.df$V1, y = centroids.df$V2))



ggplot() +
  geom_sf(data=mesos_sp,aes(fill=as.numeric(as.character(mesos_sp$furto_100mil_veic))), color="Black", size=.15) +
  scale_fill_continuous()+
  labs(subtitle="Mesorregiões de SP", size=8,fill="Furto por 100 mil Veículos") +
  theme_minimal()+theme(legend.position = "right")  +
  geom_text(aes(label = mesos_sp$name_intermediate, x = centroids.df$V1, y = centroids.df$V2))


ggplot() +
  geom_sf(data=mesos_sp,aes(fill=as.numeric(as.character(mesos_sp$roubo_100mil_veic))), color="Black", size=.15) +
  scale_fill_continuous()+
  labs(subtitle="Mesorregiões de SP", size=8,fill="Roubo por 100 mil Veículos") +
  theme_minimal()+theme(legend.position = "right")  +
  geom_text(aes(label = mesos_sp$name_intermediate, x = centroids.df$V1, y = centroids.df$V2))


ggplot() +
  geom_sf(data=mesos_sp,aes(fill=as.numeric(as.character(mesos_sp$furto_roubo_100mil_veic))), color="Black", size=.15) +
  scale_fill_continuous()+
  labs(subtitle="Mesorregiões de SP", size=8,fill="Furto e Roubo por 100 mil Veículos") +
  theme_minimal()+theme(legend.position = "right")  +
  geom_text(aes(label = mesos_sp$name_intermediate, x = centroids.df$V1, y = centroids.df$V2))



#moran using sp object
coor <- coordinates(mesos_sp_sp)

cartePPV3.knn <- knearneigh(coor, k=2) #2 neighbours
cartePPV3.nb <- knn2nb(cartePPV3.knn,row.names = mesos_sp_sp$name_intermediate)
PPV3.w <- nb2listw(cartePPV3.nb, style = "W", zero.policy = TRUE)#norm by row

plot(mesos_sp_sp, col='gray', border='blue', lwd=2,main= "Vizinhos")
plot(PPV3.w, coordinates(mesos_sp_sp), col='red', lwd=2, add=TRUE)#links
#mesos_sp_sp$soma_homi_100mil <- (mesos_sp_sp$soma_homi_100mil-min(mesos_sp_sp$soma_homi_100mil))/(max(mesos_sp_sp$soma_homi_100mil)-min(mesos_sp_sp$soma_homi_100mil))

#monthly
#death_pop
moran.plot(mesos_sp_sp$soma_homi_100mil, PPV3.w, zero.policy=TRUE)
moran.test(mesos_sp_sp$soma_homi_100mil,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=10000,mesos_sp_sp$soma_homi_100mil,PPV3.w,zero.policy = TRUE,na.action = na.omit)

#monthly death pop local
local.mi.prod<-localmoran(mesos_sp_sp$soma_homi_100mil, PPV3.w)

mesos_sp_sp$lmi<-local.mi.prod[,1]

mesos_sp_sp$lmi.p<-local.mi.prod[,5]

mesos_sp_sp$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                        ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))

#require("RColorBrewer")

#require("sp")

spplot(mesos_sp_sp, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "Homicidios")

#validated

#boxmap
quadrant <- vector(mode="numeric",length=nrow(local.mi.prod))

# centers the variable of interest around its mean
m.qualification <-mesos_sp_sp$soma_homi_100mil- mean(mesos_sp_sp$soma_homi_100mil)     

# centers the local Moran's around the mean
m.local <- local.mi.prod[,1] - mean(local.mi.prod[,1])    

# significance threshold
signif <- 0.05 

# builds a data quadrant
#positions
quadrant[m.qualification >0 & m.local>0] <- "HH"#AA  
quadrant[m.qualification <0 & m.local<0] <- "LL"#BB1      
quadrant[m.qualification <0 & m.local>0] <- "LH"#BA2
quadrant[m.qualification >0 & m.local<0] <- "HL"#AB3
#quadrant[local.mi.prod[,5]>signif] <- 0#you can choose not to run it
mesos_sp_sp$quad <- quadrant
# plot in r
#may

(aug <- ggplot(data = mesos_sp) +
    geom_sf(aes(fill = mesos_sp_sp$quad)) +
    scale_fill_manual(values=c("red","blue"))+theme(legend.position ="bottom",legend.title=element_text(size=14),legend.text=element_text(size=15),legend.direction = "horizontal",
                                                              axis.ticks.x=element_blank(), axis.text.x=element_blank())+
    labs(title = "Homicidios por 100 mil em 2019",fill="Cluster"))



