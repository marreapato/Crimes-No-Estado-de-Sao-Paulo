#install.packages("tidyverse")
#install.packages("ggthemes")
#install.packages("readxl")
library(tidyverse)
library(ggthemes)
library(readxl)

anual_sp <- data.frame(read_xlsx("Anual-Estado-SP.xlsx"))
#FRV = Furto e Roubo de Veiculos
anual_sp <- anual_sp[,-6]

spdata<- read_csv("ds_SSP_PolicyProductivity_SP-BR_utf8_2001-2020_rev3.csv")

dsp <- read_csv("ds_SSP_MonthlyOccurences_SP-BR_utf8_2001-2020_rev3.csv")

spdata==dsp

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

ds_Sp<- read_csv("ds_SSP_PolicyProductivity_SP-BR_utf8_2001-2020_rev3.csv")



