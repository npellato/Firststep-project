library(ggplot2)
theme_set(
  theme_classic() +
    theme(legend.position = "right")
)
library(mdthemes)
library(cowplot)
library(tidyverse)
library(patchwork)

## reading file--------------------------------
df <- read.csv("All_data_MIC.csv", header = T, sep = ";")

##subsetting data frame------------------------
#df_alb <- subset(df, species == "C. albicans") #subset based on species

df_names <- subset(df, select = c("straincode", "species", "date")) #subset only the specified headers
#selction FZ creation of df which contains data of each antifungi 
#creation of df assiociating antifungi names of sample and MIC data

datafluco <- df %>% select(contains("FZ")) ; fluco <- cbind(df_names, datafluco) 
dataanidu <- df %>% select(contains("AND")); anidu <- cbind(df_names, dataanidu)
datamicaf <- df %>% select(contains("MC")); micaf <- cbind(df_names, datamicaf)
datacaspo <- df %>% select(contains("CAS")); caspo <- cbind(df_names, datacaspo)
dataflucy <- df %>% select(contains("FC")); flucy <- cbind(df_names, dataflucy)
dataposac <- df %>% select(contains("PZ")); posac <- cbind(df_names, dataposac)
datavoric <- df %>% select(contains("VOR")) ;voric <- cbind(df_names, datavoric)
dataitrac <- df %>% select(contains("IZ")) ;itrac <- cbind(df_names, dataitrac)
dataampho <- df %>% select(contains("AB")) ;ampho <- cbind(df_names, dataampho)


##Transforming and plotting subsetted datas***********************************

## *******************Anidulafungin **********************--------------------------

#adding SIR
#anidu$MIC_AND[is.na(anidu$MIC_AND)] <- 0
anidu$YO_AND_SIR <- breakpoint(anidu$species, "C. albicans", anidu$YO_AND, anidu$YO_AND_SIR, 0.25, 1)
anidu$VMIC_AND_SIR <- breakpoint(anidu$species, "C. albicans", anidu$VMIC_AND, anidu$VMIC_AND_SIR, 0.031, 0.031)
anidu$MIC_AND_SIR <- breakpoint(anidu$species, "C. albicans", anidu$MIC_AND, anidu$MIC_AND_SIR, 0.031, 0.031)
anidu$YO_AND_SIR <- breakpoint(anidu$species, "C. glabrata", anidu$YO_AND, anidu$YO_AND_SIR, 0.12, 0.5)
anidu$VMIC_AND_SIR <- breakpoint(anidu$species, "C. glabrata", anidu$VMIC_AND, anidu$VMIC_AND_SIR, 0.06, 0.06)
anidu$MIC_AND_SIR <- breakpoint(anidu$species, "C. glabrata", anidu$MIC_AND, anidu$MIC_AND_SIR, 0.06, 0.06)
#anidu$YO_AND_SIR <- breakpoint(anidu$species, "C. lusitaniae", anidu$YO_AND, anidu$YO_AND_SIR, , )
#anidu$VMIC_AND_SIR <- breakpoint(anidu$species, "C. lusitaniae", anidu$VMIC_AND, anidu$VMIC_AND_SIR, 0.03, 0.03)
#anidu$MIC_AND_SIR <- breakpoint(anidu$species, "C. lusitaniae", anidu$MIC_AND, anidu$MIC_AND_SIR, 0.03, 0.03)

#Log transformation***************
anidu$YO_AND_SIR <- as.factor(anidu$YO_AND_SIR); anidu$VMIC_AND_SIR <- as.factor(anidu$VMIC_AND_SIR); anidu$MIC_AND_SIR <- as.factor(anidu$MIC_AND_SIR)

anidu$YO_AND <- log2(anidu$YO_AND)
anidu$VMIC_AND <- log2(anidu$VMIC_AND) 
anidu$MIC_AND <- log2(anidu$MIC_AND)

#plot AND Albicans *************************
alb_anidu <- anidu[anidu$species == "C. albicans", ]

alb_AND_plotYO <- ggplot(alb_anidu, aes(x = date)) +
  geom_point(aes(y=YO_AND, color=YO_AND_SIR),size = 2) +
  #geom_hline(yintercept=log2(2), color = "blue", linetype = "dashed")+
  geom_hline(yintercept=log2(0.031), color="red", linetype="dashed")+
  mdthemes::md_theme_classic()+
  labs(title = "*C. albicans* - Anidulafungin", x = "date of measurment", y = "log2(MIC) YeastOne values", color ="CLSI SIR")

alb_AND_plotMN <- ggplot(alb_anidu, aes())+
  geom_point(aes(x=date, y=VMIC_AND, color=VMIC_AND_SIR, shape = 0), size = 2)+
  geom_point(aes(x=date, y=MIC_AND, color = MIC_AND_SIR, shape = 20), size = 2)+
  scale_shape_identity()+
  geom_hline(yintercept=log2(0.25), color = "blue", linetype = "dashed")+
  geom_hline(yintercept=log2(1), color="red", linetype="dashed")+
  mdthemes::md_theme_classic()+
  labs(title = "*C. albicans* - Anidunlafungin", x = "date of measurment", y = "log2(MIC) MICRONAUT values", color ="EUCAST SIR", shape ="Reading methods")
#plot AND glabrata***********
glab_anidu <- anidu[anidu$species == "C. glabrata", ]

glab_AND_plotYO <- ggplot(glab_anidu, aes(x = date)) +
  geom_point(aes(y=YO_AND, color=YO_AND_SIR),size = 2) +
  #geom_hline(yintercept=log2(2), color = "blue", linetype = "dashed")+
  geom_hline(yintercept=log2(0.06), color="red", linetype="dashed")+
  mdthemes::md_theme_classic()+
  labs(title = "*C. glabrata* - Anidulafungin", x = "date of measurment", y = "log2(MIC) YeastOne values", color ="CLSI SIR")

glab_AND_plotMN <- ggplot(glab_anidu, aes( x=date))+
  geom_point(aes(y=VMIC_AND, color=VMIC_AND_SIR, shape = 0), size = 2)+
  geom_point(aes(y = MIC_AND, color = MIC_AND_SIR, shape = 20), size = 2)+
  geom_hline(yintercept=log2(0.12), color = "blue", linetype = "dashed")+
  geom_hline(yintercept=log2(0.5), color="red", linetype="dashed")+
  mdthemes::md_theme_classic()+
  scale_shape_identity()+
  labs(title = "*C. glabrata* - Anidunlafungin", x = "date of measurment", y = "log2(MIC) MICRONAUT values", color ="EUCAST SIR", shape ="Reading methods")


AND_plots <- plot_grid(alb_AND_plotYO, alb_AND_plotMN, glab_AND_plotYO, glab_AND_plotMN)
AND_plots
## *******************Caspofungin***************************------------------------
#adding SIR
#caspo$MIC_CAS[is.na(caspo$MIC_CAS)] <- 0
caspo$YO_CAS_SIR <- breakpoint(caspo$species, "C. albicans", caspo$YO_CAS, caspo$YO_CAS_SIR, 0.25, 1)
caspo$VMIC_CAS_SIR <- breakpoint(caspo$species, "C. albicans", caspo$VMIC_CAS, caspo$VMIC_CAS_SIR, 0.031, 0.031)
caspo$MIC_CAS_SIR <- breakpoint(caspo$species, "C. albicans", caspo$MIC_CAS, caspo$MIC_CAS_SIR, 0.031, 0.031)
caspo$YO_CAS_SIR <- breakpoint(caspo$species, "C. glabrata", caspo$YO_CAS, caspo$YO_CAS_SIR, 0.12, 0.5)
caspo$VMIC_CAS_SIR <- breakpoint(caspo$species, "C. glabrata", caspo$VMIC_CAS, caspo$VMIC_CAS_SIR, 0.06, 0.06)
caspo$MIC_CAS_SIR <- breakpoint(caspo$species, "C. glabrata", caspo$MIC_CAS, caspo$MIC_CAS_SIR, 0.06, 0.06)
#caspo$YO_CAS_SIR <- breakpoint(caspo$species, "C. lusitaniae", caspo$YO_CAS, caspo$YO_CAS_SIR, , )
#caspo$VMIC_CAS_SIR <- breakpoint(caspo$species, "C. lusitaniae", caspo$VMIC_CAS, caspo$VMIC_CAS_SIR, 0.03, 0.03)
#caspo$MIC_CAS_SIR <- breakpoint(caspo$species, "C. lusitaniae", caspo$MIC_CAS, caspo$MIC_CAS_SIR, 0.03, 0.03)

#Log transformation***************
caspo$YO_CAS_SIR <- as.factor(caspo$YO_CAS_SIR); caspo$VMIC_CAS_SIR <- as.factor(caspo$VMIC_CAS_SIR); caspo$MIC_CAS_SIR <- as.factor(caspo$MIC_CAS_SIR)

caspo$YO_CAS <- log2(caspo$YO_CAS)
caspo$VMIC_CAS <- log2(caspo$VMIC_CAS) 
caspo$MIC_CAS <- log2(caspo$MIC_CAS)

#plot CAS Albicans *************************
alb_caspo <- caspo[caspo$species == "C. albicans", ]

alb_CAS_plotYO <- ggplot(alb_caspo, aes(x = date)) +
  geom_point(aes(y=YO_CAS, color=YO_CAS_SIR),size = 2) +
  #geom_hline(yintercept=log2(2), color = "blue", linetype = "dashed")+
  geom_hline(yintercept=log2(0.031), color="red", linetype="dashed")+
  mdthemes::md_theme_classic()+
  labs(title = "*C. albicans* - Caspofugin", x = "date of measurment", y = "log2(MIC) YeastOne values", color ="CLSI SIR")

alb_CAS_plotMN <- ggplot(alb_caspo, aes())+
  geom_point(aes(x=date, y=VMIC_CAS, color=VMIC_CAS_SIR, shape = 0), size = 2)+
  geom_point(aes(x=date, y=MIC_CAS, color = MIC_CAS_SIR, shape = 20), size = 2)+
  scale_shape_identity()+
  geom_hline(yintercept=log2(0.25), color = "blue", linetype = "dashed")+
  geom_hline(yintercept=log2(1), color="red", linetype="dashed")+
  mdthemes::md_theme_classic()+
  labs(title = "*C. albicans* - Caspofungin", x = "date of measurment", y = "log2(MIC) MICRONAUT values", color ="EUCAST SIR", shape ="Reading methods")
#plot CAS glabrata***********
glab_caspo <- caspo[caspo$species == "C. glabrata", ]

glab_CAS_plotYO <- ggplot(glab_caspo, aes(x = date)) +
  geom_point(aes(y=YO_CAS, color=YO_CAS_SIR),size = 2) +
  #geom_hline(yintercept=log2(2), color = "blue", linetype = "dashed")+
  geom_hline(yintercept=log2(0.06), color="red", linetype="dashed")+
  mdthemes::md_theme_classic()+
  labs(title = "*C. glabrata* - Caspofugin", x = "date of measurment", y = "log2(MIC) YeastOne values", color ="CLSI SIR")

glab_CAS_plotMN <- ggplot(glab_caspo, aes( x=date))+
  geom_point(aes(y=VMIC_CAS, color=VMIC_CAS_SIR, shape = 0), size = 2)+
  geom_point(aes(y = MIC_CAS, color = MIC_CAS_SIR, shape = 20), size = 2)+
  geom_hline(yintercept=log2(0.12), color = "blue", linetype = "dashed")+
  geom_hline(yintercept=log2(0.5), color="red", linetype="dashed")+
  mdthemes::md_theme_classic()+
  scale_shape_identity()+
  labs(title = "*C. glabrata* - Caspofungin", x = "date of measurment", y = "log2(MIC) MICRONAUT values", color ="EUCAST SIR", shape ="Reading methods")


CAS_plots <- plot_grid(alb_CAS_plotYO, alb_CAS_plotMN, glab_CAS_plotYO, glab_CAS_plotMN)
CAS_plots
## *******************Micafungin***************************-----------------------

#adding SIR
#micaf$MIC_MC[is.na(micaf$MIC_MC)] <- 0
micaf$YO_MC_SIR <- breakpoint(micaf$species, "C. albicans", micaf$YO_MC, micaf$YO_MC_SIR, 0.25, 1)
micaf$VMIC_MC_SIR <- breakpoint(micaf$species, "C. albicans", micaf$VMIC_MC, micaf$VMIC_MC_SIR, 0.016, 0.016)
micaf$MIC_MC_SIR <- breakpoint(micaf$species, "C. albicans", micaf$MIC_MC, micaf$MIC_MC_SIR, 0.016, 0.016)

micaf$YO_MC_SIR <- breakpoint(micaf$species, "C. glabrata", micaf$YO_MC, micaf$YO_MC_SIR, 0.06, 0.25)
micaf$VMIC_MC_SIR <- breakpoint(micaf$species, "C. glabrata", micaf$VMIC_MC, micaf$VMIC_MC_SIR, 0.031, 0.031)
micaf$MIC_MC_SIR <- breakpoint(micaf$species, "C. glabrata", micaf$MIC_MC, micaf$MIC_MC_SIR, 0.032, 0.032)

#micaf$YO_MC_SIR <- breakpoint(micaf$species, "C. lusitaniae", micaf$YO_MC, micaf$YO_MC_SIR, , )
#micaf$VMIC_MC_SIR <- breakpoint(micaf$species, "C. lusitaniae", micaf$VMIC_MC, micaf$VMIC_MC_SIR, 0.03, 0.03)
#micaf$MIC_MC_SIR <- breakpoint(micaf$species, "C. lusitaniae", micaf$MIC_MC, micaf$MIC_MC_SIR, 0.03, 0.03)

#Log transformation***************
micaf$YO_MC_SIR <- as.factor(micaf$YO_MC_SIR); micaf$VMIC_MC_SIR <- as.factor(micaf$VMIC_MC_SIR); micaf$MIC_MC_SIR <- as.factor(micaf$MIC_MC_SIR)

micaf$YO_MC <- log2(micaf$YO_MC)
micaf$VMIC_MC <- log2(micaf$VMIC_MC) 
micaf$MIC_MC <- log2(micaf$MIC_MC)

#plot MC Albicans *************************
alb_micaf <- micaf[micaf$species == "C. albicans", ]

alb_MC_plotYO <- ggplot(alb_micaf, aes(x = date)) +
  geom_point(aes(y=YO_MC, color=YO_MC_SIR),size = 2) +
  #geom_hline(yintercept=log2(2), color = "blue", linetype = "dashed")+
  geom_hline(yintercept=log2(0.016), color="red", linetype="dashed")+
  mdthemes::md_theme_classic()+
  labs(title = "*C. albicans* - Micafungin", x = "date of measurment", y = "log2(MIC) YeastOne values", color ="CLSI SIR")

alb_MC_plotMN <- ggplot(alb_micaf, aes())+
  geom_point(aes(x=date, y=VMIC_MC, color=VMIC_MC_SIR, shape = 0), size = 2)+
  geom_point(aes(x=date, y=MIC_MC, color = MIC_MC_SIR, shape = 20), size = 2)+
  scale_shape_identity()+
  geom_hline(yintercept=log2(0.25), color = "blue", linetype = "dashed")+
  geom_hline(yintercept=log2(1), color="red", linetype="dashed")+
  mdthemes::md_theme_classic()+
  labs(title = "*C. albicans* - Micafungin", x = "date of measurment", y = "log2(MIC) MICRONAUT values", color ="EUCAST SIR", shape ="Reading methods")
#plot MC glabrata***********
glab_micaf <- micaf[micaf$species == "C. glabrata", ]

glab_MC_plotYO <- ggplot(glab_micaf, aes(x = date)) +
  geom_point(aes(y=YO_MC, color=YO_MC_SIR),size = 2) +
  #geom_hline(yintercept=log2(2), color = "blue", linetype = "dashed")+
  geom_hline(yintercept=log2(0.032), color="red", linetype="dashed")+
  mdthemes::md_theme_classic()+
  labs(title = "*C. glabrata* - Micafungin", x = "date of measurment", y = "log2(MIC) YeastOne values", color ="CLSI SIR")

glab_MC_plotMN <- ggplot(glab_micaf, aes( x=date))+
  geom_point(aes(y=VMIC_MC, color=VMIC_MC_SIR, shape = 0), size = 2)+
  geom_point(aes(y = MIC_MC, color = MIC_MC_SIR, shape = 20), size = 2)+
  geom_hline(yintercept=log2(0.06), color = "blue", linetype = "dashed")+
  geom_hline(yintercept=log2(0.25), color="red", linetype="dashed")+
  mdthemes::md_theme_classic()+
  scale_shape_identity()+
  labs(title = "*C. glabrata* - Micafungin", x = "date of measurment", y = "log2(MIC) MICRONAUT values", color ="EUCAST SIR", shape ="Reading methods")


MC_plots <- plot_grid(alb_MC_plotYO, alb_MC_plotMN, glab_MC_plotYO, glab_MC_plotMN)
MC_plots
## *******************Amphotericin B****need bp values***********************---------------------
# *******************Flucytosine******need to do*********************-----------------------

## *******************Fluconazole*****************************----------------------
#adding SIR

fluco$YO_FZ_SIR <- breakpoint(fluco$species, "C. albicans", fluco$YO_FZ, fluco$YO_FZ_SIR, 2, 8)
fluco$VMIC_FZ_SIR <- breakpoint(fluco$species, "C. albicans", fluco$VMIC_FZ, fluco$VMIC_FZ_SIR, 2, 4)
fluco$MIC_FZ_SIR <- breakpoint(fluco$species, "C. albicans", fluco$MIC_FZ, fluco$MIC_FZ_SIR, 2, 4)

fluco$YO_FZ_SIR <- breakpoint(fluco$species, "C. glabrata", fluco$YO_FZ, fluco$YO_FZ_SIR, 32, 64)
fluco$YO_FZ_SIR[fluco$species == "C. glabrata" & fluco$YO_FZ_SIR == "S"] <- 'I'
fluco$VMIC_FZ_SIR <- breakpoint(fluco$species, "C. glabrata", fluco$VMIC_FZ, fluco$VMIC_FZ_SIR, 0.001, 16)
fluco$MIC_FZ_SIR <- breakpoint(fluco$species, "C. glabrata", fluco$MIC_FZ, fluco$MIC_FZ_SIR, 0.001, 16)
#fluco$YO_FZ_SIR <- breakpoint(fluco$species, "C. lusitaniae", fluco$YO_FZ, fluco$YO_FZ_SIR, , )
#fluco$VMIC_FZ_SIR <- breakpoint(fluco$species, "C. lusitaniae", fluco$VMIC_FZ, fluco$VMIC_FZ_SIR, 0.03, 0.03)
#fluco$MIC_FZ_SIR <- breakpoint(fluco$species, "C. lusitaniae", fluco$MIC_FZ, fluco$MIC_FZ_SIR, 0.03, 0.03)
#Log transformation**************************
fluco$YO_FZ_SIR <- as.factor(fluco$YO_FZ_SIR); fluco$VMIC_FZ_SIR <- as.factor(fluco$VMIC_FZ_SIR); fluco$MIC_FZ_SIR <- as.factor(fluco$MIC_FZ_SIR)
fluco$YO_FZ <- log2(fluco$YO_FZ)
fluco$VMIC_FZ <- log2(fluco$VMIC_FZ) 
fluco$MIC_FZ <- log2(fluco$MIC_FZ)


#plot FZ Albicans****************************
alb_fluco <- fluco[fluco$species == "C. albicans", ]
alb_FZ_plotYO <- ggplot(alb_fluco, aes()) +
  geom_point(aes(date, YO_FZ, color=YO_FZ_SIR),size = 2) +
  geom_hline(yintercept=log2(2), color = "blue", linetype = "dashed")+
  geom_hline(yintercept=log2(4), color="red", linetype="dashed")+
  mdthemes::md_theme_classic()+
  labs(title = "*C. albicans* - Fluconazole", x = "date of measurment", y = "log2(MIC) YeastOne  values", color ="CLSI SIR")

alb_FZ_plotMN <- ggplot(alb_fluco, aes( x=date))+
  geom_point(aes(y=VMIC_FZ, color=VMIC_FZ_SIR, shape = 0), size = 2)+
  geom_point(aes(y=MIC_FZ, color=MIC_FZ_SIR, shape = 20), size = 2)+
  geom_hline(yintercept=log2(2), color = "blue", linetype = "dashed")+
  geom_hline(yintercept=log2(8), color="red", linetype="dashed")+
  mdthemes::md_theme_classic()+
  scale_shape_identity()+
  labs(title = "*C. albicans* - Fluconazole", x = "date of measurment", y = "log2(MIC) MICRONAUT  values", color ="EUCAST SIR", shape ="Reading methods")

#plot FZ glabrata
#plot FZ glabrata****************************
glab_fluco <- fluco[fluco$species == "C. glabrata", ]
glab_FZ_plotYO <- ggplot(glab_fluco, aes()) +
  geom_point(aes(date, YO_FZ, color=YO_FZ_SIR),size = 2) +
  geom_hline(yintercept=log2(0.001), color = "blue", linetype = "dashed")+
  geom_hline(yintercept=log2(16), color="red", linetype="dashed")+
  mdthemes::md_theme_classic()+
  labs(title = "*C. glabrata* - Fluconazole", x = "date of measurment", y = "log2(MIC) YeastOne  values", color ="CLSI SIR")

glab_FZ_plotMN <- ggplot(glab_fluco, aes( x=date))+
  geom_point(aes(y=VMIC_FZ, color=VMIC_FZ_SIR, shape = 0), size = 2)+
  geom_point(aes(y=MIC_FZ, color=MIC_FZ_SIR, shape = 20), size = 2)+
  geom_hline(yintercept=log2(32), color = "blue", linetype = "dashed")+
  geom_hline(yintercept=log2(64), color="red", linetype="dashed")+
  mdthemes::md_theme_classic()+
  scale_shape_identity()+
  labs(title = "*C. glabrata* - Fluconazole", x = "date of measurment", y = "log2(MIC) MICRONAUT  values", color ="EUCAST SIR", shape ="Reading methods")


FZ_plot <- plot_grid(alb_FZ_plotYO, alb_FZ_plotMN, glab_FZ_plotYO, glab_FZ_plotMN)
FZ_plot

## *******************Posaconazole*****need breakpoint values**********************-----------------------
#adding SIR

posac$YO_PZ_SIR <- breakpoint(posac$species, "C. albicans", posac$YO_PZ, posac$YO_PZ_SIR, '?', '?')
posac$VMIC_PZ_SIR <- breakpoint(posac$species, "C. albicans", posac$VMIC_PZ, posac$VMIC_PZ_SIR, 0.06, 0.06)
posac$MIC_PZ_SIR <- breakpoint(posac$species, "C. albicans", posac$MIC_PZ, posac$MIC_PZ_SIR, 0.06, 0.06)

posac$YO_PZ_SIR <- breakpoint(posac$species, "C. glabrata", posac$YO_PZ, posac$YO_PZ_SIR, '?', '?')
#posac$YO_PZ_SIR[posac$species == "C. glabrata" & posac$YO_PZ_SIR == "S"] <- 'I'
posac$VMIC_PZ_SIR <- breakpoint(posac$species, "C. glabrata", posac$VMIC_PZ, posac$VMIC_PZ_SIR, '?', '?')
posac$MIC_PZ_SIR <- breakpoint(posac$species, "C. glabrata", posac$MIC_PZ, posac$MIC_PZ_SIR, '?', '?')
#posac$YO_PZ_SIR <- breakpoint(posac$species, "C. lusitaniae", posac$YO_PZ, posac$YO_PZ_SIR, , )
#posac$VMIC_PZ_SIR <- breakpoint(posac$species, "C. lusitaniae", posac$VMIC_PZ, posac$VMIC_PZ_SIR, 0.03, 0.03)
#posac$MIC_PZ_SIR <- breakpoint(posac$species, "C. lusitaniae", posac$MIC_PZ, posac$MIC_PZ_SIR, 0.03, 0.03)
#Log transformation**************************
posac$YO_PZ_SIR <- as.factor(posac$YO_PZ_SIR); posac$VMIC_PZ_SIR <- as.factor(posac$VMIC_PZ_SIR); posac$MIC_PZ_SIR <- as.factor(posac$MIC_PZ_SIR)
posac$YO_PZ <- log2(posac$YO_PZ)
posac$VMIC_PZ <- log2(posac$VMIC_PZ) 
posac$MIC_PZ <- log2(posac$MIC_PZ)


#plot PZ Albicans****************************
alb_posac <- posac[posac$species == "C. albicans", ]
alb_PZ_plotYO <- ggplot(alb_posac, aes()) +
  geom_point(aes(date, YO_PZ, color=YO_PZ_SIR),size = 2) +
  geom_hline(yintercept=log2(2), color = "blue", linetype = "dashed")+
  geom_hline(yintercept=log2(4), color="red", linetype="dashed")+
  mdthemes::md_theme_classic()+
  labs(title = "*C. albicans* - posaconazole", x = "date of measurment", y = "log2(MIC) YeastOne  values", color ="CLSI SIR")

alb_PZ_plotMN <- ggplot(alb_posac, aes( x=date))+
  geom_point(aes(y=VMIC_PZ, color=VMIC_PZ_SIR, shape = 0), size = 2)+
  geom_point(aes(y=MIC_PZ, color=MIC_PZ_SIR, shape = 20), size = 2)+
  geom_hline(yintercept=log2(2), color = "blue", linetype = "dashed")+
  geom_hline(yintercept=log2(8), color="red", linetype="dashed")+
  mdthemes::md_theme_classic()+
  scale_shape_identity()+
  labs(title = "*C. albicans* - posaconazole", x = "date of measurment", y = "log2(MIC) MICRONAUT  values", color ="EUCAST SIR", shape ="Reading methods")

#plot PZ glabrata
#plot PZ glabrata****************************
glab_posac <- posac[posac$species == "C. glabrata", ]
glab_PZ_plotYO <- ggplot(glab_posac, aes()) +
  geom_point(aes(date, YO_PZ, color=YO_PZ_SIR),size = 2) +
  geom_hline(yintercept=log2(0.001), color = "blue", linetype = "dashed")+
  geom_hline(yintercept=log2(16), color="red", linetype="dashed")+
  mdthemes::md_theme_classic()+
  labs(title = "*C. glabrata* - posaconazole", x = "date of measurment", y = "log2(MIC) YeastOne  values", color ="CLSI SIR")

glab_PZ_plotMN <- ggplot(glab_posac, aes( x=date))+
  geom_point(aes(y=VMIC_PZ, color=VMIC_PZ_SIR, shape = 0), size = 2)+
  geom_point(aes(y=MIC_PZ, color=MIC_PZ_SIR, shape = 20), size = 2)+
  geom_hline(yintercept=log2(32), color = "blue", linetype = "dashed")+
  geom_hline(yintercept=log2(64), color="red", linetype="dashed")+
  mdthemes::md_theme_classic()+
  scale_shape_identity()+
  labs(title = "*C. glabrata* - posaconazole", x = "date of measurment", y = "log2(MIC) MICRONAUT  values", color ="EUCAST SIR", shape ="Reading methods")


PZ_plot <- plot_grid(alb_PZ_plotYO, alb_PZ_plotMN, glab_PZ_plotYO, glab_PZ_plotMN)
PZ_plot
## *******************Voriconazole******need bp value glabra*********************-----------------------
#adding SIR

voric$YO_VOR_SIR <- breakpoint(voric$species, "C. albicans", voric$YO_VOR, voric$YO_VOR_SIR, 0.12, 1)
voric$VMIC_VOR_SIR <- breakpoint(voric$species, "C. albicans", voric$VMIC_VOR, voric$VMIC_VOR_SIR, 0.06, 0.25)
voric$MIC_VOR_SIR <- breakpoint(voric$species, "C. albicans", voric$MIC_VOR, voric$MIC_VOR_SIR, 0.06, 0.25)

voric$YO_VOR_SIR <- breakpoint(voric$species, "C. glabrata", voric$YO_VOR, voric$YO_VOR_SIR, '?', '?')
voric$YO_VOR_SIR[voric$species == "C. glabrata" & voric$YO_VOR_SIR == "S"] <- 'I'
voric$VMIC_VOR_SIR <- breakpoint(voric$species, "C. glabrata", voric$VMIC_VOR, voric$VMIC_VOR_SIR, '?', '?')
voric$MIC_VOR_SIR <- breakpoint(voric$species, "C. glabrata", voric$MIC_VOR, voric$MIC_VOR_SIR, '?', '?')
#voric$YO_VOR_SIR <- breakpoint(voric$species, "C. lusitaniae", voric$YO_VOR, voric$YO_VOR_SIR, , )
#voric$VMIC_VOR_SIR <- breakpoint(voric$species, "C. lusitaniae", voric$VMIC_VOR, voric$VMIC_VOR_SIR, 0.03, 0.03)
#voric$MIC_VOR_SIR <- breakpoint(voric$species, "C. lusitaniae", voric$MIC_VOR, voric$MIC_VOR_SIR, 0.03, 0.03)
#Log transformation**************************
voric$YO_VOR_SIR <- as.factor(voric$YO_VOR_SIR); voric$VMIC_VOR_SIR <- as.factor(voric$VMIC_VOR_SIR); voric$MIC_VOR_SIR <- as.factor(voric$MIC_VOR_SIR)
voric$YO_VOR <- log2(voric$YO_VOR)
voric$VMIC_VOR <- log2(voric$VMIC_VOR) 
voric$MIC_VOR <- log2(voric$MIC_VOR)


#plot VOR Albicans****************************
alb_voric <- voric[voric$species == "C. albicans", ]
alb_VOR_plotYO <- ggplot(alb_voric, aes()) +
  geom_point(aes(date, YO_VOR, color=YO_VOR_SIR),size = 2) +
  geom_hline(yintercept=log2(2), color = "blue", linetype = "dashed")+
  geom_hline(yintercept=log2(4), color="red", linetype="dashed")+
  mdthemes::md_theme_classic()+
  labs(title = "*C. albicans* - voriconazole", x = "date of measurment", y = "log2(MIC) YeastOne  values", color ="CLSI SIR")

alb_VOR_plotMN <- ggplot(alb_voric, aes( x=date))+
  geom_point(aes(y=VMIC_VOR, color=VMIC_VOR_SIR, shape = 0), size = 2)+
  geom_point(aes(y=MIC_VOR, color=MIC_VOR_SIR, shape = 20), size = 2)+
  geom_hline(yintercept=log2(2), color = "blue", linetype = "dashed")+
  geom_hline(yintercept=log2(8), color="red", linetype="dashed")+
  mdthemes::md_theme_classic()+
  scale_shape_identity()+
  labs(title = "*C. albicans* - voriconazole", x = "date of measurment", y = "log2(MIC) MICRONAUT  values", color ="EUCAST SIR", shape ="Reading methods")

#plot VOR glabrata
#plot VOR glabrata****************************
glab_voric <- voric[voric$species == "C. glabrata", ]
glab_VOR_plotYO <- ggplot(glab_voric, aes()) +
  geom_point(aes(date, YO_VOR, color=YO_VOR_SIR),size = 2) +
  geom_hline(yintercept=log2(0.001), color = "blue", linetype = "dashed")+
  geom_hline(yintercept=log2(16), color="red", linetype="dashed")+
  mdthemes::md_theme_classic()+
  labs(title = "*C. glabrata* - voriconazole", x = "date of measurment", y = "log2(MIC) YeastOne  values", color ="CLSI SIR")

glab_VOR_plotMN <- ggplot(glab_voric, aes( x=date))+
  geom_point(aes(y=VMIC_VOR, color=VMIC_VOR_SIR, shape = 0), size = 2)+
  geom_point(aes(y=MIC_VOR, color=MIC_VOR_SIR, shape = 20), size = 2)+
  geom_hline(yintercept=log2(32), color = "blue", linetype = "dashed")+
  geom_hline(yintercept=log2(64), color="red", linetype="dashed")+
  mdthemes::md_theme_classic()+
  scale_shape_identity()+
  labs(title = "*C. glabrata* - voriconazole", x = "date of measurment", y = "log2(MIC) MICRONAUT  values", color ="EUCAST SIR", shape ="Reading methods")


VOR_plot <- plot_grid(alb_VOR_plotYO, alb_VOR_plotMN) #glab_VOR_plotYO, glab_VOR_plotMN)
VOR_plot
## *******************Itraconazole*******need to do ********************-----------------------


##essential agreement(EA) and category agreement (CA)--------------------------------------------------
#attribution of SIR for agreement


##creation breakpoint vector function ---------------------------- 

#function for creating breakponts


breakpoint <- function(data_species, species, data_ttt, data_SIR, st, rt){ 
  #call fct with 
  #data_ttt: the type of treatment you want to calculate the SIR
  #data_SIR: the column to output the value, 
  #st: the sensitive threshold value, 
  #rt the resistant treshold value
  
    for (i in 1:length(data_ttt)){
      
      if(data_species[i] == species){ #verify that only the right species get SIR
        
        if(data_ttt[i] <= st){ # assign sensitive if below or equals to st
          data_SIR[i] <- 'S'
          #
        } else if(data_ttt[i] > st & data_ttt[i] < rt ){ #assigne Intermediate if between st FZ rt
          data_SIR[i] <- 'I'
          
        } else if(data_ttt[i] >= rt){ #assign Resistante if higher or equals to rt
          data_SIR[i] <- 'R'
          
        }
      }
      
    }
  return(data_SIR)
}

#to do the SIR attribution to a vector species (do not work as intended for now !)
breakpoint_allSpecies <- function(allspecies,allst, allrt ){
  for(sp in 1:length(allspecies)){
    print(allspecies[sp])
    SIR <- breakpoint_CLSI(anidu$species, allspecies[sp], anidu$YO_FZ, anidu$YO_FZ_SIR, allst[sp], allrt[sp])
  }
  return(SIR)
}



