library(ggplot2)
library(tidyverse)
library(caret)
library(xlsx)


## reading file--------------------------------
df <- read.csv("All_data_MIC.csv", header = T, sep = ";")
dilutions <- read.csv("dilutions_antifungi.csv", header =T, sep =";")

##subsetting data frame------------------------

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

##essential agreement(EA) and category agreement (CA)--------------------------------------------------
#attribution of SIR for agreement
##Caspofungin--------------------------
#YO
caspo$YO_CAS_SIR <- agreement_bp(caspo$species, "C. albicans", caspo$YO_CAS, caspo$YO_CAS_SIR, 6,8, dilutions$caspofungin_YO)
caspo$YO_CAS_SIR <- agreement_bp(caspo$species, "C. glabrata", caspo$YO_CAS, caspo$YO_CAS_SIR, 5,7, dilutions$caspofungin_YO)
#VMN
caspo$VMIC_CAS_SIR <- agreement_bp(caspo$species, "C. albicans", caspo$VMIC_CAS, caspo$VMIC_CAS_SIR, 4, 4, dilutions$caspofungin_MN)
caspo$VMIC_CAS_SIR <- agreement_bp(caspo$species, "C. glabrata", caspo$VMIC_CAS, caspo$VMIC_CAS_SIR, 5, 5, dilutions$caspofungin_MN)
#MN
caspo$MIC_CAS_SIR <- agreement_bp(caspo$species, "C. albicans", caspo$MIC_CAS, caspo$MIC_CAS_SIR, 4,4, dilutions$caspofungin_MN)
caspo$MIC_CAS_SIR <- agreement_bp(caspo$species, "C. glabrata", caspo$MIC_CAS, caspo$MIC_CAS_SIR, 5,5, dilutions$caspofungin_MN)
#Fluconazole------------------------------
#YO
fluco$YO_FZ_SIR <- agreement_bp(fluco$species, "C. albicans", fluco$YO_FZ, fluco$YO_FZ_SIR, 5,7, dilutions$fluconazole_YO)
fluco$YO_FZ_SIR <- agreement_bp(fluco$species, "C. glabrata", fluco$YO_FZ, fluco$YO_FZ_SIR, '?','?', dilutions$fluconazole_YO)
#VMN
fluco$VMIC_FZ_SIR <- agreement_bp(fluco$species, "C. albicans", fluco$VMIC_FZ, fluco$VMIC_FZ_SIR, 5, 6, dilutions$fluconazole_MN)
fluco$VMIC_FZ_SIR <- agreement_bp(fluco$species, "C. glabrata", fluco$VMIC_FZ, fluco$VMIC_FZ_SIR, '?', 8, dilutions$fluconazole_MN)
#MN
fluco$MIC_FZ_SIR <- agreement_bp(fluco$species, "C. albicans", fluco$MIC_FZ, fluco$MIC_FZ_SIR, 5,6, dilutions$fluconazole_MN)
fluco$MIC_FZ_SIR <- agreement_bp(fluco$species, "C. glabrata", fluco$MIC_FZ, fluco$MIC_FZ_SIR, '?',8, dilutions$fluconazole_MN)



##creation of the confusion matrix--------------------------------------
#Caspofungin----------------------
# for(i in 1:length(caspo$YO_CAS_SIR)){
#   for(j in 1:length(caspo$VMIC_CAS_SIR))
#     
# }
predcaspoVMIC <- caspo$VMIC_CAS_SIR
predcaspoMIC <- caspo$MIC_CAS_SIR
refcaspo <- caspo$YO_CAS_SIR
caspomatrix_YO_VMIC <- confusionMatrix(predcaspoVMIC, refcaspo, dnn = c("YeastOne", "Visual MICRONAUT"))
caspomatrix_YO_MIC <- confusionMatrix(predcaspoMIC, refcaspo, dnn = c("YeastOne", "photo MICRONAUT"))
caspomatrix_VMIC_MIC <- confusionMatrix(predcaspoMIC, predcaspoVMIC, dnn = c("visual MICRONAUT", "photo MICRONAUT"))
#Fluconazole---------------------

predflucoVMIC <- fluco$VMIC_FZ_SIR[fluco$species == "C. albicans"]
predflucoMIC <- fluco$MIC_FZ_SIR[fluco$species == "C. albicans"]
reffluco <- fluco$YO_FZ_SIR[fluco$species== "C. albicans"]
flucomatrix_YO_VMIC <- confusionMatrix(predflucoVMIC, reffluco, dnn = c("YeastOne", "Visual MICRONAUT"))
flucomatrix_YO_MIC <- confusionMatrix(predflucoMIC, reffluco, dnn = c("YeastOne", "photo MICRONAUT"))
flucomatrix_VMIC_MIC <- confusionMatrix(predflucoMIC, predflucoVMIC, dnn = c("visual MICRONAUT", "photo MICRONAUT"))
# 
# write.table(flucomatrix_YO_VMIC$table, file="flucomatric_YO_VMIC.txt", sep = "\t")
# write.table(flucomatrix_YO_MIC$table, file="flucomatric_YO_VMIC.txt", sep = "\t", append = T)
# write.table(flucomatrix_VMIC_MIC$table, file="flucomatric_YO_VMIC.txt", sep = "\t", append = T)








##Function definition--------------------------------------------------

agreement_bp <- function(data_species, species, data_ttt, data_SIR, refpositionbpS,refpositionbpR, dilutions){ 
  #call fct with 
  #data_ttt: the type of treatment you want to calculate the SIR
  #data_SIR: the column to output the value, exemple : 
  # refpositionbp =
  #
  
  for (i in 1:length(data_ttt)){
    
    if(data_species[i] == species){ #verify that only the right species get SIR
      
      if(data_ttt[i] < dilutions[refpositionbpS -3]){ # assign highly sensitive if below 3 dilutions from breakpoint
        data_SIR[i] <- 'HS'
        #
      } else if(data_ttt[i] == dilutions[refpositionbpS -3] | data_ttt[i] == dilutions[refpositionbpS -2] ){ #assigne sensitive at -3 or -2 dilutions below breakpoint
        data_SIR[i] <- 'S'
        
      } else if(data_ttt[i] == dilutions[refpositionbpS -1] | data_ttt[i] == dilutions[refpositionbpS +1] | data_ttt[i] == dilutions[refpositionbpR -1] | data_ttt[i] == dilutions[refpositionbpR +1]){#assigne sensitive at -1 or +1 dilutions from breakpoint
        data_SIR[i] <- 'I'
        
      }else if(data_ttt[i] == dilutions[refpositionbpR +2] | data_ttt[i] == dilutions[refpositionbpR +3]){ #assigne resistant at +3 or -2 dilutions abovebreakpoint
        data_SIR[i] <- 'R'
        
      }else if(data_ttt[i] > dilutions[refpositionbpR +3]){ # assign highly resistant if 3 dilutions above breakpoint
        data_SIR[i] <- 'HR'
        
      
      }
    }
  }
  return(as.factor(data_SIR))
}

#******************************3
matrixmaker <- function(ref_data_ttt, ){}

