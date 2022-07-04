#
# Calculation mean attributions per Salmonella serotype over all years (2007-2019)
# Author: Annemieke Mulder
#

# Libraries
library(writexl)
library(boot)
library(DescTools) # necessary for the calculation of confidence intervals (CI's) for multinomial proportions
library(tidyverse)

# Salmonella serotypes
# set working directory
setwd('...')

# Read data
# Salmonella attributions
attr_total_salmonella <- read.xlsx(".../Results/Source probabilities.xls", 1)
# Salmonella isolates
salmonella_isolates <- read.xlsx(".../Results/salmonella_serotypes_v3.xlsx", 1) #13814 cases

# ---------------------------------------------------------------------------------------
# Data cleaning

# select years 2007-2019
attr_total_salmonella <- subset(attr_total_salmonella, (jaar > 2006 & jaar < 2020))

# Adjust different names for same serotype
attr_total_salmonella$serotype_naam[attr_total_salmonella$serotype_naam == "Paratyphi B. var. Java"] <- "Paratyphi B variatie Java"
attr_total_salmonella$serotype_naam[attr_total_salmonella$serotype_naam == "SI 1,4,5,12:i:- "] <- "1,4,5,12:i:-"
attr_total_salmonella$serotype_naam[attr_total_salmonella$serotype_naam == "1,4,[5],12:i:-"] <- "1,4,5,12:i:-"

# Remove typhoidal salmonellas
attr_total_salmonella <- subset(attr_total_salmonella, !serotype_naam == 'Paratyphi B variatie Java')

# ----------------------------------------------------------------------------------------
# Calculate mean attributions per serotype per livestock group and merge datasets

# Mean of attributions per serotype over all years - pigs
attr_sal_serotype_pigs <- aggregate(x = attr_total_salmonella$Pig,                # Specify data column
                      by = list(attr_total_salmonella$serotype),              # Specify group indicator
                       FUN = mean)                            # Specify function (i.e. sum)

# Mean relative attribution of number of years per serotype - pigs
attr_sal_serotype_pigs_yrs <- aggregate(x = attr_total_salmonella$Pig,                # Specify data column
                                    by = list(attr_total_salmonella$serotype),              # Specify group indicator
                                    FUN = mean)                            # Specify function (i.e. sum)


attr_sal_serotype_cattle <- aggregate(x = attr_total_salmonella$Cattle,                # Specify data column
                                    by = list(attr_total_salmonella$serotype),              # Specify group indicator
                                    FUN = mean)                            # Specify function (i.e. sum)

attr_sal_serotype_chicken <- aggregate(x = attr_total_salmonella$Chicken,                # Specify data column
                                    by = list(attr_total_salmonella$serotype),              # Specify group indicator
                                    FUN = mean)                            # Specify function (i.e. sum)

attr_sal_serotype_layer <- aggregate(x = attr_total_salmonella$Layer,                # Specify data column
                                    by = list(attr_total_salmonella$serotype),              # Specify group indicator
                                    FUN = mean)                            # Specify function (i.e. sum)

attr_sal_serotype_reptiles <- aggregate(x = attr_total_salmonella$Reptiles,                # Specify data column
                                    by = list(attr_total_salmonella$serotype),              # Specify group indicator
                                    FUN = mean)                            # Specify function (i.e. sum)

# r merge by rownames
# merge pigs and cattle
attr_sal_serotype <- merge(attr_sal_serotype_pigs, attr_sal_serotype_cattle, by = 'Group.1')
attr_sal_serotype$pigs <- attr_sal_serotype$x.x
attr_sal_serotype$cattle <- attr_sal_serotype$x.y
attr_sal_serotype <- subset(attr_sal_serotype, select = c(Group.1,pigs,cattle))
# merge with chicken
attr_sal_serotype <- merge(attr_sal_serotype, attr_sal_serotype_chicken, by = 'Group.1')
attr_sal_serotype$chicken <- attr_sal_serotype$x
attr_sal_serotype <- subset(attr_sal_serotype, select = c(Group.1,pigs,cattle,chicken))
# merge with layer
attr_sal_serotype <- merge(attr_sal_serotype, attr_sal_serotype_layer, by = 'Group.1')
attr_sal_serotype$layer <- attr_sal_serotype$x
attr_sal_serotype <- subset(attr_sal_serotype, select = c(Group.1,pigs,cattle,chicken,layer))
# merge with reptiles
attr_sal_serotype <- merge(attr_sal_serotype, attr_sal_serotype_reptiles, by = 'Group.1')
attr_sal_serotype$reptiles <- attr_sal_serotype$x
attr_sal_serotype <- subset(attr_sal_serotype, select = c(Group.1,pigs,cattle,chicken,layer,reptiles))

# Adapt column name serotypes
attr_sal_serotype$serotype <- attr_sal_serotype$Group.1
attr_sal_serotype <- subset(attr_sal_serotype, select = c(serotype,pigs,cattle,chicken,layer,reptiles))

# Add serotype name to dataset
sal_ser_name <- subset(attr_total_salmonella, select = c(serotype,serotype_naam))
attr_sal_serotype <- merge(attr_sal_serotype, sal_ser_name, by = 'serotype')

# ------------------------------------------------------------------------------------------------------------------------------------
# Data cleaning merged dataset isolates

# check naming of species in both datasets controleren, make them uniform
salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "Enteritidis faagtype  Pt 1"] <- "Enteritidis Pt 1"
salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "Enteritidis faagtype  Pt 11"] <- "Enteritidis Pt 11"
salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "Enteritidis faagtype  Pt 13"] <- "Enteritidis Pt 13"
salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "Enteritidis faagtype  Pt 13a"] <- "Enteritidis Pt 13a"
salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "Enteritidis faagtype  Pt 14b"] <- "Enteritidis Pt 14b"
salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "Enteritidis faagtype  Pt 1b"] <- "Enteritidis Pt 1b"
salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "Enteritidis faagtype  Pt 2"] <- "Enteritidis Pt 2"
salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "Enteritidis faagtype  Pt 21"] <- "Enteritidis Pt 21"
salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "Enteritidis faagtype  Pt 3"] <- "Enteritidis Pt 3"
salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "Enteritidis faagtype  Pt 4"] <- "Enteritidis Pt 4"
salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "Enteritidis faagtype  Pt 6"] <- "Enteritidis Pt 6"
salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "Enteritidis faagtype  Pt 6a"] <- "Enteritidis Pt 6a"
salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "Enteritidis faagtype  Pt 8"] <- "Enteritidis Pt 8"
salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "Enteritidis a-typisch faagpatroon"] <- "Enteritidis : ARS"
salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "Enteritidis geen faagreacties"] <- "Enteritidis : OS"

salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "S. Typhimurium a-typisch faagpatroon"] <- "Typhimurium : ARS"
salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "S. Typhimurium faagtype 111"] <- "Typhimurium : 111"
salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "S. Typhimurium faagtype 353"] <- "Typhimurium : 353"
salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "S. Typhimurium faagtype 506"] <- "Typhimurium : 506"
salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "S. Typhimurium faagtype 507"] <- "Typhimurium : 507"
salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "S. Typhimurium faagtype 508"] <- "Typhimurium : 508"
salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "S. Typhimurium faagtype 510"] <- "Typhimurium : 510"
salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "S. Typhimurium faagtype 561"] <- "Typhimurium : 561"
salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "S. Typhimurium faagtype 651"] <- "Typhimurium : 651"
salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "S. Typhimurium faagtype 90"] <- "Typhimurium : 90"
salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "S. Typhimurium geen faagreacties"] <- "Typhimurium : OS"

#salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "1,4,[5],12:i:-"] <- "1,4,5,12:i:-"


# Biochemisch Salm. Rough anders schrijven
salmonella_isolates$serotype.omschrijving[salmonella_isolates$serotype.omschrijving == "Biochemisch Salm. rough"] <- "Biochemisch Salm. Rough"

# keep only rows containing serotype names in attribution dataframe that are present in the salmonella isolates dataframe, which are unique in serotype nr + values of attributions
attr_sal_serotype <- attr_sal_serotype[(attr_sal_serotype$serotype %in% salmonella_isolates$serotype),]
attr_sal_serotype <- attr_sal_serotype[!duplicated(attr_sal_serotype[ , c("serotype", "pigs", "cattle", "chicken", "layer", "reptiles")]), ] 

#------------------------------------------------------------------------------------------------------------------------------
# Create dataset with overview most important source 

# Select only the sources from dataframe
attr_sal_serotype2 <- subset(attr_sal_serotype, select = c(pigs,cattle,chicken,layer,reptiles))

# Calculate max of each row and attach column names to them
source <- as.data.frame(colnames(attr_sal_serotype2)[max.col(attr_sal_serotype2, ties.method = "first")])

# Combine datasets attributions and sources
serotype_source <- cbind(attr_sal_serotype,source)

# Rename column source dataset
serotype_source$source <- serotype_source$`colnames(attr_sal_serotype2)[max.col(attr_sal_serotype2, ties.method = "first")]`
serotype_source <- subset(serotype_source, select = c(serotype,serotype_naam,pigs,cattle,chicken,layer,reptiles,source))

# Check numbers!
# 13825 cases in total in salmonella isolates
salmonella_isolates_source <- merge(salmonella_isolates, serotype_source,  by = "serotype") # 461 cases removed that were not found in source, 13353 cases remain
salmonella_isolates_source <- subset(salmonella_isolates_source, select = c(serotype,Freq,pigs,cattle,chicken,layer,reptiles,source))
salmonella_isolates_source <- unique(salmonella_isolates_source)
 
salmonella_reptiles <- subset(salmonella_isolates_source, source == "reptiles") # 1261 cases
salmonella_reptiles_1source <- subset(salmonella_reptiles, reptiles == 1) # 298 cases # not shown in article (85 serovars), 13055 cases remain.
salmonella_reptiles_2source <- subset(salmonella_reptiles, reptiles < 1 & reptiles > 0) # 963 cases subdivided over other sources (24 serovars)
# Numbers per new source instead of reptiles:
# Pigs = 431 cases
# Chicken = 369 cases
# Layer = 157 cases
# Cattle = 6 cases

salmonella_pigs <- subset(salmonella_isolates_source, source == "pigs") # 6305 cases + 431 additional reptile cases = 6736 cases in total
salmonella_cattle <- subset(salmonella_isolates_source, source == "cattle") # 317 cases + 6 additional reptile cases = 323 cases in total
salmonella_meatpoultry <- subset(salmonella_isolates_source, source == "chicken") # 1157 cases + 369 additional reptile cases = 1526 cases in total
salmonella_layer <- subset(salmonella_isolates_source, source == "layer") # 4313 cases + 157 additional reptile cases = 4470 cases in total

# In total: 6736+686+1314+4319 = 13055 cases in analysis!

#------------------------------------------------------------------------------------------------------------------------------

# Export dataset for comparison with literature sources
# Used as input for table S3 in manuscript
write_xlsx(serotype_source, '.../Results/serotype_source_v4.xlsx')





