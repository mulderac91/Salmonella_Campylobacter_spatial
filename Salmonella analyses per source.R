
# Load packages
library(readxl)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(RColorBrewer)
library(Matrix)
library(RANN)
library(tidyverse)
library(colorspace)
library(writexl)

# Set years
years <- 2007:2019
n.years <- length(years)

# Read pc4 population numbers for each year
pop.pc4.year.data <- vector(mode = "list", length = n.years)
for (i in seq_len(n.years)) {
  pop.pc4.year.data[[i]] <- read.delim(file = paste0(".../Data/...BevolkingPerPostcode_1januari", years[i], ".txt")) %>%
    # Select relevant columns
    select(pc4, mannen_0tot5:mannen_95tot100, vrouwen_0tot5:vrouwen_95tot100) %>%
    # Reshape population numbers into long format
    gather(key = "sex_agecat", value = "population", -pc4) %>%
    # Add year
    mutate(year = years[i])
}

# Read pc4 polygons for each year
pc4.year.sf <- vector(mode = "list", length = n.years)
for (i in seq_len(n.years)) {
  pc4.year.sf[[i]] <- st_read(
    dsn = paste0(".../Data/pc4 - poly/", years[i]),
    layer = paste0("pc4_", years[i]),
    type = 6,
    quiet = TRUE) %>%
    # Add year
    mutate(year = years[i]) %>%
    # Set CRS to 29882 (ignore warning)
    st_set_crs(28992)
}

# Rename pc4 workaround
pc4.year.sf <- lapply(X = pc4.year.sf, FUN = function(data) {
  # Rename the different original pc4 names (pc4, PC4, postcode, etc.) to pc4
  # Be sure pc4 is in the first column
  names(data)[1] <- "pc4"
  #rename(pc4 = PC4) %>%
  data %>%
    arrange(pc4)
})

# Check double postal codes
sapply(X = pc4.year.sf, FUN = function(data) data$pc4 %>% duplicated %>% sum)
# Are there any duplicated records? Which?
pc4.year.sf[[i]]$pc4 %>% duplicated %>% any
pc4.year.sf$pc4 %>% duplicated %>% which

# Read pc6 points and population (2016)
pop.pc6.sf <- read.delim(file = ".../Data/pc6/pc6_2016.txt") %>%
  # Rename inwoners -> population
  rename(population = inwoners) %>%
  # Drop pc6 with 0 inhabitants
  filter(population > 0) %>%
  # Convert to sf object
  st_as_sf(coords = c("x", "y")) %>%
  # Set CRS to 29882 (ignore warning)
  st_set_crs(28992)


# Read hexagonal reference map
# Depending on which size of the hexagonal areas you choose for the analyses

# Area is 90 km^2
hex.sf <- readRDS(file = ".../data/.../hex_090.rds")

# # # Area is 50 km^2
#hex.sf <- readRDS(file = ".../data/.../hex_050.rds")

# Area is 25 km^2
#hex.sf <- readRDS(file = ".../data/.../hex_025.rds")

# Area is 10 km^2
#hex.sf <- readRDS(file = ".../data/.../hex_010.rds")

#read salmonella pc4 data
Humane_isolaten_data_Salmonella_2005_2020_combined <- read_excel(".../Data/.../__.xls")
salmonella_pc4 <- Humane_isolaten_data_Salmonella_2005_2020_combined

#rename columns
salmonella_pc4 <- salmonella_pc4 %>%
  rename(birth.date = Geboortedat.) %>%
  rename(onset.date = `Creatie dat.`) %>%
  rename(sex = S) %>%
  rename(pc4 = `Postcode nr.`) %>%
  rename(pc2 = `Postcode Lt`) %>%
  rename(rap.date = `Rap. dat.`) %>%
  rename(patient.nr = `pat. nr.`)

# Calculate age of patients
# check class from date in dataframe
class(salmonella_pc4$onset.date)
class(salmonella_pc4$birth.date)
class(salmonella_pc4$rap.date)
# change class from date to 'date' format
salmonella_pc4$onset.date <- as.Date(salmonella_pc4$onset.date, format('%d/%m/%Y'))
salmonella_pc4$birth.date <- as.Date(salmonella_pc4$birth.date, format('%Y-%m-%d'))
salmonella_pc4$rap.date <- as.Date(salmonella_pc4$rap.date, format('%Y-%m-%d'))
#calculate interval for age calculation
salmonella_pc4$Interval_data <- salmonella_pc4$onset.date - salmonella_pc4$birth.date
#convert to years as integer
salmonella_pc4$age <- as.integer(salmonella_pc4$Interval_data / 365)

#make year column and filter 2007-2019
salmonella_pc4$year <- format(salmonella_pc4$onset.date, format = "%Y")
salmonella_pc4 <- filter(salmonella_pc4, salmonella_pc4$year >= 2007 & salmonella_pc4$year <= 2019)

# Drop S. Typhi, Paratyphi A, Paratyphi B, Paratyphi B variatie Java, they are thypoidal and are not causing gastro intestinal complaints, we are only looking at NTS
salmonella_pc4 <- subset(salmonella_pc4, !`serotype omschrijving`=='Typhi') #8002
salmonella_pc4 <- subset(salmonella_pc4, !`serotype omschrijving`=='Paratyphi A') #1318
salmonella_pc4 <- subset(salmonella_pc4, !`serotype omschrijving`=='Paratyphi B') #9003
salmonella_pc4 <- subset(salmonella_pc4, !`serotype omschrijving`=='Paratyphi B variatie Java') #1199
salmonella_pc4 <- subset(salmonella_pc4, !`serotype omschrijving`=='Typhi 9,12:j:z66') #8903
salmonella_pc4 <- subset(salmonella_pc4, !`serotype omschrijving`=='S. Typhi 9,12:z66:-') #8904

# Drop rows from salmonella_isolates for which it is certain they are no salmonella or it is not certain if they are salmonella
salmonella_pc4 <- subset(salmonella_pc4, !`serotype omschrijving`=='Cultuur verontreinigd') #9995
salmonella_pc4 <- subset(salmonella_pc4, !`serotype omschrijving`=='Geen Salmonella') #9999
salmonella_pc4 <- subset(salmonella_pc4, !`serotype omschrijving`=='n.v.t /not applicable') #9994
salmonella_pc4 <- subset(salmonella_pc4, !`serotype omschrijving`=='Na herhaalde pogingen geen groei') #9990

# Drop non-motiles
salmonella_pc4 <- subset(salmonella_pc4, !`serotype omschrijving`=='4,[5],12:-:- (non-motile)') #7002
salmonella_pc4 <- subset(salmonella_pc4, !`serotype omschrijving`=='9,12:-:- (non-motile)') #7006
salmonella_pc4 <- subset(salmonella_pc4, !Serotype == 7008)
salmonella_pc4 <- subset(salmonella_pc4, !Serotype == 7003)
salmonella_pc4 <- subset(salmonella_pc4, !Serotype == 7005)

# This is the Salmonella dataset as input for main Salmonella descriptives table! # 19771 cases as start

#select data based on column "serotype omschrijving"
#alter some descriptions of different serotypes
salmonella_pc4$`serotype omschrijving`[salmonella_pc4$Serotype == 7221] <- "Typhimurium"
salmonella_pc4$`serotype omschrijving`[salmonella_pc4$Serotype == 7236] <- "Typhimurium"
salmonella_pc4$`serotype omschrijving`[salmonella_pc4$Serotype == 7237] <- "Typhimurium"
salmonella_pc4$`serotype omschrijving`[salmonella_pc4$Serotype == 7039] <- "Typhimurium"

#remove individual cases linked to food sources S. Typhimurium and S. Enteritidis
#load dataset with cases linked to food sources S. Typhimurium and S. Enteritidis
outbreaks <- read_excel(".../Data/__.xlsx", sheet = "Sheet1")
#convert Monsternr in salmonella_pc4 to numeric value
salmonella_pc4$Monsternr <- as.numeric(salmonella_pc4$Monsternr)
#drop rows containing Monsternr. that are linked to food sources
salmonella_pc4 <- salmonella_pc4[!(salmonella_pc4$Monsternr %in% outbreaks$monsternr),]

#remove individual cases linked to food sources non S. Typhimurium and S. Enteritidis
#load dataset with cases linked to food sources non S. Typhimurium and S. Enteritidis
outbreaks.2 <- read_excel(".../Data/__.xlsx", sheet = "Sheet1")
#drop rows containing Monsternr. that are linked to food sources
salmonella_pc4 <- salmonella_pc4[!(salmonella_pc4$Monsternr %in% outbreaks.2$monsternr),]

# 18,085 cases left Salmonella overall

#select relevant columns (patient.nr, onset.date, sex, pc4, age, sex, rap.date, Serotype, Buitenland)
salmonella_pc4 <- salmonella_pc4 %>% select(patient.nr, onset.date, rap.date, sex, pc4, age, `serotype omschrijving`, Serotype, year, Buitenland)
#remove rows in age with left over NA values
salmonella_pc4 <- salmonella_pc4[!is.na(salmonella_pc4$age),] # 18,004 cases left Salmonella overall

#remove duplicated rows
salmonella_pc4 <- distinct(salmonella_pc4) # 17885 cases left Salmonella overall

# Filter unique pat. nr. 6 months apart
salmonella_pc4 <- salmonella_pc4 %>% arrange(patient.nr)
salmonella_pc4 <- salmonella_pc4 %>%
  mutate(daysbetween = onset.date - lag(onset.date))

#make duplicate column, but first change class of patient nr to numeric (was character)
salmonella_pc4$patient.nr <- as.numeric(salmonella_pc4$patient.nr)
#then make duplicate column by subtracting
salmonella_pc4 <- salmonella_pc4 %>%
  mutate(duplicate = patient.nr - lag(patient.nr))

#now remove rows if: days between < 183 (6 months) and duplicate == 0
salmonella_pc4 <- salmonella_pc4[!(salmonella_pc4$daysbetween <= 183 & salmonella_pc4$duplicate == 0), ]
#remove rows with only NAs in columns
salmonella_pc4 <- salmonella_pc4[rowSums(is.na(salmonella_pc4)) != ncol(salmonella_pc4), ]

# 17,213 cases left for Salmonella overall

#select relevant columns (onset.date, sex, pc4, age, sex, rap.date, Serotype)
salmonella_pc4 <- salmonella_pc4 %>% select(patient.nr, onset.date, rap.date, sex, pc4, age, `serotype omschrijving`, Serotype, year, Buitenland)

#remove foreign isolates
salmonella_pc4 <- salmonella_pc4 %>% filter(is.na(Buitenland)) # 14877 cases left Salmonella overall
#Make ? in sex NA and remove NA's
sum(is.na(salmonella_pc4))
salmonella_pc4$sex[salmonella_pc4$sex == '?'] <- NA
salmonella_pc4 <- salmonella_pc4[!is.na(salmonella_pc4$sex), ] # 14314 cases left

#filter on PC4 > 0 and < 9999
salmonella_pc4 <- salmonella_pc4[(salmonella_pc4$pc4 > 0 & salmonella_pc4$pc4 < 9999), ] # 14,160 cases left
# remove cases from which pc4s are unknown
salmonella_pc4 <- salmonella_pc4[!is.na(salmonella_pc4$pc4), ] # 13814 cases left

#select relevant columns (onset.date, sex, pc4, age, sex, rap.date, Serotype omschrijving)
salmonella_pc4 <- salmonella_pc4 %>% select(patient.nr, onset.date, rap.date, sex, pc4, age, `serotype omschrijving`, Serotype, year)

#remove rows with left over NA values
salmonella_pc4 <- na.omit(salmonella_pc4)
#sort by pc4
salmonella_pc4 <- salmonella_pc4 %>% arrange(pc4)
#convert to dataframe (was tibble)
salmonella_pc4 <- salmonella_pc4 %>% as.data.frame

#select relevant columns (onset.date, sex, pc4, age, sex, rap.date, Serotype omschrijving)
salmonella_pc4 <- salmonella_pc4 %>% select(onset.date, rap.date, sex, pc4, age, `serotype omschrijving`, Serotype, year)

# Select proper data to work with: salmonella serovars per source animal

# Layers
#salmonella_pc4$Serotype <- as.numeric(salmonella_pc4$Serotype)
# salmonella_pc4 <- salmonella_pc4[salmonella_pc4$Serotype %in% c("1010", "1043", "1068", "1071", "1219", "1257",
                                                                # "1272", "1359", "1389", "1393", "1403", "4011",
                                                                # "4012", "4401", "4655", "5100", "5121", "5122", 
                                                                # "5125", "5133", "5134", "5135", "5142", "5144",
                                                                # "5145", "5146", "5147", "5150", "5157", "5160",
                                                                # "5167", "5198", "5199", "5209", "7045", "1085",
                                                                # "1167", "1230", "1308", "1329", "7048"), ]
# 
# # Chicken (meat poultry)
# salmonella_pc4 <- salmonella_pc4[salmonella_pc4$Serotype %in% c("1003", "1015", "1091", "1102", "1157", "1169", "1186", 
                                                                    # "1187", "1200", "1208", "1282", "1348", "1355", "1415", 
                                                                    # "1429", "4090", "7039", "1042", "1112", "1136", "1215",
                                                                    # "1267", "1311", "1335", "1460", "1587", "1630", "1655",
                                                                    # "4130", "4995", "5153", "5191", "5196", "7002", "7032",
                                                                    # "7039"), ]

# Pigs
salmonella_pc4 <- salmonella_pc4[salmonella_pc4$Serotype %in% c("1023", "1058", "1066", "1070", "1106", "1144", "1148",
                                                                "1235", "1237", "1242", "1252", "1278", "1279", "1290",
                                                                "1301", "1338", "1342", "1373", "1374", "1401", "1507",
                                                                "4000", "4111", "4506", "4507", "4510", "4561", "4998",
                                                                "4999", "5000", "5158", "7064", "7221", "7236", "7237",
                                                                "9986", "1097", "1103", "1139", "1206", "1258", "1436",
                                                                "1442", "1484", "1531", "4001", "4010", "4020", "4061",
                                                                "4100", "4280", "4281", "4292", "4296", "4301", "4345",
                                                                "4350", "4351", "4501", "4504", "4658", "4690", "5181",
                                                                "7017", "7073", "9987"), ]

# # Cattle
# salmonella_pc4 <- salmonella_pc4[salmonella_pc4$Serotype %in% c("4003", "1110", "1275", "4353", "4508", "4651", "5161", 
#                                                                 "4002", "4080", "4290", "4300", "4652", "7037"), ]

# ------------------------------------------------------------------- ANIMAL DATA -------------------------------------------------------------------------------------------------------------------

# Read point animal data (2012)
animal.xy.sf.2012 <- read_excel(path = ".../Data/__.xlsx") %>%
  # Convert to dataframe
  as.data.frame %>%
  # Select relevant columns
  select(c(x_coor, y_coor, Varkens_totaal, Pluimvee_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
           Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen)) %>%
  # Select records that have at least 1 observation (= remove records with all NA's)
  filter_at(
    .vars = vars(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                 Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen),
    .vars_predicate = any_vars(!is.na(.))) %>%
  # Convert to simple feature
  st_as_sf(coords = c("x_coor", "y_coor")) %>%
  # Set CRS to 28992 (RD_NEW). Ignore warning
  st_set_crs(28992)

# Read point animal data (2015)
animal.xy.sf.2015 <- read_excel(path = ".../Data/__.xlsx", sheet = 'Calculations_animal_data') %>%
  # Convert to dataframe
  as.data.frame %>%
  # Select relevant columns
  select(c(x_coor, y_coor, Varkens_totaal, Pluimvee_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
           Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen)) %>%
  # Select records that have at least 1 observation (= remove records with all NA's)
  filter_at(
    .vars = vars(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                 Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen),
    .vars_predicate = any_vars(!is.na(.))) %>%
  # Convert to simple feature
  st_as_sf(coords = c("x_coor", "y_coor")) %>%
  # Set CRS to 28992 (RD_NEW). Ignore warning
  st_set_crs(28992)

# Read point animal data (2018)
animal.xy.sf.2018 <- read_excel(path = ".../Data/___.xlsx", sheet = 'Spatial_analyses_data') %>%
  # Convert to dataframe
  as.data.frame %>%
  # Select relevant columns
  select(c(x_coor, y_coor, Varkens_totaal, Pluimvee_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
           Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen)) %>%
  # Select records that have at least 1 observation (= remove records with all NA's)
  filter_at(
    .vars = vars(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                 Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen),
    .vars_predicate = any_vars(!is.na(.))) %>%
  # Convert to simple feature
  st_as_sf(coords = c("x_coor", "y_coor")) %>%
  # Set CRS to 28992 (RD_NEW). Ignore warning
  st_set_crs(28992)

# Assign years to datasets
animal.xy.sf.2007 <- animal.xy.sf.2012
animal.xy.sf.2007$year <- 2007
#write.table(animal.xy.sf.2007, "Data/dierdata/per_year/animal.xy.sf.2007.txt")
animal.xy.sf.2008 <- animal.xy.sf.2012
animal.xy.sf.2008$year <- 2008
#write.table(animal.xy.sf.2008, "Data/dierdata/per_year/animal.xy.sf.2008.txt")
animal.xy.sf.2009 <- animal.xy.sf.2012
animal.xy.sf.2009$year <- 2009
#write.table(animal.xy.sf.2009, "Data/dierdata/per_year/animal.xy.sf.2009.txt")
animal.xy.sf.2010 <- animal.xy.sf.2012
animal.xy.sf.2010$year <- 2010
#write.table(animal.xy.sf.2010, "Data/dierdata/per_year/animal.xy.sf.2010.txt")
animal.xy.sf.2011 <- animal.xy.sf.2012
animal.xy.sf.2011$year <- 2011
#write.table(animal.xy.sf.2011, "Data/dierdata/per_year/animal.xy.sf.2011.txt")
animal.xy.sf.2012$year <- 2012
#write.table(animal.xy.sf.2012, "Data/dierdata/per_year/animal.xy.sf.2012.txt")
animal.xy.sf.2013 <- animal.xy.sf.2012
animal.xy.sf.2013$year <- 2013
#write.table(animal.xy.sf.2013, "Data/dierdata/per_year/animal.xy.sf.2013.txt")
animal.xy.sf.2014 <- animal.xy.sf.2015
animal.xy.sf.2014$year <- 2014
#write.table(animal.xy.sf.2014, "Data/dierdata/per_year/animal.xy.sf.2014.txt")
animal.xy.sf.2015$year <- 2015
#write.table(animal.xy.sf.2015, "Data/dierdata/per_year/animal.xy.sf.2015.txt")
animal.xy.sf.2016 <- animal.xy.sf.2015
animal.xy.sf.2016$year <- 2016
#write.table(animal.xy.sf.2016, "Data/dierdata/per_year/animal.xy.sf.2016.txt")
animal.xy.sf.2017 <- animal.xy.sf.2018
animal.xy.sf.2017$year <- 2017
#write.table(animal.xy.sf.2017, "Data/dierdata/per_year/animal.xy.sf.2017.txt")
animal.xy.sf.2018$year <- 2018
#write.table(animal.xy.sf.2018, "Data/dierdata/per_year/animal.xy.sf.2018.txt")
animal.xy.sf.2019 <- animal.xy.sf.2018
animal.xy.sf.2019$year <- 2019
#write.table(animal.xy.sf.2019, "Data/dierdata/per_year/animal.xy.sf.2019.txt")

# Merge dierdatasets
animal.xy.sf <- rbind(animal.xy.sf.2007,animal.xy.sf.2008,animal.xy.sf.2009,animal.xy.sf.2010,animal.xy.sf.2011,animal.xy.sf.2012,animal.xy.sf.2013,animal.xy.sf.2014,animal.xy.sf.2015,
                     animal.xy.sf.2016,animal.xy.sf.2017,animal.xy.sf.2018,animal.xy.sf.2019)

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Example: breaks = 0, 15, 30 -> intervals = [0, 15), [15, 30)
age.breaks <- c(0, 5, 10, 50, Inf)

# In pop.pc4.year.data, add age categories and summarize population numbers
# Use lapply because this is a list of dataframes
pop.pc4.year.data <- pop.pc4.year.data %>%
  lapply(FUN = function(data) {
    data %>%
      # First Separate sex_agecat into sex and agecat. E.g. mannen_0tot5 -> mannen, 0tot5
      separate(col = sex_agecat, into = c("sex", "agecat"), sep = "_") %>%
      # Then separate agecat into agelwr and ageupr. Convert to integer. E.g. 0tot5 -> 0, 5
      separate(col = agecat, into = c("agelwr", "ageupr"), sep = "tot", convert = TRUE) %>%
      # Add age categories by cutting mean(c(agelwr, ageupr - 1)). E.g. 0, 5 -> 2 -> [0, 5)
      # Intervals are open on the right
      mutate(agecat = cut((agelwr + (ageupr - 1))/2, breaks = age.breaks, right = FALSE, include.lowest = TRUE)) %>%
      # Relabel mannen -> male and vrouwen -> female
      mutate(sex = recode_factor(sex, mannen = "male", vrouwen = "female")) %>%
      # Group by year, pc4, agecat, sex
      # Summarize population numbers
      group_by(year, pc4, agecat, sex) %>%
      summarize(population = sum(population)) %>%
      # Convert to data.frame (was tibble)
      as.data.frame
  })

#   # First some modifications
#     # Relabel M -> male and V -> female
salmonella.pc4.year.data <- salmonella_pc4 %>%
  # First some modifications
  mutate(
    # Relabel Man -> male and Vrouw -> female
    sex = recode_factor(sex, M = "male", V = "female"),
    # Add year (based on onset.date)
    year = year(onset.date),
    # Add season (summer = May - Oct, winter = Nov - Apr)
    #season = as.factor(season),
    season = month(onset.date) %in% 5:10 %>%
      factor(levels = c(FALSE, TRUE), labels = c("winter", "summer")),
    # Add age categories
    agecat = as.numeric(age) %>% cut(breaks = age.breaks, right = FALSE, include.lowest = TRUE)) %>%
  # Filter years 2007:2019
  filter(year %in% 2007:2019) %>%
  # Group by year, pc4, agecat, sex, season
  # Summarize cases
  group_by(year, pc4, agecat, sex, season) %>%
  summarize(cases = n()) %>%
  # Convert to dataframe (was tibble)
  as.data.frame %>%
  # Split by year
  split(f = .$year)

# Join pop.pc4.year.data and salmonella.pc4.data by year
pc4.year.data <- vector(mode = "list", length = n.years)
for (i in seq_len(n.years)) {
  # First complete records to match pc4 levels in pc4.year.sf
  # year, agecat and sex are complete in pop.pc4.year.data
  # season is complete in salmonella.pc4.year.data
  # pc4 is complete in pc4.year.sf
  tmp <- expand.grid(
    year   = pop.pc4.year.data[[i]]$year %>% unique,
    pc4    = pc4.year.sf[[i]]$pc4,
    agecat = pop.pc4.year.data[[i]]$agecat %>% levels,
    sex    = pop.pc4.year.data[[i]]$sex %>% levels,
    season = salmonella.pc4.year.data[[i]]$season %>% levels)

  # Left join these complete combinations with pop.pc4.year.data and salmonella.pc4.year.data
  pop.pc4.year.data[[i]] <- left_join(tmp, pop.pc4.year.data[[i]]) %>%
    mutate(
      # Replace NA's by 0
      population = is.na(population) %>% ifelse(yes = 0, no = population),
      # Divide population by 4 because of season (4x 3 months)
      #population = population / 4)
      # Divide population by 2 because of season (2x 6 months)
      population = population / 2)
  salmonella.pc4.year.data[[i]] <- left_join(tmp, salmonella.pc4.year.data[[i]]) %>%
    mutate(
      # Replace NA's by 0
      cases = is.na(cases) %>% ifelse(yes = 0, no = cases))

  # Join pop.pc4.year.data with salmonella.pc4.data into pc4.year.data
  pc4.year.data[[i]] <- full_join(pop.pc4.year.data[[i]], salmonella.pc4.year.data[[i]])
}

# Make intersection between hexagons and pc6 (as sparse matrix)
# (same for each year, therefore outside the loop)
hex.pc6 <- st_intersects(hex.sf, pop.pc6.sf)
hex.pc6 <- sparseMatrix(
  i = hex.pc6 %>% seq_along %>% rep(times = hex.pc6 %>% sapply(FUN = length)),
  j = hex.pc6 %>% unlist,
  dims = c(nrow(hex.sf), nrow(pop.pc6.sf)),
  x = 1)

# Some pc6's have population or cases (after distribution) but these are not aggregated because there is no hex associated with them
# Which pc6's have no hex?
jx <- which(colSums(hex.pc6) == 0)
# Find nearest hex using fast nearest neighbour search
ix <- nn2(
  data  = hex.sf %>% st_centroid %>% st_coordinates,
  query = pop.pc6.sf[jx, ] %>% st_coordinates,
  k = 1) %>% .$nn.idx %>% as.vector
# Assign nearest hex to these pc6's
hex.pc6 <- hex.pc6 + sparseMatrix(i = ix, j = jx, dims = c(nrow(hex.sf), nrow(pop.pc6.sf)), x = 1)

# For each year
hex.pop.year.data <- vector(mode = "list", length = n.years)
for (i in seq_len(n.years)) {

  # Print progress
  print(years[i])

  # Make intersection between pc6 and pc4 (as sparse matrix)
  # (differs between years, therefore within the loop)
  pc6.pc4 <- st_intersects(pop.pc6.sf, pc4.year.sf[[i]])
  pc6.pc4 <- sparseMatrix(
    i = pc6.pc4 %>% seq_along %>% rep(times = pc6.pc4 %>% sapply(FUN = length)),
    j = pc6.pc4 %>% unlist,
    dims = c(nrow(pop.pc6.sf), nrow(pc4.year.sf[[i]])),
    x = 1)

  # Some pc4's have population or cases, but these are not distributed because there is no pc6 associated with them
  # # Which pc4's have no pc6?
  jx <- which(colSums(pc6.pc4) == 0)
  # Find nearest pc6 using fast nearest neighbour search
  ix <- nn2(
    data  = pop.pc6.sf %>% st_coordinates,
    query = pc4.year.sf[[i]][jx, ] %>% st_centroid %>% st_coordinates,
    k = 1) %>% .$nn.idx %>% as.vector
  # Assign nearest pc6 to these pc4's
  pc6.pc4 <- pc6.pc4 + sparseMatrix(i = ix, j = jx, dims = c(nrow(pop.pc6.sf), nrow(pc4.year.sf[[i]])), x = 1)

  # Add pc6 population numbers to columns of pc6.pc4
  pc6.pc4 <- pc6.pc4 * pop.pc6.sf$population

  # Calculate pc6 population fractions for each pc4 area by dividing each column by its column total
  # Use a trick for sparse matrices:
  # see https://stackoverflow.com/questions/39284774/column-rescaling-for-a-very-large-sparse-matrix-in-r
  pc6.pc4@x <- pc6.pc4@x / rep.int(colSums(pc6.pc4), diff(pc6.pc4@p))

  # R is the redistribution matrix pc4 -> pc6 -> hex
  # All we have to do is to multiply pc4 population or cases with this matrix to get the numbers in the hexagons
  R <- hex.pc6 %*% pc6.pc4

  # Because we have multiple strata, split pc4.year.data by agecat, sex, season
  stratum.pc4.year.data <- pc4.year.data[[i]] %>% split(f = list(.$agecat, .$sex, .$season))

  # For each stratum, redistribute pc4 population and cases to the hexagons
  stratum.hex.pop.year.data <- stratum.pc4.year.data %>% lapply(FUN = function(data) {
    data.frame(
      # For each hex, these are constant:
      year = data$year %>% unique,
      hex = hex.sf %>% nrow %>% seq_len,
      agecat = data$agecat %>% unique,
      sex = data$sex %>% unique,
      season = data$season %>% unique,
      # But these must be redistributed:
      population = R %*% data$population %>% as.vector,
      cases = R %*% data$cases %>% as.vector)
  })

  # rbind stratum.pop.hex.year.data back into one dataframe
  hex.pop.year.data[[i]] <- stratum.hex.pop.year.data %>% do.call(what = "rbind")
}

# Finally rbind hex.pop.year.data into one dataframe
hex.pop.data <- hex.pop.year.data %>% do.call(what = "rbind")

# ----------------------------

# Population weighted number of animals within 1 km for each hex ----
#
hex.sf %>% st_area %>% as.numeric

# Create 1 km buffer around each pc6 point (also done for 250 m to investigate dose-response relationship)
pop.pc6.buffer.sf <- pop.pc6.sf %>%
  st_buffer(dist = 1000, nQuadSegs = 5)

# Spatial left join pop.pc6.buffer.sf with animal.year.xy.sf for each year
  # 2007
  animal.pc6.buffer.sf.2007 <- st_join(pop.pc6.buffer.sf, st_as_sf(animal.xy.sf.2007), left = TRUE) %>%
    #    Replace NAs with 0
    mutate_at(
      .vars = vars(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                   Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen),
      .funs = funs(replace(., list = is.na(.), values = 0)))
  animal.pc6.buffer.sf.2007$year <- 2007
  # 2008
  animal.pc6.buffer.sf.2008 <- st_join(pop.pc6.buffer.sf, st_as_sf(animal.xy.sf.2008), left = TRUE) %>%
    #    Replace NAs with 0
    mutate_at(
      .vars = vars(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                   Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen),
      .funs = funs(replace(., list = is.na(.), values = 0)))
  animal.pc6.buffer.sf.2008$year <- 2008
  # 2009
  animal.pc6.buffer.sf.2009 <- st_join(pop.pc6.buffer.sf, st_as_sf(animal.xy.sf.2009), left = TRUE) %>%
    #    Replace NAs with 0
    mutate_at(
      .vars = vars(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                   Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen),
      .funs = funs(replace(., list = is.na(.), values = 0)))
  animal.pc6.buffer.sf.2009$year <- 2009
  # 2010
  animal.pc6.buffer.sf.2010 <- st_join(pop.pc6.buffer.sf, st_as_sf(animal.xy.sf.2010), left = TRUE) %>%
    #    Replace NAs with 0
    mutate_at(
      .vars = vars(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                   Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen),
      .funs = funs(replace(., list = is.na(.), values = 0)))
  animal.pc6.buffer.sf.2010$year <- 2010
  # 2011
  animal.pc6.buffer.sf.2011 <- st_join(pop.pc6.buffer.sf, st_as_sf(animal.xy.sf.2011), left = TRUE) %>%
    #    Replace NAs with 0
    mutate_at(
      .vars = vars(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                   Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen),
      .funs = funs(replace(., list = is.na(.), values = 0)))
  animal.pc6.buffer.sf.2011$year <- 2011
  # 2012
  animal.pc6.buffer.sf.2012 <- st_join(pop.pc6.buffer.sf, st_as_sf(animal.xy.sf.2012), left = TRUE) %>%
    #    Replace NAs with 0
    mutate_at(
      .vars = vars(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                   Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen),
      .funs = funs(replace(., list = is.na(.), values = 0)))
  animal.pc6.buffer.sf.2012$year <- 2012
  # 2013
  animal.pc6.buffer.sf.2013 <- st_join(pop.pc6.buffer.sf, st_as_sf(animal.xy.sf.2013), left = TRUE) %>%
    #    Replace NAs with 0
    mutate_at(
      .vars = vars(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                   Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen),
      .funs = funs(replace(., list = is.na(.), values = 0)))
  animal.pc6.buffer.sf.2013$year <- 2013
  # 2014
  animal.pc6.buffer.sf.2014 <- st_join(pop.pc6.buffer.sf, st_as_sf(animal.xy.sf.2014), left = TRUE) %>%
    #    Replace NAs with 0
    mutate_at(
      .vars = vars(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                   Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen),
      .funs = funs(replace(., list = is.na(.), values = 0)))
  animal.pc6.buffer.sf.2014$year <- 2014
  # 2015
  animal.pc6.buffer.sf.2015 <- st_join(pop.pc6.buffer.sf, st_as_sf(animal.xy.sf.2015), left = TRUE) %>%
    #    Replace NAs with 0
    mutate_at(
      .vars = vars(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                   Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen),
      .funs = funs(replace(., list = is.na(.), values = 0)))
  animal.pc6.buffer.sf.2015$year <- 2015
  # 2016
  animal.pc6.buffer.sf.2016 <- st_join(pop.pc6.buffer.sf, st_as_sf(animal.xy.sf.2016), left = TRUE) %>%
    #    Replace NAs with 0
    mutate_at(
      .vars = vars(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                   Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen),
      .funs = funs(replace(., list = is.na(.), values = 0)))
  animal.pc6.buffer.sf.2016$year <- 2016
  # 2017
  animal.pc6.buffer.sf.2017 <- st_join(pop.pc6.buffer.sf, st_as_sf(animal.xy.sf.2017), left = TRUE) %>%
    #    Replace NAs with 0
    mutate_at(
      .vars = vars(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                   Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen),
      .funs = funs(replace(., list = is.na(.), values = 0)))
  animal.pc6.buffer.sf.2017$year <- 2017
  # 2018
  animal.pc6.buffer.sf.2018 <- st_join(pop.pc6.buffer.sf, st_as_sf(animal.xy.sf.2018), left = TRUE) %>%
    #    Replace NAs with 0
    mutate_at(
      .vars = vars(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                   Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen),
      .funs = funs(replace(., list = is.na(.), values = 0)))
  animal.pc6.buffer.sf.2018$year <- 2018
  # 2019
  animal.pc6.buffer.sf.2019 <- st_join(pop.pc6.buffer.sf, st_as_sf(animal.xy.sf.2019), left = TRUE) %>%
    #    Replace NAs with 0
    mutate_at(
      .vars = vars(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                   Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen),
      .funs = funs(replace(., list = is.na(.), values = 0)))
  animal.pc6.buffer.sf.2019$year <- 2019


# Sum animals within each pc6 1 km buffer
# (use aggregate, is much faster than dplyr's group_by/summarize)
# 2007
pc6.animal.data.2007 <- aggregate(
    formula = cbind(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                    Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen) ~ pc6,
    FUN = sum,
    data = animal.pc6.buffer.sf.2007)
pc6.animal.data.2007$year <- 2007
# 2008
pc6.animal.data.2008 <- aggregate(
  formula = cbind(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                  Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen) ~ pc6,
  FUN = sum,
  data = animal.pc6.buffer.sf.2008)
pc6.animal.data.2008$year <- 2008
# 2009
pc6.animal.data.2009 <- aggregate(
  formula = cbind(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                  Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen) ~ pc6,
  FUN = sum,
  data = animal.pc6.buffer.sf.2009)
pc6.animal.data.2009$year <- 2009
# 2010
pc6.animal.data.2010 <- aggregate(
  formula = cbind(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                  Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen) ~ pc6,
  FUN = sum,
  data = animal.pc6.buffer.sf.2010)
pc6.animal.data.2010$year <- 2010
# 2011
pc6.animal.data.2011 <- aggregate(
  formula = cbind(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                  Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen) ~ pc6,
  FUN = sum,
  data = animal.pc6.buffer.sf.2011)
pc6.animal.data.2011$year <- 2011
# 2012
pc6.animal.data.2012 <- aggregate(
  formula = cbind(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                  Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen) ~ pc6,
  FUN = sum,
  data = animal.pc6.buffer.sf.2012)
pc6.animal.data.2012$year <- 2012
# 2013
pc6.animal.data.2013 <- aggregate(
  formula = cbind(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                  Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen) ~ pc6,
  FUN = sum,
  data = animal.pc6.buffer.sf.2013)
pc6.animal.data.2013$year <- 2013
# 2014
pc6.animal.data.2014 <- aggregate(
  formula = cbind(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                  Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen) ~ pc6,
  FUN = sum,
  data = animal.pc6.buffer.sf.2014)
pc6.animal.data.2014$year <- 2014
# 2015
pc6.animal.data.2015 <- aggregate(
  formula = cbind(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                  Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen) ~ pc6,
  FUN = sum,
  data = animal.pc6.buffer.sf.2015)
pc6.animal.data.2015$year <- 2015
# 2016
pc6.animal.data.2016 <- aggregate(
  formula = cbind(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                  Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen) ~ pc6,
  FUN = sum,
  data = animal.pc6.buffer.sf.2016)
pc6.animal.data.2016$year <- 2016
# 2017
pc6.animal.data.2017 <- aggregate(
  formula = cbind(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                  Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen) ~ pc6,
  FUN = sum,
  data = animal.pc6.buffer.sf.2017)
pc6.animal.data.2017$year <- 2017
# 2018
pc6.animal.data.2018 <- aggregate(
  formula = cbind(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                  Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen) ~ pc6,
  FUN = sum,
  data = animal.pc6.buffer.sf.2018)
pc6.animal.data.2018$year <- 2018
# 2019
pc6.animal.data.2019 <- aggregate(
  formula = cbind(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                  Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen) ~ pc6,
  FUN = sum,
  data = animal.pc6.buffer.sf.2019)
pc6.animal.data.2019$year <- 2019

# Construct population weight matrix
# Transpose hex.pc6 for easier calculations
pc6.hex <- t(hex.pc6)
# Add pc6 population numbers to columns of pc6.hex
pc6.hex <- pc6.hex * pop.pc6.sf$population
# Calculate pc6 population fractions for each hex by dividing each column by its hex total (use trick)
pc6.hex@x <- pc6.hex@x / rep.int(colSums(pc6.hex), diff(pc6.hex@p))

# Calculate weighted number of animals within 1 km for each hex for each year
# 2007
animal.hex.sf.2007 <- cbind(
  hex.sf,
  (t(pc6.hex) %*% (pc6.animal.data.2007 %>% select(-c(pc6,year)) %>% as.matrix)) %>% as.matrix)
animal.hex.sf.2007$year <- 2007
# 2008
animal.hex.sf.2008 <- cbind(
  hex.sf,
  (t(pc6.hex) %*% (pc6.animal.data.2008 %>% select(-c(pc6,year)) %>% as.matrix)) %>% as.matrix)
animal.hex.sf.2008$year <- 2008
# 2009
animal.hex.sf.2009 <- cbind(
  hex.sf,
  (t(pc6.hex) %*% (pc6.animal.data.2009 %>% select(-c(pc6,year)) %>% as.matrix)) %>% as.matrix)
animal.hex.sf.2009$year <- 2009
# 2010
animal.hex.sf.2010 <- cbind(
  hex.sf,
  (t(pc6.hex) %*% (pc6.animal.data.2010 %>% select(-c(pc6,year)) %>% as.matrix)) %>% as.matrix)
animal.hex.sf.2010$year <- 2010
# 2011
animal.hex.sf.2011 <- cbind(
  hex.sf,
  (t(pc6.hex) %*% (pc6.animal.data.2011 %>% select(-c(pc6,year)) %>% as.matrix)) %>% as.matrix)
animal.hex.sf.2011$year <- 2011
# 2012
animal.hex.sf.2012 <- cbind(
  hex.sf,
  (t(pc6.hex) %*% (pc6.animal.data.2012 %>% select(-c(pc6,year)) %>% as.matrix)) %>% as.matrix)
animal.hex.sf.2012$year <- 2012
# 2013
animal.hex.sf.2013 <- cbind(
  hex.sf,
  (t(pc6.hex) %*% (pc6.animal.data.2013 %>% select(-c(pc6,year)) %>% as.matrix)) %>% as.matrix)
animal.hex.sf.2013$year <- 2013
# 2014
animal.hex.sf.2014 <- cbind(
  hex.sf,
  (t(pc6.hex) %*% (pc6.animal.data.2014 %>% select(-c(pc6,year)) %>% as.matrix)) %>% as.matrix)
animal.hex.sf.2014$year <- 2014
# 2015
animal.hex.sf.2015 <- cbind(
  hex.sf,
  (t(pc6.hex) %*% (pc6.animal.data.2015 %>% select(-c(pc6,year)) %>% as.matrix)) %>% as.matrix)
animal.hex.sf.2015$year <- 2015
# 2016
animal.hex.sf.2016 <- cbind(
  hex.sf,
  (t(pc6.hex) %*% (pc6.animal.data.2016 %>% select(-c(pc6,year)) %>% as.matrix)) %>% as.matrix)
animal.hex.sf.2016$year <- 2016
# 2017
animal.hex.sf.2017 <- cbind(
  hex.sf,
  (t(pc6.hex) %*% (pc6.animal.data.2017 %>% select(-c(pc6,year)) %>% as.matrix)) %>% as.matrix)
animal.hex.sf.2017$year <- 2017
# 2018
animal.hex.sf.2018 <- cbind(
  hex.sf,
  (t(pc6.hex) %*% (pc6.animal.data.2018 %>% select(-c(pc6,year)) %>% as.matrix)) %>% as.matrix)
animal.hex.sf.2018$year <- 2018
# 2019
animal.hex.sf.2019 <- cbind(
  hex.sf,
  (t(pc6.hex) %*% (pc6.animal.data.2019 %>% select(-c(pc6,year)) %>% as.matrix)) %>% as.matrix)
animal.hex.sf.2019$year <- 2019

# merge pc6.animal.data.year
animal.hex.year.sf <- rbind(animal.hex.sf.2007,animal.hex.sf.2008,animal.hex.sf.2009,animal.hex.sf.2010,animal.hex.sf.2011,animal.hex.sf.2012,animal.hex.sf.2013,animal.hex.sf.2014,animal.hex.sf.2015,
                            animal.hex.sf.2016,animal.hex.sf.2017,animal.hex.sf.2018,animal.hex.sf.2019)

#--------------------------------------------------------------------------------------------------

#
# Exploratory analysis ----
#

# Start with plotting the data
#
# Aggegrate population and cases over all years
# Calculate raw incidence
hex.agg.sf <- hex.pop.data %>%
  # Summarize population and cases
   group_by(hex) %>%
   summarize(population = sum(population), cases = sum(cases)) %>%
  # Calculate raw incidence per 100,000
  # Replace NA's by 0
  mutate(incidence = (cases/population*1e5) %>% replace(., list = is.na(.), values = 0)) %>%
  # Join with hex.sf
  full_join(hex.sf) %>%
  # Convert to sf object
  st_as_sf

# ------------------------ Creating maps ------------------------
# Set working directory
# setwd('.../Results/Maps/...')
#
# Population
# plot(hex.agg.sf[, "population"],
#      breaks = "fisher", nbreaks = 50,
#      pal = colorRampPalette(brewer.pal(name = "Greys", n = 9)),
#      key.pos = 1,
#      lwd = 0.1,
#      main = "A)")
#
# Raw incidence
# plot(hex.agg.sf[, "incidence"],
#      breaks = "fisher", nbreaks = 50,
#      pal = colorRampPalette(brewer.pal(name = "Greys", n = 9)),
#      key.pos = 1,
#      lwd = 0.1,
#      main = "S. Enteritidis incidence per 100,000 2007 - 2019")

# Raw incidence new
# Prepare data
# hex.agg.sf$incidence_cat <- ifelse(hex.agg.sf$incidence <= 0.5, "0.0-0.5", 0)
# hex.agg.sf$incidence_cat <- ifelse(hex.agg.sf$incidence > 0.5 & hex.agg.sf$incidence <= 1.5, "0.5-1.5", hex.agg.sf$incidence_cat)
# hex.agg.sf$incidence_cat <- ifelse(hex.agg.sf$incidence > 1.5 & hex.agg.sf$incidence <= 2.5, "1.5-2.5", hex.agg.sf$incidence_cat)
# hex.agg.sf$incidence_cat <- ifelse(hex.agg.sf$incidence > 2.5 & hex.agg.sf$incidence <= 4.0, "2.5-4.0", hex.agg.sf$incidence_cat)
# hex.agg.sf$incidence_cat <- ifelse(hex.agg.sf$incidence > 4.0 & hex.agg.sf$incidence <= 6.0, "4.0-6.0", hex.agg.sf$incidence_cat)
# hex.agg.sf$incidence_cat <- ifelse(hex.agg.sf$incidence > 6.0 & hex.agg.sf$incidence <= 10.0, "6.0-10.0", hex.agg.sf$incidence_cat)
# hex.agg.sf$incidence_cat <- ifelse(hex.agg.sf$incidence > 10.0, ">10.0", hex.agg.sf$incidence_cat)
# hex.agg.sf$incidence <- as.factor(hex.agg.sf$incidence_cat)
# 
# Save dataset
# saveRDS(hex.agg.sf, file = ".../Results/Maps/Figure 1/Rds files/incidence_pigs.Rds")

#-----------------------------------------------------------------------------------------------------

# Spatial analysis ----
#
#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

#install INLA op server (if above does not work):
#INLA:::inla.dynload.workaround()

# Load packages
library(INLA)
#inla.binary.install()

# Source sf2nb function (spatial neighbours function)
source(file = ".../scripts/sf2nb.R")

# Get spatial neighbours of hex.sf
hex.nb <- sf2nb(hex.sf)

# Plot spatial neighbours
graphics.off()
plot(hex.nb, coords = hex.sf %>% st_centroid %>% st_coordinates, pch = ".", lwd = 0.1)

# Some modifications of hex.pop.data
# Aggregate hex.pop.data over years
hex.pop.agg.data <- hex.pop.data %>%
  # Some modifications
  mutate(
    cases = ifelse(population == 0, yes = NA, no = cases) %>% round,
    population = ifelse(population == 0, yes = 1, no = population),
    hex.car = hex,
    hex.iid = hex)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# With animals

# Join hex.pop.agg.data with animal.cat.hex.sf
tmp.data <- full_join(hex.pop.agg.data, animal.hex.year.sf)


#--------------select fully covered hexagons based on covered pc4s based on geographical labbias analysis--------------------------------------------------------------


# Read covered PC4s excel file
pc4_covered <- read_excel(".../Data/__.xlsx")
pc4_covered$covered <- 1
pc4_covered$PC4 <- pc4_covered$pc4

#keep rows containing covered pc4s
#to be able to look at how many cases are left then:
#salmonella_pc4 <- salmonella_pc4[(salmonella_pc4$pc4 %in% pc4_covered$pc4),]

# Read pc4 polygons of 2012
pc4_2012 <- read_sf('.../Data/pc4_2012.shp')

# Join pc4_covered with pc4_2012
pc4_2012.sf <- full_join(x = pc4_2012, y = pc4_covered, by = "PC4")

# -----------------------

# select covered pc4s
pc4_2012_covered.sf <- subset(pc4_2012.sf, pc4_2012.sf$covered == 1)

# Drop columns
pc4_2012_covered.sf <- subset(pc4_2012_covered.sf, select = c(PC4))

# Intersect covered PC4s with hexagon using st_intersection 
st_geometry(pc4_2012_covered.sf) <- st_sfc(lapply(st_geometry(pc4_2012_covered.sf), function(x) st_buffer(x, 0)), crs = st_crs(pc4_2012_covered.sf))
hex.cov.sf <- st_intersection(hex.sf, pc4_2012_covered.sf)

# --------------------------------------------CONDITION: more than 80% of the area of the hexagon should be covered by the area(s) of covered pc4's ---------------------------

# add in areas in m2
hex.cov.sf.area <- hex.cov.sf %>%
  mutate(area = st_area(.) %>% as.numeric())

# for each hexagon, get total area covered with covered PC4s
hex.cov.sf.area2 <- hex.cov.sf.area %>% 
                      as_tibble() %>% 
                      group_by(hex) %>% 
                      summarize(area = sum(area))

# add column with hexagon size (m2)
## TAKE CARE: this should be adapted if analysis is reran for other hexagonal sizes!
hex.cov.sf.area2$hex.size <- 90000000

# calculate total area of hex covered
hex.cov.sf.area2$covered <- ((hex.cov.sf.area2$area/hex.cov.sf.area2$hex.size)*100)

# select % covered hexagons (adapt according to analysis!)
hex.cov.sf.area2 <- subset(hex.cov.sf.area2, (covered > 80)) # >80% coverage

# select the proper hexagons to be used within the analysis from tmp.data using hex.cov.sf.area2
tmp.data <- tmp.data[(tmp.data$hex %in% hex.cov.sf.area2$hex),]

# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#including relevel function
tmp.data$agecat<-relevel(tmp.data$agecat, ref = "[10,50)")

#count nr of cases per year
aggregate(tmp.data$cases, by=list(Category=tmp.data$year), FUN= sum, na.rm = TRUE)

# Univariate analyses
# Age
mod1 <- inla(
  formula = cases ~ agecat +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE),
  verbose = TRUE)

summary(mod1)
result.mod1 <- mod1$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 4)
result.mod1$pvalue <- 2*(1 - pnorm(abs(mod1$summary.fixed[,"mean"]/mod1$summary.fixed[,"sd"])))
result.mod1

# # Plot random effects
# plot(cbind(hex.sf, car = mod1$summary.random$hex.car[, "mean"])[, "car"],
#      breaks = "fisher", nbreaks = 50,
#      pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
#      key.pos = 1,
#      lwd = 0.1)
# plot(cbind(hex.sf, iid = mod1$summary.random$hex.iid[, "mean"])[, "iid"],
#      breaks = "fisher", nbreaks = 50,
#      pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
#      key.pos = 1,
#      lwd = 0.1)

# Sex
mod2 <- inla(
  formula = cases ~ sex +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE))

summary(mod2)
result.mod2 <- mod2$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 4)
result.mod2$pvalue <- 2*(1 - pnorm(abs(mod2$summary.fixed[,"mean"]/mod2$summary.fixed[,"sd"])))
result.mod2

# # Plot random effects
# plot(cbind(hex.sf, car = mod2$summary.random$hex.car[, "mean"])[, "car"],
#      breaks = "fisher", nbreaks = 50,
#      pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
#      key.pos = 1,
#      lwd = 0.1)
# plot(cbind(hex.sf, iid = mod2$summary.random$hex.iid[, "mean"])[, "iid"],
#      breaks = "fisher", nbreaks = 50,
#      pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
#      key.pos = 1,
#      lwd = 0.1)

# Season
tmp.data$season <-relevel(tmp.data$season, ref = "winter")
mod3 <- inla(
  formula = cases ~ season +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE),
  verbose = TRUE)

summary(mod3)
result.mod3 <- mod3$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 4)
result.mod3$pvalue <- 2*(1 - pnorm(abs(mod3$summary.fixed[,"mean"]/mod3$summary.fixed[,"sd"])))
result.mod3

# # Plot random effects
# plot(cbind(hex.sf, car = mod3$summary.random$hex.car[, "mean"])[, "car"],
#      breaks = "fisher", nbreaks = 50,
#      pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
#      key.pos = 1,
#      lwd = 0.1)
# plot(cbind(hex.sf, iid = mod3$summary.random$hex.iid[, "mean"])[, "iid"],
#      breaks = "fisher", nbreaks = 50,
#      pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
#      key.pos = 1,
#      lwd = 0.1)

# Poultry, Laying hens
mod4.1 <- inla(
  formula = cases ~ log2(Kip_Leghen + 1) +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE))

summary(mod4.1)
result.mod4.1 <- mod4.1$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 4)
result.mod4.1$pvalue <- 2*(1 - pnorm(abs(mod4.1$summary.fixed[,"mean"]/mod4.1$summary.fixed[,"sd"])))
result.mod4.1

# # Plot random effects
# plot(cbind(hex.sf, car = mod4.1$summary.random$hex.car[, "mean"])[, "car"],
#      breaks = "fisher", nbreaks = 50,
#      pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
#      key.pos = 1,
#      lwd = 0.1)
# plot(cbind(hex.sf, iid = mod4.1$summary.random$hex.iid[, "mean"])[, "iid"],
#      breaks = "fisher", nbreaks = 50,
#      pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
#      key.pos = 1,
#      lwd = 0.1)

#Poultry, Broiler chickens
mod4.2 <- inla(
  formula = cases ~ log2(Kip_Kuiken + 1) +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE))

summary(mod4.2)
result.mod4.2 <- mod4.2$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 4)
result.mod4.2$pvalue <- 2*(1 - pnorm(abs(mod4.2$summary.fixed[,"mean"]/mod4.2$summary.fixed[,"sd"])))
result.mod4.2

# # Plot random effects
# plot(cbind(hex.sf, car = mod4.2$summary.random$hex.car[, "mean"])[, "car"],
#      breaks = "fisher", nbreaks = 50,
#      pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
#      key.pos = 1,
#      lwd = 0.1)
# plot(cbind(hex.sf, iid = mod4.2$summary.random$hex.iid[, "mean"])[, "iid"],
#      breaks = "fisher", nbreaks = 50,
#      pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
#      key.pos = 1,
#      lwd = 0.1)

# Pigs
mod5 <- inla(
  formula = cases ~ log2(Varkens_totaal + 1) +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE),
  verbose = TRUE)

summary(mod5)
result.mod5 <- mod5$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 4)
result.mod5$pvalue <- 2*(1 - pnorm(abs(mod5$summary.fixed[,"mean"]/mod5$summary.fixed[,"sd"])))
result.mod5

# # Plot random effects
# plot(cbind(hex.sf, car = mod5$summary.random$hex.car[, "mean"])[, "car"],
#      breaks = "fisher", nbreaks = 50,
#      pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
#      key.pos = 1,
#      lwd = 0.1)
# plot(cbind(hex.sf, iid = mod5$summary.random$hex.iid[, "mean"])[, "iid"],
#      breaks = "fisher", nbreaks = 50,
#      pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
#      key.pos = 1,
#      lwd = 0.1)

# Dairy cows
mod6.1 <- inla(
  formula = cases ~ log2(Rund_Melk_totaal + 1) +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE))

summary(mod6.1)
result.mod6.1 <- mod6.1$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 4)
result.mod6.1$pvalue <- 2*(1 - pnorm(abs(mod6.1$summary.fixed[,"mean"]/mod6.1$summary.fixed[,"sd"])))
result.mod6.1

# # Plot random effects
# plot(cbind(hex.sf, car = mod6$summary.random$hex.car[, "mean"])[, "car"],
#      breaks = "fisher", nbreaks = 50,
#      pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
#      key.pos = 1,
#      lwd = 0.1)
# plot(cbind(hex.sf, iid = mod6$summary.random$hex.iid[, "mean"])[, "iid"],
#      breaks = "fisher", nbreaks = 50,
#      pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
#      key.pos = 1,
#      lwd = 0.1)

# Veal calves
mod6.2 <- inla(
  formula = cases ~ log2(Rund_Vlees_totaal + 1) +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE),
  verbose = TRUE)

summary(mod6.2)
result.mod6.2 <- mod6.2$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 4)
result.mod6.2$pvalue <- 2*(1 - pnorm(abs(mod6.2$summary.fixed[,"mean"]/mod6.2$summary.fixed[,"sd"])))
result.mod6.2

# # Plot random effects
# plot(cbind(hex.sf, car = mod6$summary.random$hex.car[, "mean"])[, "car"],
#      breaks = "fisher", nbreaks = 50,
#      pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
#      key.pos = 1,
#      lwd = 0.1)
# plot(cbind(hex.sf, iid = mod6$summary.random$hex.iid[, "mean"])[, "iid"],
#      breaks = "fisher", nbreaks = 50,
#      pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
#      key.pos = 1,
#      lwd = 0.1)

# Small ruminants
mod9 <- inla(
  formula = cases ~ log2(Kleine_Herkauwers_totaal + 1) +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data,
  control.inla = list(
    #strategy = "gaussian",
    int.strategy = "eb"),
  control.predictor = list(compute = TRUE),
  verbose = TRUE)

summary(mod9)
result.mod9 <- mod9$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 4)
result.mod9$pvalue <- 2*(1 - pnorm(abs(mod9$summary.fixed[,"mean"]/mod9$summary.fixed[,"sd"])))
result.mod9

# # Plot random effects
# plot(cbind(hex.sf, car = mod9$summary.random$hex.car[, "mean"])[, "car"],
#      breaks = "fisher", nbreaks = 50,
#      pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
#      key.pos = 1,
#      lwd = 0.1)
# plot(cbind(hex.sf, iid = mod9$summary.random$hex.iid[, "mean"])[, "iid"],
#      breaks = "fisher", nbreaks = 50,
#      pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
#      key.pos = 1,
#      lwd = 0.1)
#----------------------------------


# Multivariate analyses

#select summer
tmp.data.summer <- subset(tmp.data, tmp.data$season == 'summer')

# analysis summer
mod10 <- inla(
  formula = cases ~ sex + agecat + log2(Kip_Leghen + 1) + log2(Kip_Kuiken + 1) + log2(Varkens_totaal + 1) +
    log2(Rund_Melk_totaal + 1) + log2(Rund_Vlees_totaal + 1) + log2(Kleine_Herkauwers_totaal + 1) +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data.summer,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE))

summary(mod10)
result.mod10 <- mod10$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 2)
result.mod10$pvalue <- 2*(1 - pnorm(abs(mod10$summary.fixed[,"mean"]/mod10$summary.fixed[,"sd"])))
result.mod10

# # Plot random effects
# plot(cbind(hex.sf, car = mod10$summary.random$hex.car[, "mean"])[, "car"],
#      breaks = "fisher", nbreaks = 50,
#      pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
#      key.pos = 1,
#      lwd = 0.1)
# plot(cbind(hex.sf, iid = mod10$summary.random$hex.iid[, "mean"])[, "iid"],
#      breaks = "fisher", nbreaks = 50,
#      pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
#      key.pos = 1,
#      lwd = 0.1)

#select winter
tmp.data.winter <- subset(tmp.data, tmp.data$season == 'winter')
#tmp.data.cat.winter <- subset(tmp.data.cat, tmp.data$season == 'winter')

# analysis winter
mod11 <- inla(
  formula = cases ~ sex + agecat + log2(Kip_Leghen + 1) + log2(Kip_Kuiken + 1) + log2(Varkens_totaal + 1) +
    log2(Rund_Melk_totaal + 1) + log2(Rund_Vlees_totaal + 1) + log2(Kleine_Herkauwers_totaal + 1) +
    f(hex.car, model = "besag",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1))),
      graph = hex.nb) +
    f(hex.iid, model = "iid",
      hyper = list(prec = list(prior = "loggamma", param = c(1, 0.1)))),
  E = population,
  family = "poisson",
  data = tmp.data.winter,
  control.inla = list(int.strategy = "eb"),
  control.predictor = list(compute = TRUE))

summary(mod11)
result.mod11 <- mod11$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 2)
result.mod11$pvalue <- 2*(1 - pnorm(abs(mod11$summary.fixed[,"mean"]/mod11$summary.fixed[,"sd"])))
result.mod11

# # Plot random effects
# plot(cbind(hex.sf, car = mod11$summary.random$hex.car[, "mean"])[, "car"],
#      breaks = "fisher", nbreaks = 50,
#      pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
#      key.pos = 1,
#      lwd = 0.1)
# plot(cbind(hex.sf, iid = mod11$summary.random$hex.iid[, "mean"])[, "iid"],
#      breaks = "fisher", nbreaks = 50,
#      pal = colorRampPalette(rev(brewer.pal(name = "Spectral", n = 9))),
#      key.pos = 1,
#      lwd = 0.1)