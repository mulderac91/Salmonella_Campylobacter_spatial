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


# --------------------------------------------------2007--------------------------------------------------------------------------------------
# Set years
years <- 2007
n.years <- length(years)

# Read pc4 population numbers for each year
pop.pc4.year.data <- vector(mode = "list", length = n.years)
for (i in seq_len(n.years)) {
  pop.pc4.year.data[[i]] <- read.delim(file = paste0(".../data/.../BevolkingPerPostcode_1januari", years[i], ".txt")) %>%
    # Select relevant columns
    select(pc4, mannen_0tot5:mannen_95tot100, vrouwen_0tot5:vrouwen_95tot100) %>%
    # Reshape population numbers into long format
    gather(key = "sex_agecat", value = "population", -pc4) %>%
    # Add year
    mutate(year = years[i])
}

### NOTE PC4 shapefile in map 2006 is the same as the one of 2007, because there is no pc4 available for 2006

# Read pc4 polygons for each year
pc4.year.sf <- vector(mode = "list", length = n.years)
for (i in seq_len(n.years)) {
  pc4.year.sf[[i]] <- st_read(
    dsn = paste0(".../data/.../", years[i]),
    layer = paste0("pc4_", years[i]), 
    type = 6, 
    quiet = TRUE) %>%
    # Add year
    mutate(year = years[i]) %>%
    # Set CRS to 29882 (ignore warning)
    st_set_crs(28992)
}

#make pc4.sf 
pc4.sf <- st_read(
  dsn = paste0(".../data/.../2007"),
  layer = paste0("pc4_2007"), 
  type = 6, 
  quiet = TRUE) %>%
  # Add year
  mutate(year = 2007) %>%
  # Set CRS to 29882 (ignore warning)
  st_set_crs(28992)

# Rename pc4 workaround
pc4.year.sf <- lapply(X = pc4.year.sf, FUN = function(data) {
  # Rename the different original pc4 names (pc4, PC4, postcode, etc.) to pc4
  # Be sure pc4 is in the first column
  names(data)[1] <- "pc4"
  #rename(pc4 = PC4) %>%
  data %>%
    arrange(pc4)
})

# Check dubbele postcodes
sapply(X = pc4.year.sf, FUN = function(data) data$pc4 %>% duplicated %>% sum)
# Are there any duplicated records? Which?
pc4.year.sf[[i]]$pc4 %>% duplicated %>% any
pc4.year.sf$pc4 %>% duplicated %>% which

# Read pc6 points and population (2007)
pop.pc6.sf <- read.delim(file = ".../data/.../pc6_2007.txt", sep = ";") %>%
  # Rename ppcpers -> population
  rename(population = ppcpers) %>%
  # Drop pc6 with 0 inhabitants
  filter(population > 0) %>%
  # Convert to sf object
  st_as_sf(coords = c("xcoord", "ycoord")) %>%
  # Set CRS to 29882 (ignore warning)
  st_set_crs(28992)

# Read point animal data (2012)
animal.xy.sf <- read_excel(path = ".../data/__.xlsx") %>%
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


# Make intersection between pc4s and pc6 (as sparse matrix)
# (same for each year, therefore outside the loop)
pc4.pc6 <- st_intersects(pc4.sf, pop.pc6.sf)
pc4.pc6 <- sparseMatrix(
  i = pc4.pc6 %>% seq_along %>% rep(times = pc4.pc6 %>% sapply(FUN = length)),
  j = pc4.pc6 %>% unlist,
  dims = c(nrow(pc4.sf), nrow(pop.pc6.sf)),
  x = 1)

# Some pc6's have population or cases (after distribution) but these are not aggregated because there is no pc4 associated with them
# Which pc6's have no pc4?
jx <- which(colSums(pc4.pc6) == 0)
# Find nearest pc4 using fast nearest neighbour search
ix <- nn2(
  data  = pc4.sf %>% st_centroid %>% st_coordinates,
  query = pop.pc6.sf[jx, ] %>% st_coordinates,
  k = 1) %>% .$nn.idx %>% as.vector
# Assign nearest pc4 to these pc6's
pc4.pc6 <- pc4.pc6 + sparseMatrix(i = ix, j = jx, dims = c(nrow(pc4.sf), nrow(pop.pc6.sf)), x = 1)


# Population weighted number of animals within 1 km for each pc4 ----
#
pc4.sf %>% st_area %>% as.numeric

# Create 1 km buffer around each pc6 point (also done for 100 m, 500 m and 1500 m to investigate dose-response relationship)
pop.pc6.buffer.sf <- pop.pc6.sf %>%
  st_buffer(dist = 1000, nQuadSegs = 5)

# Spatial left join pop.pc6.buffer.sf with animal.xy.sf
animal.pc6.buffer <- st_join(pop.pc6.buffer.sf, animal.xy.sf, left = TRUE) %>%
  #    Replace NAs with 0
  mutate_at(
    .vars = vars(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                 Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen),
    .funs = funs(replace(., list = is.na(.), values = 0)))

# Sum animals within each pc6 1 km buffer
# (use aggregate, is much faster than dplyr's group_by/summarize)
pc6.animal.data <- aggregate(
  formula = cbind(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                  Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen) ~ pc6nr,
  FUN = sum,
  data = animal.pc6.buffer)

# Construct population weight matrix
# Transpose pc4.pc6 for easier calculations
pc6.pc4 <- t(pc4.pc6)
# Add pc6 population numbers to columns of pc6.pc4
pc6.pc4 <- pc6.pc4 * pop.pc6.sf$population
# Calculate pc6 population fractions for each pc4 by dividing each column by its pc4 total (use trick)
pc6.pc4@x <- pc6.pc4@x / rep.int(colSums(pc6.pc4), diff(pc6.pc4@p))

# Calculate weighted number of animals within 1 km for each pc4
animal.pc4.sf <- cbind(
  pc4.sf,
  (t(pc6.pc4) %*% (pc6.animal.data %>% select(-pc6nr) %>% as.matrix)) %>% as.matrix)

#now we have a dataframe with the pc4 and pop weighted numbers: combine this with titer data (based on pc4)


# -------------- PIENTER DATA INLADEN ----------------------- #

#read pienter data 2006-2007
pienter <- read_excel(".../data/.../Pienter_campy.xlsx")
pienter_data <- pienter
#rename columns
pienter_data <- pienter_data %>% 
  rename(birth.date = gebdat2) %>%
  rename(sex = gesl2) %>%
  rename(pc4 = `ZipCode`) %>%
  rename(onset.date = invuldat)

class(pienter_data$birth.date)
class(pienter_data$onset.date)

pienter_data$birth.date <- as.Date(pienter_data$birth.date, format('%Y-%m-%d'))
pienter_data$onset.date <- as.Date(pienter_data$onset.date, format('%Y-%m-%d'))

#calculate interval for age calculation 
pienter_data$Interval_data <- pienter_data$onset.date - pienter_data$birth.date
#convert to years as integer
pienter_data$age <- as.integer(pienter_data$Interval_data / 365)

#create year column and select correct years (2006-2007)
pienter_data$year <- format(pienter_data$onset.date, format = "%Y")
pienter_data <- filter(pienter_data, pienter_data$year == 2007|pienter_data$year == 2006)

#remove duplicated rows
pienter_data <- distinct(pienter_data)

#remove cases with unknown age
pienter_data <- pienter_data[!is.na(pienter_data$age),]

# remove NA's in sex
pienter_data <- pienter_data[!is.na(pienter_data$sex), ]
# relabel sex 1 = male, 2 = female
pienter_data$sex[which(pienter_data$sex == 1)] <- "male"
pienter_data$sex[which(pienter_data$sex == 2)] <- "female"


#filter on PC4 > 0 and < 9999  
pienter_data <- pienter_data[(pienter_data$pc4 > 0 & pienter_data$pc4 < 9999), ]

#remove cases with unknown seroincidence
pienter_data <- pienter_data[!is.na(pienter_data$seroincidence), ]

#select relevant columns
pienter_data <- pienter_data %>% select(onset.date, sex, pc4, year, age, IgM, IgG, IgA, seroincidence) 

#sort by pc4
pienter_data <- pienter_data %>% arrange(pc4)
#convert to dataframe (was tibble) 
pienter_data <- pienter_data %>% as.data.frame 
#view(pienter_data)

#create year column and select correct year
pienter_data <- filter(pienter_data, pienter_data$year == 2007)

# now merge pop weighted nr dataframe with pienter data, based on pc4 
pc4_data_2007 <- merge(pienter_data, animal.pc4.sf, by = "pc4") #dataframe with both titer values and pop weighted animal nrs
pc4_data_2007 <- na.omit(pc4_data_2007) 
pc4_data_2007$pc4 <- as.factor(pc4_data_2007$pc4)

# --------------------------------------------------2006--------------------------------------------------------------------------------------
# Set years
years <- 2006
n.years <- length(years)

# Read pc4 population numbers for each year
pop.pc4.year.data <- vector(mode = "list", length = n.years)
for (i in seq_len(n.years)) {
  pop.pc4.year.data[[i]] <- read.delim(file = paste0(".../data/.../BevolkingPerPostcode_1januari", years[i], ".txt")) %>%
    # Select relevant columns
    select(pc4, mannen_0tot5:mannen_95tot100, vrouwen_0tot5:vrouwen_95tot100) %>%
    # Reshape population numbers into long format
    gather(key = "sex_agecat", value = "population", -pc4) %>%
    # Add year
    mutate(year = years[i])
}

### NOTE PC4 shapefile in map 2006 is the same as the one of 2007, because there is no pc4 available for 2006

# Read pc4 polygons for each year
pc4.year.sf <- vector(mode = "list", length = n.years)
for (i in seq_len(n.years)) {
  pc4.year.sf[[i]] <- st_read(
    dsn = paste0(".../data/Exposure dataset pienter/", years[i]),
    layer = paste0("pc4_", years[i]), 
    type = 6, 
    quiet = TRUE) %>%
    # Add year
    mutate(year = years[i]) %>%
    # Set CRS to 29882 (ignore warning)
    st_set_crs(28992)
}

#make pc4.sf 
pc4.sf <- st_read(
  dsn = paste0(".../data/Exposure dataset pienter/2007"),
  layer = paste0("pc4_2007"), 
  type = 6, 
  quiet = TRUE) %>%
  # Add year
  mutate(year = 2007) %>%
  # Set CRS to 29882 (ignore warning)
  st_set_crs(28992)

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

# Read pc6 points and population (2006)
pop.pc6.sf <- read.delim(file = ".../data/.../pc6_2006.txt", sep = ";") %>%
  # Rename ppcpers -> population
  rename(population = ppcpers) %>%
  # Drop pc6 with 0 inhabitants
  filter(population > 0) %>%
  # Convert to sf object
  st_as_sf(coords = c("xcoord", "ycoord")) %>%
  # Set CRS to 29882 (ignore warning)
  st_set_crs(28992)

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

# Make intersection between pc4s and pc6 (as sparse matrix)
# (same for each year, therefore outside the loop)
pc4.pc6 <- st_intersects(pc4.sf, pop.pc6.sf)
pc4.pc6 <- sparseMatrix(
  i = pc4.pc6 %>% seq_along %>% rep(times = pc4.pc6 %>% sapply(FUN = length)),
  j = pc4.pc6 %>% unlist,
  dims = c(nrow(pc4.sf), nrow(pop.pc6.sf)),
  x = 1)

# Some pc6's have population or cases (after distribution) but these are not aggregated because there is no pc4 associated with them
# Which pc6's have no pc4?
jx <- which(colSums(pc4.pc6) == 0)
# Find nearest pc4 using fast nearest neighbour search
ix <- nn2(
  data  = pc4.sf %>% st_centroid %>% st_coordinates,
  query = pop.pc6.sf[jx, ] %>% st_coordinates,
  k = 1) %>% .$nn.idx %>% as.vector
# Assign nearest pc4 to these pc6's
pc4.pc6 <- pc4.pc6 + sparseMatrix(i = ix, j = jx, dims = c(nrow(pc4.sf), nrow(pop.pc6.sf)), x = 1)

# Population weighted number of animals within 1 km for each pc4 ----
#
pc4.sf %>% st_area %>% as.numeric

# Create 1 km buffer around each pc6 point (also done for 100 m, 500 m and 1500 m to investigate dose-response relationship)
pop.pc6.buffer.sf <- pop.pc6.sf %>%
  st_buffer(dist = 1000, nQuadSegs = 5)

# Spatial left join pop.pc6.buffer.sf with animal.xy.sf
animal.pc6.buffer <- st_join(pop.pc6.buffer.sf, animal.xy.sf, left = TRUE) %>%
  #    Replace NAs with 0
  mutate_at(
    .vars = vars(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                 Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen),
    .funs = funs(replace(., list = is.na(.), values = 0)))

# Sum animals within each pc6 1 km buffer
# (use aggregate, is much faster than dplyr's group_by/summarize)
pc6.animal.data <- aggregate(
  formula = cbind(Pluimvee_totaal, Varkens_totaal, Rundvee_totaal, Geiten_totaal, Schapen_totaal, Kleine_Herkauwers_totaal,
                  Rund_Melk_totaal, Rund_Vlees_totaal, Kip_Kuiken, Kip_Leghen) ~ pc6nr,
  FUN = sum,
  data = animal.pc6.buffer)

# Construct population weight matrix
# Transpose pc4.pc6 for easier calculations
pc6.pc4 <- t(pc4.pc6)
# Add pc6 population numbers to columns of pc6.pc4
pc6.pc4 <- pc6.pc4 * pop.pc6.sf$population
# Calculate pc6 population fractions for each pc4 by dividing each column by its pc4 total (use trick)
pc6.pc4@x <- pc6.pc4@x / rep.int(colSums(pc6.pc4), diff(pc6.pc4@p))

# Calculate weighted number of animals within 1 km for each pc4
animal.pc4.sf <- cbind(
  pc4.sf,
  (t(pc6.pc4) %*% (pc6.animal.data %>% select(-pc6nr) %>% as.matrix)) %>% as.matrix)

#now we have a dataframe with the pc4 and pop weighted numbers: combine this with titer data (based on pc4)


# -------------- load PIENTER DATA ----------------------- #

#read pienter data 2006-2007
pienter <- read_excel(".../data/Exposure dataset pienter/Pienter_campy.xlsx")
pienter_data <- pienter
#rename columns
pienter_data <- pienter_data %>% 
  rename(birth.date = gebdat2) %>%
  rename(sex = gesl2) %>%
  rename(pc4 = `ZipCode`) %>%
  rename(onset.date = invuldat)

class(pienter_data$birth.date)
class(pienter_data$onset.date)

pienter_data$birth.date <- as.Date(pienter_data$birth.date, format('%Y-%m-%d'))
pienter_data$onset.date <- as.Date(pienter_data$onset.date, format('%Y-%m-%d'))

#calculate interval for age calculation 
pienter_data$Interval_data <- pienter_data$onset.date - pienter_data$birth.date
#convert to years as integer
pienter_data$age <- as.integer(pienter_data$Interval_data / 365)

#remove cases with unknown age
pienter_data <- pienter_data[!is.na(pienter_data$age),]

#remove duplicated rows
pienter_data <- distinct(pienter_data)
# remove NA's in sex
pienter_data <- pienter_data[!is.na(pienter_data$sex), ]
# rename labes sex to 1 = male, 2 = female
pienter_data$sex[which(pienter_data$sex == 1)] <- "male"
pienter_data$sex[which(pienter_data$sex == 2)] <- "female"

#filter on PC4 > 0 and < 9999  
pienter_data <- pienter_data[(pienter_data$pc4 > 0 & pienter_data$pc4 < 9999), ]

#select relevant columns
pienter_data <- pienter_data %>% select(onset.date, sex, pc4, age, IgM, IgG, IgA, seroincidence) 

#sort by pc4
pienter_data <- pienter_data %>% arrange(pc4)
#convert to dataframe (was tibble) 
pienter_data <- pienter_data %>% as.data.frame 
#view(pienter_data)

#create year column and select correct year
pienter_data$year <- format(pienter_data$onset.date, format = "%Y")
pienter_data <- filter(pienter_data, pienter_data$year == 2006)


# now merge pop weighted nr dataframe with pienter data, based on pc4 
pc4_data_2006 <- merge(pienter_data, animal.pc4.sf, by = "pc4") #dataframe with both titer values and pop weighted animal nrs
pc4_data_2006 <- na.omit(pc4_data_2006) 
pc4_data_2006$pc4 <- as.factor(pc4_data_2006$pc4)

## combine datasets 2006 and 2007
pc4_data <- rbind(pc4_data_2006,pc4_data_2007)

# Calculate Age categories
pc4_data <- pc4_data %>%
  mutate(
    # Add age categories
    agecat = as.numeric(age) %>% cut(breaks = age.breaks, right = FALSE, include.lowest = TRUE))

# Create maps of seroincidence
# pc4_data_2 <- pc4_data %>%
#    group_by(pc4) %>%
#   summarise(seroincidence = mean(seroincidence))

# pc4.sf$seroincidence <- pc4_data_2$seroincidence[match(pc4.sf$pc4, pc4_data_2$pc4)]
# 
# # export shapefile to work in ArcGIS with it
#  st_write(pc4.sf, "/s-schijf/mulderac/Campylobacter - spatial/results/pc4.sf.seroincidence.shp")

# export as rds file
#saveRDS(pc4.sf, file = ".../pc4.sf.seroincidence.Rds")

#make separate dataframes, with age groups etc 
# with log of all Ig's 
pc4_data$IgM <- as.numeric(pc4_data$IgM)
pc4_data$IgM_log <- log(pc4_data$IgM + 1)

pc4_data$IgG <- as.numeric(pc4_data$IgG)
pc4_data$IgG_log <- log(pc4_data$IgG + 1)

pc4_data$IgA <- as.numeric(pc4_data$IgA)
pc4_data$IgA_log <- log(pc4_data$IgA + 1)

#take log of incidence as well, since very small nrs 
pc4_data$seroincidence <- as.numeric(pc4_data$seroincidence)
pc4_data$seroincidence <- log(pc4_data$seroincidence + 1)

#with log of all pop weighted animal nrs 
pc4_data$Varkens_totaal <- log2(pc4_data$Varkens_totaal + 1)
pc4_data$Kleine_Herkauwers_totaal <- log2(pc4_data$Kleine_Herkauwers_totaal + 1)
pc4_data$Rund_Melk_totaal <- log2(pc4_data$Rund_Melk_totaal + 1)
pc4_data$Rund_Vlees_totaal <- log2(pc4_data$Rund_Vlees_totaal + 1)
pc4_data$Kip_Kuiken <- log2(pc4_data$Kip_Kuiken + 1)
pc4_data$Kip_Leghen <- log2(pc4_data$Kip_Leghen + 1)

#-----model -----#
# first analyse data with mixed linear model, to see random effect of PC4 clustering 
library(lme4)
# to obtain p-values of fixed effects, use package lmerTest, then they will automatically be attached
library(lmerTest)
# For t-test interpretations, a similar approach was used as by Luke (2017), where the null hypothesis was rejected for t-values lower than -1.96 and bigger than +1.96 (Luke, 2017).

# seroincidence 
# first with mixed model 

#including relevel function
pc4_data$agecat <-relevel(pc4_data$agecat, ref = "[10,50)")
pc4_data$sex <- factor(pc4_data$sex, ordered = FALSE )
pc4_data$sex <-relevel(pc4_data$sex, ref = "male")

# Dairy cows 
model1 <- lmer(seroincidence ~ Rund_Melk_totaal + (1 | pc4), data = pc4_data)
summary(model1)
confint(model1)
# Veal calves
model2 <- lmer(seroincidence ~ Rund_Vlees_totaal + (1 | pc4), data = pc4_data)
summary(model2)
confint(model2)
# Pigs
model3 <- lmer(seroincidence ~ Varkens_totaal + (1 | pc4), data = pc4_data)
summary(model3)
confint(model3)
# Broiler chickens
model4 <- lmer(seroincidence ~ Kip_Kuiken + (1 | pc4), data = pc4_data)
summary(model4)
confint(model4)
# Laying hens
model5 <- lmer(seroincidence ~ Kip_Leghen + (1 | pc4), data = pc4_data)
summary(model5)
confint(model5)
# Small ruminants
model6 <- lmer(seroincidence ~ Kleine_Herkauwers_totaal + (1 | pc4), data = pc4_data)
summary(model6)
confint(model6)
# Agecat
model7 <- lmer(seroincidence ~ agecat + (1 | pc4), data = pc4_data)
summary(model7)
confint(model7)
# Sex
model8 <- lmer(seroincidence ~ sex + (1 | pc4), data = pc4_data)
summary(model8)
confint(model8)

# Multivariate
model9 <- lmer(seroincidence ~ agecat + sex + Rund_Melk_totaal + Rund_Vlees_totaal + Varkens_totaal
             + Kip_Kuiken + Kip_Leghen + Kleine_Herkauwers_totaal + (1 | pc4), data=pc4_data)
summary(model9)
confint(model9)