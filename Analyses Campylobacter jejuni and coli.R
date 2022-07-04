
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
years <- 2014:2019
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

# Read pc4 polygons for each year
pc4.year.sf <- vector(mode = "list", length = n.years)
for (i in seq_len(n.years)) {
  pc4.year.sf[[i]] <- st_read(
    dsn = paste0(".../data/pc4 - poly/", years[i]),
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

# Check dubbele postcodes
sapply(X = pc4.year.sf, FUN = function(data) data$pc4 %>% duplicated %>% sum)
# Are there any duplicated records? Which?
pc4.year.sf[[i]]$pc4 %>% duplicated %>% any
pc4.year.sf$pc4 %>% duplicated %>% which

# Combine separate pc4 features into one feature
dubl <- pc4.year.sf[[i]]$pc4 %>% duplicated %>% which
for (dubl.i in dubl) {
# Which pc4 is duplicated?
 pc4.dubl <- pc4.year.sf[[i]][dubl.i, ]$pc4
x = pc4.year.sf[[i]] %>% filter(pc4 == pc4.dubl)
 }
pc4.year.sf[[i]][3990:3992, ]
pc4.year.sf[[i]]$pc4[[3992]]

# Read pc6 points and population (2016)
pop.pc6.sf <- read.delim(file = ".../data/.../pc6_2016.txt") %>%
  # Rename inwoners -> population
  rename(population = inwoners) %>%
  # Drop pc6 with 0 inhabitants
  filter(population > 0) %>%
  # Convert to sf object
  st_as_sf(coords = c("x", "y")) %>%
  # Set CRS to 29882 (ignore warning)
  st_set_crs(28992)


# Read hexagonal reference map
# Area is 90 km^2
hex.sf <- readRDS(file = ".../data/.../hex_090.rds")

# Area is 50 km^2
#hex.sf <- readRDS(file = ".../data/.../hex_050.rds")

# 
# # Area is 25 km^2
#hex.sf <- readRDS(file = ".../data/.../hex_025.rds")

# Area is 10 km^2
#hex.sf <- readRDS(file = ".../data/.../hex_010.rds")

#read campylobacter pc4 data
Campylobacter_2014_2019 <- read.table(".../data/__.txt", header = TRUE, sep = "\t")

#rename columns
Campylobacter_pc4_2014_2019 <- Campylobacter_2014_2019 %>%
                                rename(age = leeftijd) %>%
                                rename(onset.date = opnamedatum) %>%
                                rename(year = afname_jaar) %>%
                                rename(sex = geslacht) %>%
                                rename(afname.date = afnamedatum) %>%
                                rename(patient.nr = patientid_anoniem)
# If onset.date is missing, replace by afname.date
Campylobacter_pc4_2014_2019$onset.date <- ifelse(Campylobacter_pc4_2014_2019$onset.date == "", yes = Campylobacter_pc4_2014_2019$afname.date, no = Campylobacter_pc4_2014_2019$onset.date)

# Adapt onset.date class to date variable
# check class of date in dataframe
class(Campylobacter_pc4_2014_2019$onset.date)
# change class from date to 'date' format and change the format itself
Campylobacter_pc4_2014_2019$onset.date <- as.Date(Campylobacter_pc4_2014_2019$onset.date, format('%m/%d/%Y'))

# redefine year column as year variable
Campylobacter_pc4_2014_2019$year <- format(Campylobacter_pc4_2014_2019$year, format = "%Y")

# select relevant columns (patient.nr, onset.date, sex, pc4, age, sex, rap.date, Serotype)
Campylobacter_pc4_2014_2019 <- Campylobacter_pc4_2014_2019 %>% select(patient.nr, onset.date, sex, pc4, age, organisme, year)

# filter C. coli and C. jejuni
strings <- c("Campylobacter jejuni", "Campylobacter coli")
campylobacter_pc4 <- Campylobacter_pc4_2014_2019 %>%
  filter(str_detect(organisme, paste(strings, collapse = "|")))

#remove rows in age with left over NA values
campylobacter_pc4 <- campylobacter_pc4[!is.na(campylobacter_pc4$age),]

#remove duplicated rows
campylobacter_pc4 <- distinct(campylobacter_pc4)

# remove NA's in sex
sum(is.na(campylobacter_pc4)) # 0 NA's

#filter on PC4 > 0 and < 9999
campylobacter_pc4 <- campylobacter_pc4[(campylobacter_pc4$pc4 > 0 & campylobacter_pc4$pc4 < 9999), ]

# Filter unique pat. nr. 2 months apart
campylobacter_pc4 <- campylobacter_pc4 %>% arrange(patient.nr)
campylobacter_pc4 <- campylobacter_pc4 %>%
  mutate(daysbetween = onset.date - lag(onset.date))

#make duplicate column, but first change class of patient nr to numeric (was character)
campylobacter_pc4$patient.nr <- as.numeric(campylobacter_pc4$patient.nr)
#then make duplicate column by subtracting
campylobacter_pc4 <- campylobacter_pc4 %>%
  mutate(duplicate = patient.nr - lag(patient.nr))

#now remove rows if: days between < 61 (2 months) and duplicate == 0
campylobacter_pc4 <- campylobacter_pc4[!(campylobacter_pc4$daysbetween <= 61 & campylobacter_pc4$duplicate == 0), ]
#remove rows with only NAs in columns
campylobacter_pc4 <- campylobacter_pc4[rowSums(is.na(campylobacter_pc4)) != ncol(campylobacter_pc4), ]

#remove rows with left over NA values
campylobacter_pc4 <- na.omit(campylobacter_pc4)
#sort by pc4
campylobacter_pc4 <- campylobacter_pc4 %>% arrange(pc4)
#convert to dataframe (was tibble)
campylobacter_pc4 <- campylobacter_pc4 %>% as.data.frame

#select relevant columns (onset.date, sex, pc4, age, sex, organisme, year)
campylobacter_pc4 <- campylobacter_pc4 %>% select(onset.date, sex, pc4, age, organisme, year)

#select data based on column "organisme" = species (jejuni or coli)
campylobacter_pc4 <- campylobacter_pc4 %>%
  filter(str_detect(organisme, "Campylobacter jejuni"))

# campylobacter_pc4 <- campylobacter_pc4 %>%
#   filter(str_detect(organisme, "Campylobacter coli"))

# ------------------------------------------------------------------- ANIMAL DATA -------------------------------------------------------------------------------------------------------------------

# Read point animal data (2015)
animal.xy.sf.2015 <- read_excel(path = ".../data/__.xlsx", sheet = 'Calculations_animal_data') %>%
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
animal.xy.sf.2018 <- read_excel(path = ".../data/__.xlsx", sheet = 'Spatial_analyses_data') %>%
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
animal.xy.sf.2014 <- animal.xy.sf.2015
animal.xy.sf.2014$year <- 2014
#write.table(animal.xy.sf.2014, "Data/.../animal.xy.sf.2014.txt")
animal.xy.sf.2015$year <- 2015
#write.table(animal.xy.sf.2015, "Data/.../animal.xy.sf.2015.txt")
animal.xy.sf.2016 <- animal.xy.sf.2015
animal.xy.sf.2016$year <- 2016
#write.table(animal.xy.sf.2016, "Data/.../animal.xy.sf.2016.txt")
animal.xy.sf.2017 <- animal.xy.sf.2018
animal.xy.sf.2017$year <- 2017
#write.table(animal.xy.sf.2017, "Data/.../animal.xy.sf.2017.txt")
animal.xy.sf.2018$year <- 2018
#write.table(animal.xy.sf.2018, "Data/.../animal.xy.sf.2018.txt")
animal.xy.sf.2019 <- animal.xy.sf.2018
animal.xy.sf.2019$year <- 2019
#write.table(animal.xy.sf.2019, "Data/.../animal.xy.sf.2019.txt")

# Merge dierdatasets
animal.xy.sf <- rbind(animal.xy.sf.2014,animal.xy.sf.2015,animal.xy.sf.2016,animal.xy.sf.2017,animal.xy.sf.2018,animal.xy.sf.2019)

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
campylobacter.pc4.year.data <- campylobacter_pc4 %>%
  # First some modifications
  mutate(
    # Relabel Man -> male and Vrouw -> female
    sex = recode_factor(sex, 'M' = "male", 'F' = "female"),
    # Make year integer
    year = as.integer(year),
    # Add season (summer = May - Oct, winter = Nov - Apr)
    season = month(onset.date) %in% 5:10 %>%
      factor(levels = c(FALSE, TRUE), labels = c("winter", "summer")),
    # Add age categories
    agecat = as.numeric(age) %>% cut(breaks = age.breaks, right = FALSE, include.lowest = TRUE)) %>%
  # Filter years 2007:2019
  filter(year %in% 2014:2019) %>%
  # Group by year, pc4, agecat, sex, season
  # Summarize cases
  group_by(year, pc4, agecat, sex, season) %>%
  summarize(cases = n()) %>%
  # Convert to dataframe (was tibble)
  as.data.frame %>%
  # Split by year
  split(f = .$year)

# Join pop.pc4.year.data and campylobacter.pc4.data by year
pc4.year.data <- vector(mode = "list", length = n.years)
for (i in seq_len(n.years)) {
  # First complete records to match pc4 levels in pc4.year.sf
  # year, agecat and sex are complete in pop.pc4.year.data
  # season is complete in campylobacter.pc4.year.data
  # pc4 is complete in pc4.year.sf
  tmp <- expand.grid(
    year   = pop.pc4.year.data[[i]]$year %>% unique,
    pc4    = pc4.year.sf[[i]]$pc4,
    agecat = pop.pc4.year.data[[i]]$agecat %>% levels,
    sex    = pop.pc4.year.data[[i]]$sex %>% levels,
    season = campylobacter.pc4.year.data[[i]]$season %>% levels)

  # Left join these complete combinations with pop.pc4.year.data and campylobacter.pc4.year.data
  pop.pc4.year.data[[i]] <- left_join(tmp, pop.pc4.year.data[[i]]) %>%
    mutate(
      # Replace NA's by 0
      population = is.na(population) %>% ifelse(yes = 0, no = population),
      # Divide population by 4 because of season (4x 3 months)
      #population = population / 4)
      # Divide population by 2 because of season (2x 6 months)
      population = population / 2)
  campylobacter.pc4.year.data[[i]] <- left_join(tmp, campylobacter.pc4.year.data[[i]]) %>%
    mutate(
      # Replace NA's by 0
      cases = is.na(cases) %>% ifelse(yes = 0, no = cases))

  # Join pop.pc4.year.data with campylobacter.pc4.data into pc4.year.data
  pc4.year.data[[i]] <- full_join(pop.pc4.year.data[[i]], campylobacter.pc4.year.data[[i]])
}

# -------------------------------------------

# Make intersection between hexagons and pc6 (as sparse matrix)
# (same for each year, therefore outside the loop)
hex.pc6 <- st_intersects(hex.sf, pop.pc6.sf)
hex.pc6 <- sparseMatrix(
  i = hex.pc6 %>% seq_along %>% rep(times = hex.pc6 %>% sapply(FUN = length)),
  j = hex.pc6 %>% unlist,
  dims = c(nrow(hex.sf), nrow(pop.pc6.sf)),
  x = 1,
  repr = "T")

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

# -----------------------Animal data----------------------------------------------------------------------------------------------------

# Spatial left join pop.pc6.buffer.sf with animal.year.xy.sf for each year
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
animal.hex.year.sf <- rbind(animal.hex.sf.2014,animal.hex.sf.2015,animal.hex.sf.2016,animal.hex.sf.2017,animal.hex.sf.2018,animal.hex.sf.2019)

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

# Population
# plot(hex.agg.sf[, "population"],
#      breaks = "fisher", nbreaks = 50,
#      pal = colorRampPalette(brewer.pal(name = "Greys", n = 9)),
#      key.pos = 1,
#      lwd = 0.1,
#      main = "A)")
#
# # Raw incidence
# plot(hex.agg.sf[, "incidence"],
#      breaks = "fisher", nbreaks = 50,
#      pal = colorRampPalette(brewer.pal(name = "Greys", n = 9)),
#      key.pos = 1,
#      lwd = 0.1,
#      main = "S. Enteritidis incidence per 100,000 2007 - 2019")

# Raw incidence new
# Prepare data
# hex.agg.sf$incidence_cat <- ifelse(hex.agg.sf$incidence <= 3.0, "0.0-3.0", 0)
# hex.agg.sf$incidence_cat <- ifelse(hex.agg.sf$incidence > 3.0 & hex.agg.sf$incidence <= 10.0, "3.0-10.0", hex.agg.sf$incidence_cat)
# hex.agg.sf$incidence_cat <- ifelse(hex.agg.sf$incidence > 10.0 & hex.agg.sf$incidence <= 20.0, "10.0-20.0", hex.agg.sf$incidence_cat)
# hex.agg.sf$incidence_cat <- ifelse(hex.agg.sf$incidence > 20.0 & hex.agg.sf$incidence <= 30.0, "20.0-30.0", hex.agg.sf$incidence_cat)
# hex.agg.sf$incidence_cat <- ifelse(hex.agg.sf$incidence > 30.0 & hex.agg.sf$incidence <= 40.0, "30.0-40.0", hex.agg.sf$incidence_cat)
# hex.agg.sf$incidence_cat <- ifelse(hex.agg.sf$incidence > 40.0 & hex.agg.sf$incidence <= 60.0, "40.0-60.0", hex.agg.sf$incidence_cat)
# hex.agg.sf$incidence_cat <- ifelse(hex.agg.sf$incidence > 60.0, ">10.0", hex.agg.sf$incidence_cat)
# hex.agg.sf$incidence <- as.factor(hex.agg.sf$incidence_cat)
# 
# Save dataset
# saveRDS(hex.agg.sf, file = ".../results/.../incidence_jejuni.Rds")

#----------------------------------------------------
# Spatial analysis ----
#
#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

#install INLA op server (if the above code does not work):
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
# Analyses with animals

# Join hex.pop.agg.data with animal.cat.hex.sf
tmp.data <- full_join(hex.pop.agg.data, animal.hex.year.sf)

#--------------select fully covered hexagons based on covered pc4s based on geographical labbias analysis--------------------------------------------------------------


# Read covered PC4s excel file
pc4_covered <- read_excel('.../data/__.xlsx')
pc4_covered$covered <- 1
pc4_covered$PC4 <- pc4_covered$pc4

#keep rows containing covered pc4s
# campylobacter_pc4 <- campylobacter_pc4[(campylobacter_pc4$pc4 %in% pc4_covered$pc4),]

# Read pc4 polygons of 2016
# Chose this year, because calculation incidence in coverage calculation was based on LSI data of 2014-2018 - chose year in the middle
pc4_2016 <- read_sf('.../data/.../pc4_2016.shp')

# Join pc4_covered with pc4_2016
pc4_2016.sf <- full_join(x = pc4_2016, y = pc4_covered, by = "PC4")

# -----------------------

# select covered pc4s
pc4_2016_covered.sf <- subset(pc4_2016.sf, pc4_2016.sf$covered == 1)

# Drop columns
pc4_2016_covered.sf <- subset(pc4_2016_covered.sf, select = c(PC4))

# Intersect covered PC4s with hexagon using st_intersection 
st_geometry(pc4_2016_covered.sf) <- st_sfc(lapply(st_geometry(pc4_2016_covered.sf), function(x) st_buffer(x, 0)), crs = st_crs(pc4_2016_covered.sf))
hex.cov.sf <- st_intersection(hex.sf, pc4_2016_covered.sf)

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
#hex.cov.sf.area2 <- subset(hex.cov.sf.area2, (covered > 99.78)) #100% coverage
#hex.cov.sf.area2 <- subset(hex.cov.sf.area2, (covered > 90)) #>90% coverage
#hex.cov.sf.area2 <- subset(hex.cov.sf.area2, (covered > 70)) #>70% coverage
#hex.cov.sf.area2 <- subset(hex.cov.sf.area2, (covered > 60)) #>60% coverage

# # for export hex areas that are covered:
# hex.sf$covered <- hex.cov.sf.area2$covered[match(hex.sf$hex, hex.cov.sf.area2$hex)]
# drop NA's
# hex.sf <- hex.sf %>%
#   drop_na()
# export
#  saveRDS(hex.sf, file = ".../results/.../covered_hex_campylobacter.Rds")

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
  control.predictor = list(compute = TRUE))

summary(mod1)
result.mod1 <- mod1$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 4)
result.mod1$pvalue <- 2*(1 - pnorm(abs(mod1$summary.fixed[,"mean"]/mod1$summary.fixed[,"sd"])))
result.mod1

# Plot random effects
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

# Plot random effects
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

# Plot random effects
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

# Poultry, Layers
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

# Plot random effects
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

# Plot random effects
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

# Plot random effects
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
  control.predictor = list(compute = TRUE),
  verbose = TRUE)

summary(mod6.1)
result.mod6.1 <- mod6.1$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>% exp %>% round(digits = 4)
result.mod6.1$pvalue <- 2*(1 - pnorm(abs(mod6.1$summary.fixed[,"mean"]/mod6.1$summary.fixed[,"sd"])))
result.mod6.1

# Plot random effects
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

# Plot random effects
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

# Plot random effects
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
#tmp.data.cat.summer <- subset(tmp.data.cat, tmp.data$season == 'summer')

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

# Plot random effects
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

# Plot random effects
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