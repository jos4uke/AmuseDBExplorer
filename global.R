suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(RODBC))

## geo climato data
db.climate.all <- read.table(file="data/AMUSE_accession_climate_clean_data_UTF8.tab", header=TRUE, sep="\t", quote ="")
# tbl_df seems not working on vega though same dplyr version 
# db.clmate.all <- tbl_df(db.climate.df)
# rename does not work!
# dplyr::rename_(db.climate.all, .dots=setNames(list("name"),"NAME"))
# dplyr::rename_(db.climate.all, .dots=setNames(list("av"), "AV"))
# dplyr::rename_(db.climate.all, .dots=setNames(list("city"), "CITY"))
# dplyr::rename_(db.climate.all, .dots=setNames(list("country"), "COUNTRY"))
# dplyr::rename_(db.climate.all, .dots=setNames(list("alt"), "ALTITUDE"))
# dplyr::rename_(db.climate.all, .dots=setNames(list("gps_lat"), "LATITUDE"))
# dplyr::rename_(db.climate.all, .dots=setNames(list("gps_long"), "LONGITUDE"))
# dplyr::rename_(db.climate.all, .dots=setNames(list("geo_qual"), "GEOLOC_QUAL"))

names(db.climate.all)[1:8] <- c('NAME', 'AV', 'CITY', 'COUNTRY', 'ALTITUDE', 'LATITUDE', 'LONGITUDE', 'GEOLOC_QUAL')
db.climate.geoloc <- db.climate.all %>%
  select(
    NAME, 
    AV,  
    CITY,	
    COUNTRY,	
    ALTITUDE,	
    LATITUDE,	
    LONGITUDE,
    GEOLOC_QUAL
  )

db.climate.geoloc[6:7] <- sapply(6:7, function(i){
  as.numeric(as.vector(db.climate.geoloc[,i]))
})

### accessions with gps coordinates
acc_gps <- db.climate.geoloc %>%
  filter(!(is.na(LATITUDE) & is.na(LONGITUDE)))
### accessions without gps coordinates
acc_wogps <- db.climate.geoloc %>%
  filter(is.na(LATITUDE) & is.na(LONGITUDE))

### gps coordinates: LATITUDE/LONGITUDE
min_lat <- min(db.climate.geoloc$LATITUDE, na.rm=TRUE)-0.5
max_lat <- max(db.climate.geoloc$LATITUDE, na.rm=TRUE)+0.5
min_long <- min(db.climate.geoloc$LONGITUDE, na.rm=TRUE)-0.5
max_long <- max(db.climate.geoloc$LONGITUDE, na.rm=TRUE)+0.5

# choices climatodatasets select box
choices_climatodatasets <- list(
  "MONTHLY HOURS OF SUNSHINE" = 'mhs',
  "MONTHLY PRECIPITATION" = 'mp',
  "MEAN MONTHLY NUMBER OF RAIN DAYS" = 'mmnrd',
  "MEAN MONTHLY TEMPERATURE" = 'mmt',
  "MEAN MONTHLY TEMP RANGE" = 'mmtr'
)

# search accessions by name on map
choices_acc_names_map <- as.list(na.omit(db.climate.geoloc$NAME))
names(choices_acc_names_map) <- na.omit(db.climate.geoloc$NAME)
choices_acc_names_map$All <- "All"

# search accessions by geoloc quality
choices_geoloc_qual <- as.list(unique(db.climate.geoloc$GEOLOC_QUAL))
names(choices_geoloc_qual) <- unique(db.climate.geoloc$GEOLOC_QUAL)

# Define colors
geoloc_qual <- unique(db.climate.geoloc$GEOLOC_QUAL)
qual_colors <- c("green", "orange", "blue", "grey")
names(qual_colors) <- geoloc_qual
colors <- qual_colors[as.character(db.climate.geoloc$GEOLOC_QUAL)]

# AmuseDB connection
con <- odbcConnect("Amuse")

# sql
sql <- "select g.acc_name, g.av_nr, p.culture, p.seed_pool, d.repet_nr, b.gal_a, b.oz_n, b.mw, b.iv, b.rg, b.rh, g.city, g.country
from am_genotype as g
join am_plant as p on p.am_genotype_id=g.id
join am_data as d on d.am_plant_id=p.id
join am_data_bioch as b on b.am_data_id=d.id"

# query
db.res <- sqlQuery(con, sql)

# close
odbcClose(con)

# all plants
db.bioch.all <- db.res %>%
  select(
    NAME = acc_name,
    AV = av_nr,
    Culture = culture,
    SeedPool = seed_pool,
    RepetNbr = repet_nr,
    Gal_A = gal_a,
    OsesNeutres = oz_n,
    MW = mw,
    IV = iv,
    RG = rg,
    RH = rh,
    City = city,
    Country = country
  )
## NB: missing values as ND prevent numerical sort on columns 5:10
# accession AV2 was not filetred in sql query

# all plants w/o missing values
db.bioch.all.clean <- db.bioch.all %>%
  filter(
    Gal_A != "ND",
    OsesNeutres != "ND",
    MW != "ND",
    IV != "ND",
    RG != "ND",
    RH != "ND"
  )
## filtering ND values allows to sort columns 6:11 numerically
db.bioch.all.clean[,6:11] <- sapply(6:11, function(i){
  as.numeric(as.vector(db.bioch.all.clean[,i]))
})

# update raw dataset
## add name
#db.bioch.all.clean <- dplyr::left_join(db.bioch.all.clean, db.climate.geoloc[,c("AV", "NAME")], by = "AV")

# incomplete datasets
db.bioch.incomplete <- db.bioch.all %>%
  filter(
#     !(AV %in% unique(db.bioch.all.clean$AV)) # not satisfying
      !(Gal_A != "ND") | is.na(Gal_A) | is.null(Gal_A) |
      !(OsesNeutres != "ND") | is.na(OsesNeutres) | is.null(OsesNeutres) |  
      !(MM != "ND") | is.na(MM) | is.null(MM) |
      !(IV != "ND") | is.na(IV) | is.null(IV) |
      !(RG != "ND") | is.na(RG) | is.null(RG) |
      !(RH != "ND") | is.na(RH) | is.null(RH)
    ) 

# controls
controls_av <- seq(from = 1000, to = 7000, by = 1000)
db.bioch.controls <- db.bioch.all.clean %>%
  filter(
    AV %in% controls_av
    )

# search accessions by name
choices_acc_names <- as.list(na.omit(db.bioch.all.clean$NAME))
names(choices_acc_names) <- na.omit(db.bioch.all.clean$NAME)
choices_acc_names$All <- "All"

# 4 plants accessions w/o missing values
## count plants by accession
p4 <- db.bioch.all %>%
  count(AV, sort = TRUE) %>%
  filter(n == 4) %>%
  select(AV)
 
# db.bioch.4p.clean <- db.bioch.all.clean %>%
#   filter(AV %in% p4$AV)

# 4 plants accessions summary
Q1 <- function(x){
  quantile(x, na.rm=TRUE)[2]
}

Q3 <- function(x){
  quantile(x, na.rm=TRUE)[4]
}

db.bioch.4p.summary <- db.bioch.all.clean %>%
  filter(AV %in% p4$AV) %>%
  select(NAME, AV, Gal_A, OsesNeutres, MM, IV, RG, RH) %>%
  group_by(NAME, AV) %>%
  summarise_each(funs(min(., na.rm = TRUE), Q1, median(., na.rm = TRUE), mean(., na.rm = TRUE), Q3, max(., na.rm = TRUE), IQR(., na.rm = TRUE), sd(., na.rm = TRUE)))
  
# choices in mucilbiochcols select box
choices_mucilbiochcols <- list(
  "Galacturonic Acid" = 'Gal_A',
  "Neutral oses" = 'OsesNeutres',
  "Molecular weight" = 'MW',
  "Intrinsic viscosity" = 'IV',
  "Giration radius" = 'RG',
  "Hydrodynamic radius" = 'RH'
  )

# choices in summarycols select box
choices_summarycols <- list(
  "minimum" = "_min",
  "first quartile (Q1)" = "_Q1",
  "median" = "_median",
  "mean" = "_mean",
  "third quartile (Q3)" = "_Q3",
  "maximum" = "_max",
  "interquartile range (IQR)" = "_IQR",
  "standard deviation"= "_sd"
  )

# no 4p accessions
db.bioch.no4p <- db.bioch.all.clean %>%
  filter(!(AV %in% p4$AV))

# choices in incomplete
choices_incompletedatasets <- list(
  "raw with less than 4 plants" = "rlt4p",
  "raw with NA/ND values" = "rnand",
  "geoclimato without gps coordinates" = "gnogps"
  )