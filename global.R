suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(RODBC))

# AmuseDB connection
con <- odbcConnect("Amuse")

# sql
sql <- "select g.av_nr, p.culture, p.seed_pool, d.repet_nr, b.gal_a, b.oz_n, b.mw, b.iv, b.rg, b.rh, g.city, g.country
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
## filtering ND values allows to sort columns 5:10 numerically
db.bioch.all.clean[,5:10] <- sapply(5:10, function(i){
  as.numeric(as.vector(db.bioch.all.clean[,i]))
})

# 4 plants accessions w/o missing values
## count plants by accession
p4 <- db.bioch.all.clean %>%
  count(AV, sort = TRUE) %>%
  filter(n == 4) %>%
  select(AV)
 
# db.bioch.4p.clean <- db.bioch.all.clean %>%
#   filter(AV %in% p4$AV)

# 4 plants accessions summary
Q1 <- function(x){
  quantile(x)[2]
}

Q3 <- function(x){
  quantile(x)[4]
}

db.bioch.4p.summary <- db.bioch.4p.clean %>%
  select(AV, Gal_A, OsesNeutres, MW, IV, RG, RH) %>%
  group_by(AV) %>%
  summarise_each(funs(min, Q1, median, mean, Q3, max, IQR, sd))
  
# choices in mucilbiochcols select box
choices_mucilbiochcols <- list(
  "Galacturonic Acid" = 'Gal_A',
  "Neutral oses" = 'OsesNeutres',
  "Molecular weight" = 'MW',
  "Intrinsic viscosity" = 'IV',
  "Giration radius" = 'RG',
  "Hydrodynamic radius" = 'RH'
  )

## geo climato data
db.climate.all <- read.table(file="data/AMUSE_accession_climate_clean_data.tab", header=TRUE, sep="\t", quote ="")
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

# choices climatodatasets select box
choices_climatodatasets <- list(
  "MONTHLY HOURS OF SUNSHINE" = 'mhs',
  "MONTHLY PRECIPITATION" = 'mp',
  "MEAN MONTHLY NUMBER OF RAIN DAYS" = 'mmnrd',
  "MEAN MONTHLY TEMPERATURE" = 'mmt',
  "MEAN MONTHLY TEMP RANGE" = 'mmtr'
)
