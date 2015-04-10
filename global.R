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
db.bioch.all <- db.res %.%
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
db.bioch.all.clean <- db.bioch.all %.%
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

# choices in mucilbiochcols select box
choices_mucilbiochcols <- list(
  "Galacturonic Acid" = 'Gal_A',
  "Neutral oses" = 'OsesNeutres',
  "Molecular weight" = 'MW',
  "Intrisic viscosity" = 'IV',
  "Giration radius" = 'RG',
  "Hydrodynamic radius" = 'RH'
  )
