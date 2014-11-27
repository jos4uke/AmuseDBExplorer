library(dplyr)
library(RODBC)

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

# clean
cleantable <- db.res %.%
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
