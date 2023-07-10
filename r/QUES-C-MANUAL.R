library(terra)
library(tidyverse)

# setwd("D:/MW/DATA/PeatIMPACTS/RPPEG_KALBAR_v2/")

# tmult <- 12.5 #(T2-T1)/2
em_fac <- read_csv("data/cstock_final.csv")
colnames(em_fac) <- c("ID" ,"LC", "faktor_emisi")
# rcl_klhk <- read_csv("LUMENS_INPUT/Tabular/lc_rcl_klhk.csv") |> 
#   as.data.frame()

# basemap <- rast("d:/MW/DATA/PeatIMPACTS/RPPEG_SUMSEL_v3/05LULC/cleaned_AP/AMBI2025cr.tif")

landuse1 <- rast("data/LC/LC2016_VER3.tif") 
names(landuse1) <- c("LC1")
landuse2 <- rast("data/LC/LC2022_VER3.tif")
names(landuse2) <- c("LC2")
landuse_stack <- c(landuse1, landuse2)
crosstab_ <- crosstab(landuse_stack) |> 
  as_tibble()
crosstab_$LC1 <- as.numeric(crosstab_$LC1)
crosstab_$LC2 <- as.numeric(crosstab_$LC2)
crosstab_wide <- pivot_wider(crosstab_, names_from = LC1, values_from = n)

crosstab_join <- left_join(crosstab_, em_fac, join_by(LC1 == ID)) |> 
  rename(faktor_emisi_LC1 = faktor_emisi) |> 
  left_join(em_fac, join_by(LC2 == ID)) |> 
  rename(faktor_emisi_LC2 = faktor_emisi) 

crosstab_join$em_raw <- (crosstab_join$faktor_emisi_LC1 - crosstab_join$faktor_emisi_LC2) / 6
crosstab_join$em_raw[is.na(crosstab_join$em_raw)] <- 0
crosstab_join$emisi <-  crosstab_join$n * crosstab_join$em_raw
sum(crosstab_join$emisi)


# peta karbon
landuse1 |>
  classify(em_fac[,-2]) -> carbon1
landuse2 |>
  classify(em_fac[,-2]) -> carbon2
chk_em<-carbon1>carbon2
chk_sq<-carbon1<carbon2
emission<-((carbon1-carbon2)*3.67)*chk_em
sequestration<-((carbon2-carbon1)*3.67)*chk_sq
comb_result <- c(carbon1, carbon2, emission, sequestration)
names(comb_result) <- c("Carbon T1", "Cabron T2", "Emission", "Sequestration")
plot(comb_result)

# save raster
writeRaster(carbon1, "result/carbon_2016.tif")
writeRaster(carbon2, "result/carbon_2022.tif")
writeRaster(emission, "result/emission_2016-2022.tif")
writeRaster(sequestration, "result/sequestration_2016-2022.tif")
write_csv(crosstab_join, "result/crosstab_join_2016-2022.csv")
save.image("result/ques_c_mineral.RData")
