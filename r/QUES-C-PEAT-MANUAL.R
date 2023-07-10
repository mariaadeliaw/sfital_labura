library(terra)
library(tidyverse)

# setwd("D:/MW/DATA/PeatIMPACTS/RPPEG_KALBAR_v2/")

# tmult <- 12.5 #(T2-T1)/2
peat_em <- read_csv("data/peatem_labura.csv") #peat decomposition factor lookup table
colnames(peat_em) <- c("ID" ,"LC", "faktor_emisi")
em_fac <- read_csv("data/cstock_final.csv")
colnames(em_fac) <- c("ID" ,"LC", "faktor_emisi_mineral")

# basemap <- rast("LUMENS_INPUT/Admin/Kab_RPPEG3_49s_diss.tif")
# peat_map <- vect("RAW_DATA/2_Gambut/FEG Kalbar 49s.shp") |> 
#   project(basemap)

landuse1_peat <- rast("data/peat/LC_16_peat.tif")
# landuse1_peat <- crop(landuse1_peat, peat_map, snap = "in", mask = TRUE)
names(landuse1_peat) <- c("LC1")
landuse2_peat <- rast("data/peat/LC_22_peat.tif") 
# landuse2_peat <- crop(landuse2_peat, peat_map, snap = "in", mask = TRUE)
names(landuse2_peat) <- c("LC2")
landuse_stack_peat <- c(landuse1_peat, landuse2_peat)
crosstab_peat_ <- crosstab(landuse_stack_peat) |> 
  as_data_frame()
crosstab_peat_$LC1 <- as.numeric(crosstab_peat_$LC1)
crosstab_peat_$LC2 <- as.numeric(crosstab_peat_$LC2)
crosstab_wide_peat <- pivot_wider(crosstab_peat_, names_from = LC1, values_from = n)

crosstab_join_peat <- left_join(crosstab_peat_, peat_em, join_by(LC1 == ID)) |> 
  rename(faktor_emisi_LC1 = faktor_emisi) |> 
  left_join(peat_em, join_by(LC2 == ID)) |> 
  rename(faktor_emisi_LC2 = faktor_emisi) |> 
  #join faktor emisi mineral
  left_join(em_fac, join_by(LC1 ==ID), keep = FALSE) |> 
  rename(faktor_emisi_mineral_LC1 = faktor_emisi_mineral) |> 
  left_join(em_fac, join_by(LC2 ==ID), keep = F) |> 
  rename(faktor_emisi_mineral_LC2 = faktor_emisi_mineral) |> 
  select(-LC.x.x, -LC.y.y) |> 
  relocate(LC.y, .after = LC.x)

crosstab_join_peat$em_raw <- (crosstab_join_peat$faktor_emisi_LC2 + crosstab_join_peat$faktor_emisi_LC1)/2 / 6
crosstab_join_peat$em_raw[is.na(crosstab_join_peat$em_raw)] <- 0
crosstab_join_peat$emisi_peat <-  crosstab_join_peat$n * crosstab_join_peat$em_raw
sum(crosstab_join_peat$emisi_peat)

#emisi dari mineral
# crosstab_join_peat$em_raw_mineral <- (crosstab_join_peat$faktor_emisi_mineral_LC2 + crosstab_join_peat$faktor_emisi_mineral_LC1)/2 / 6
# crosstab_join_peat$em_raw_mineral[is.na(crosstab_join_peat$em_raw_mineral)] <- 0
# crosstab_join_peat$emisi_mineral <-  crosstab_join_peat$n * crosstab_join_peat$em_raw_mineral
# sum(crosstab_join_peat$emisi_mineral)


# peta karbon
landuse1_peat |>
  classify(peat_em[,-2]) -> carbon_peat1 
# landuse1_peat |> 
#   classify(em_fac[,-2]) -> carbon_mineral1
landuse2_peat |>
  classify(peat_em[,-2]) -> carbon_peat2
# landuse2_peat |> 
#   classify(em_fac[,-2]) -> carbon_mineral2
chk_em_peat<-carbon_peat1>carbon_peat2
# chk_em <- carbon_mineral1 > carbon_mineral2
# chk_sq_peat<-carbon_peat1<carbon_peat2
# chk_em <- carbon_mineral1 < carbon_mineral2
emission_peat<-((carbon_peat1-carbon_peat2)*3.67)*chk_em_peat
# sequestration_peat<-((carbon_peat2-carbon_peat1)*3.67)*chk_sq

# emission_mineral<-((carbon_mineral1-carbon_mineral2)*3.67)*chk_em
# sequestration_mineral<-((carbon_mineral2-carbon_mineral1)*3.67)*chk_sq
comb_result_peat <- c(carbon_peat1, carbon_peat2, emission_peat)
names(comb_result_peat) <- c("Carbon Peat T1", "Carbon Peat T2", "Emission Peat")
plot(comb_result_peat)

# save rasters
writeRaster(landuse1_peat, "result/carbon_peat_2016.tif")
writeRaster(landuse2_peat, "result/carbon_peat_2022.tif")
writeRaster(emission_peat, "result/emission_peat_2016-2022.tif")
write_csv(crosstab_join_peat, "result/crosstab_peat_2016-2022.csv")
