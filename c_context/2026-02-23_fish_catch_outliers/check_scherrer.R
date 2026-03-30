rm(list = ls())

setwd("~/Desktop/Papers/Harrison_NW")

library(dplyr)
library(todry)
library(ggplot2)
library(R.matlab)

# Load spatial data and create a dummy EEZ for the open ocean (all grid cells that are not in an EEZ)
eez_cellfracs <- read.csv("./spatial/eez_cellfracs.csv")
latlon <- read.csv("./spatial/latlon_cellid.csv")

eez_cellfracs <- eez_cellfracs %>% dplyr::select(-c("Lon", "Lat", "Cell_Area_km2"))
all_spatial <- left_join(latlon, eez_cellfracs, by = c("Cell_ID" = "Cell_ID"))
all_spatial <- all_spatial %>% dplyr::mutate(EEZ_Number = ifelse(is.na(EEZ_Number), 99, EEZ_Number),
                                             EEZ_Name = ifelse(is.na(EEZ_Name), "Open Ocean", EEZ_Name),
                                             Frac_Cell = ifelse(is.na(Frac_Cell), 1, Frac_Cell))

eez_nums <- unique(all_spatial$EEZ_Number)

# Load NW outputs, MATLAB
scenarios <- c("5tg", "16tg", "27tg", "47tg", "150tg")

control_file <- list.files(path = "./zenodo_files/gridded_output/BAU", pattern = "control", full.names = TRUE)
control_data <- readMat(control_file[1])
control_harv <- control_data$Harv

# Extract EEZ level data (mean and standard deviation of total catch across ensemble in each eez)

for(i in 1:length(scenarios)){
  curr_file <- list.files(path = "./zenodo_files/gridded_output/BAU", pattern = scenarios[i], full.names = TRUE)
  
  curr_data <- readMat(curr_file[1])
  curr_harv <- curr_data$Harv
  curr_harv <- curr_harv[1:16,,,]
  
  for(j in 1:length(eez_nums)){
    print(paste(scenarios[i], round(j/length(eez_nums),2), sep = "_"))
    # Creat current eez spatial mask, oriented to match boats output
    eez_spatial <- all_spatial %>% dplyr::filter(EEZ_Number == eez_nums[j]) %>% dplyr::select("Lon", "Lat", "EEZ_Number", "EEZ_Name", "Frac_Cell")
    curr_eez <- left_join(latlon, eez_spatial, by = c("Lon" = "Lon", "Lat" = "Lat")) %>% dplyr::mutate(Frac_Area_km2 = Cell_Area_km2 * Frac_Cell)
    frac_area_grid <- matrix(as.vector(curr_eez$Frac_Area_km2)*1e6, nrow = 180, ncol = 360) # Multiply area by 1e6 to convert from km2 to m2
    frac_area_grid2 <- frac_area_grid
    frac_area_grid2[,1:180] <- frac_area_grid[,181:360]
    frac_area_grid2[,181:360] <- frac_area_grid[,1:180]
    frac_area_grid2 <- frac_area_grid2[180:1,]
    frac_area_grid <- frac_area_grid2
    
    eez_control <- sweep(control_harv, c(2,3), frac_area_grid, "*")
    eez_scen <- sweep(curr_harv, c(2,3), frac_area_grid, "*")
    eez_diff <- eez_scen - eez_control
    
    ## Control catch
    eez_control_total <- apply(eez_control, c(1,4), sum, na.rm = TRUE)
    eez_control_mean <- apply(eez_control_total, 1, mean,na.rm = TRUE)*60*60*24*365 # convert from catch per second, to catch per year
    eez_control_sd <- apply(eez_control_total, 1, sd,na.rm = TRUE)*60*60*24*365
    
    # NW scenario catch
    eez_scen_total <- apply(eez_scen, c(1,4), sum, na.rm = TRUE)
    eez_scen_mean <- apply(eez_scen_total, 1, mean,na.rm = TRUE)*60*60*24*365 
    eez_scen_sd <- apply(eez_scen_total, 1, sd,na.rm = TRUE)*60*60*24*365
    
    # Difference between NW and control (absolute and percentage)
    eez_diff_total <- apply(eez_diff, c(1,4), sum, na.rm = TRUE)
    eez_diff_mean <- apply(eez_diff_total, 1, mean,na.rm = TRUE)*60*60*24*365 
    eez_diff_sd <- apply(eez_diff_total, 1, sd,na.rm = TRUE)*60*60*24*365
    
    eez_diff_perc_mean <- apply(eez_diff_total/eez_control_total*100, 1, mean, na.rm = TRUE)
    eez_diff_perc_sd <-  apply(eez_diff_total/eez_control_total*100, 1, sd, na.rm = TRUE)
    
    # Put all together and save
    eez_dat <- data.frame("EEZ" = eez_spatial$EEZ_Name[1], 
                          "EEZ_Number" = rep(eez_spatial$EEZ_Number[1], 16*8),
                          "EEZ_Area_km2" = rep(sum(curr_eez$Frac_Area_km2, na.rm = TRUE), 16*8),
                          "Year" = rep(1:16, times = 8),
                          "Variable" = rep(c("catch_diff_mean", "catch_diff_std", 
                                         "catch_diff_perc_mean", "catch_diff_perc_std", 
                                         "catch_ctrl_mean", "catch_ctrl_std",
                                         "catch_NW_scenario_mean", "catch_NW_scenario_std"), each = 16),
                          "Value" = c(eez_diff_mean, eez_diff_sd,
                                      eez_diff_perc_mean, eez_diff_perc_sd,
                                      eez_control_mean, eez_control_sd,
                                      eez_scen_mean, eez_scen_sd))
    
    if(j == 1){
      eez_all_dat <- eez_dat
    }
    
    if(j != 1){
      eez_all_dat <- rbind(eez_all_dat, eez_dat)
    }
  }
  
  write.csv(eez_all_dat, paste("./zenodo_output/output_v2_BAU_", scenarios[i], ".csv", sep = ""))
}


## Compare 150tg output from Scherrer with above
above_5 <- read.csv("./zenodo_output/output_v2_BAU_5tg.csv")
above_erit <- above_5 %>% dplyr::filter(Year < 6) %>% group_by(EEZ, Variable) %>% dplyr::summarise(Value = mean(Value, na.rm = TRUE))
kk <- above_erit %>% dplyr::filter(Variable == "catch_diff_mean")
plot(kk$Value/1e9)
