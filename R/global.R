## ----------------------------------Init Options---------------------------------------##
options(shiny.maxRequestSize = 100 * 1024 ^ 2)

## ----------------------------------Load packages---------------------------------------##
#
get_WY <- function(x, numeric=TRUE) {
  x <- as.POSIXlt(x)
  yr <- x$year + 1900L
  mn <- x$mon + 1L
  ## adjust for water year
  yr <- yr + ifelse(mn < 10L, 0L, 1L)
  if(numeric)
    return(yr)
  ordered(yr)
}

get_WatershedArea_m2 <- function(file){
  getstring<- grep("Total contributing area to outlet ",
                   readLines(file), value = TRUE)
  getstring <- getstring[[1]]
  num <- readr::parse_number(getstring)
  num <- as.numeric(num) * 10000 ##convert ha to m2
  return(num)

}

# readObsDat <- function(fpath){
#   ObsData <- read.csv(fpath,stringsAsFactors = F)
#   colnames(ObsData) <- c("Date", "Q_mm_obs", "SSC_kg_day_obs",
#                          "TP_kg_day_obs","SRP_kg_day_obs","PP_kg_day_obs",
#                          "SSC_tonnes_day_obs", "SWE_obs_mm")
#   ObsData$Date <- as.Date(ObsData$Date, format = "%Y-%m-%d")
#   ObsData <- ObsData %>%
#     dplyr::mutate(WY = get_WY(ObsData$Date)) %>%
#     dplyr::select(Date, WY, everything())
#   return(as.data.frame(ObsData))
# }


process_chanwb <- function(chanwb_path, Wshed_Area_m2){

  ## read channel and watershed water and sediment data

  chanwb <- read.table(chanwb_path, skip = 11, header = F)

  ### set names of the dataframes

  colnames(chanwb) <- c("Year_chan", "Day_chan", "Elmt_ID_chan",
                        "Chan_ID_chan", "Inflow_chan", "Outflow_chan",
                        "Storage_chan", "Baseflow_chan", "Loss_chan",
                        "Balance_chan")



  chanwb <- chanwb %>% dplyr::mutate(Q_outlet_mm = (Outflow_chan/ Wshed_Area_m2 *1000),
                                     originDate = as.Date(paste0(Year_chan, "-01-01"),tz = "UTC") - lubridate::days(1),
                                     Date = as.Date(Day_chan, origin = originDate, tz = "UTC"),
                                     WY = get_WY(Date)) %>% dplyr::select(-originDate) %>%
    dplyr::select(Year_chan, Day_chan, Date, WY, everything())



  return(as.data.frame(chanwb))

}


process_ebe <- function(ebe_path, SimStartDate, SimEndDate ){

  ## read channel and watershed water and sediment data

  ebe <- utils::read.table(ebe_path, skip = 9, header = F)

  ### set names of the dataframe

  colnames(ebe) <- c("Day_ebe", "Month_ebe", "Year_ebe",
                     "P_ebe", "Runoff_ebe", "peak_ebe", "Sediment_ebe",
                     "SRP_ebe", "PP_ebe", "TP_ebe")

  ## calcs
  ebe <- ebe %>% dplyr::mutate(Date = seq(from = as.Date(SimStartDate),
                                          to = as.Date(SimEndDate), by = 1),
                               WY = get_WY(Date),
                               Sediment_tonnes_ebe = Sediment_ebe/1000,
                               SRP_tonnes_ebe = SRP_ebe/1000,
                               PP_tonnes_ebe = PP_ebe/1000,
                               TP_tonnes_ebe = TP_ebe/1000) %>%
    dplyr::select(Day_ebe, Month_ebe, Year_ebe, Date, WY, everything())



  return(as.data.frame(ebe))

}

calc_watbal <- function(link){
  a <- read.table(link, skip = 23,
                  col.names = c("OFE",	"J",	"Y",	"P",	"RM",	"Q",	"Ep",	"Es",
                                "Er",	"Dp",	"UpStrmQ",	"SubRIn",	"latqcc",
                                "Total_Soil_Water",	"frozwt",	"Snow_Water",	"QOFE",
                                "Tile",	"Irr",	"Area")) %>%
    dplyr::mutate_if(is.character,as.numeric)


  a <- a %>%  dplyr::mutate(wb = P-Q-Ep - Es- Er - Dp - latqcc +
                              lag(Total_Soil_Water) - Total_Soil_Water +
                              lag(frozwt) - frozwt+ lag(Snow_Water) - Snow_Water) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, 3)) %>% dplyr::select(wb) %>%
    dplyr::summarise_all(.funs = sum, na.rm = TRUE) %>%
    dplyr::mutate(WeppID =readr::parse_number(gsub("^.*/", "", link)))

  return(as.data.frame(a))
}

merge_daily_Sim_obs_Vars <- function(totalwatsed_df, chanwb_df, ebe_df){
  daily<- dplyr::left_join(as.data.frame(totalwatsed_df), as.data.frame(chanwb_df), by = c("Date", "WY")) %>%
    dplyr::left_join(as.data.frame(ebe_df),  by = c("Date", "WY")) %>%
    dplyr::mutate_at(c("area_m_2",	"precip_vol_m_3",	"rain_melt_vol_m_3",	"transpiration_vol_m_3",
                "evaporation_vol_m_3",	"percolation_vol_m_3",	"runoff_vol_m_3",	"lateral_flow_vol_m_3",
                "storage_vol_m_3",	"sed_det_kg",	"sed_dep_kg",	"sed_del_kg",
                "class_1",	"class_2",	"class_3",	"class_4",	"class_5",
                "area_ha",	"cumulative_sed_del_tonnes",	"sed_del_density_tonne_ha",
                "precipitation_mm",	"rain_melt_mm",	"transpiration_mm",	"evaporation_mm",	"et_mm",
                "percolation_mm",	"runoff_mm",	"lateral_flow_mm",	"storage_mm",
                "reservoir_volume_mm",	"baseflow_mm",	"aquifer_losses_mm",
                "streamflow_mm",	"swe_mm",	"sed_del_tonne",	"p_load_mg",
                "p_runoff_mg",	"p_lateral_mg",	"p_baseflow_mg",	"total_p_kg",
                "particulate_p_kg",	"soluble_reactive_p_kg",	"p_total_kg_ha",	"particulate_p_kg_ha",
                "soluble_reactive_p_kg_ha",	"Elmt_ID_chan",	"Chan_ID_chan",	"Inflow_chan",	"Outflow_chan",
                "Storage_chan",	"Baseflow_chan",	"Loss_chan",	"Balance_chan",
                "Q_outlet_mm",	"Day_ebe",	"P_ebe",	"Runoff_ebe",	"peak_ebe",
                "Sediment_ebe",	"SRP_ebe",	"PP_ebe",	"TP_ebe",
                "Sediment_tonnes_ebe",	"SRP_tonnes_ebe",	"PP_tonnes_ebe",
                "TP_tonnes_ebe"),as.numeric)
  return(daily)
}



mean_by_WY <- function(dailydf){
  WY_mean_df <- dailydf %>%
    dplyr::select(-julian, -year, -Date, -Year_chan, -Day_chan,
                  -Chan_ID_chan, -Elmt_ID_chan) %>%
    dplyr::group_by(WY) %>%
    dplyr::summarise_all(.funs = mean) %>% dplyr::ungroup()
  return(as.data.frame(WY_mean_df))
}


get_geometry <- function(link){
  link <- paste0(link,"browse/export/arcmap/subcatchments.json")
  geometry <- sf::st_read(link)%>%
    dplyr::select(WeppID, geometry) %>%
    sf::st_transform(4326) %>%
    dplyr::group_by(WeppID)%>%
    dplyr::summarize(geometry = sf::st_union(geometry))

  # %>%
    # sf::st_cast("POLYGON") %>%
    # as("Spatial")
  return(geometry)

}



gethillwatfiles<- function(link){
  link <- paste0(link,"browse/wepp/output/")
  pg <- rvest::read_html(link)
  wat_dat <- rvest::html_attr(rvest::html_nodes(pg, "a"), "href") %>%
    stringr::str_subset(".wat.dat", negate = FALSE)
  wat_dat <- paste0(link, wat_dat)
  return(wat_dat)

}


# nPairs_function <- function(obs, pred){
#   length(pred[which(!is.na(obs) & !is.na(pred))])
# }
#
# DV_function <- function (Qobs, Qsim) {
#   # original data:
#   Qobs_ori <-Qobs
#   Qsim_ori <-Qsim
#   # throw away missing values (both obs and sim must have paired values)
#   Qsim <- Qsim_ori[!is.na(Qobs_ori) & !is.na(Qsim_ori)]
#   Qobs <- Qobs_ori[!is.na(Qobs_ori) & !is.na(Qsim_ori)]
#   if (length(Qobs) == 0 || length(Qsim) == 0)
#     return(NA)
#   DV <-((sum(Qobs)- sum(Qsim))/sum(Qobs))*100
#   return(DV)
# }
#
# NSeff_function <- function (Qobs, Qsim) {
#   # original data:
#   Qobs_ori <-Qobs
#   Qsim_ori <-Qsim
#   # throw away missing values (both obs and sim must have paired values)
#   Qsim <- Qsim_ori[!is.na(Qobs_ori) & !is.na(Qsim_ori)]
#   Qobs <- Qobs_ori[!is.na(Qobs_ori) & !is.na(Qsim_ori)]
#   if (length(Qobs) == 0 || length(Qsim) == 0)
#     return(NA)
#   NS <- 1 - (sum((Qobs-Qsim)^2)/sum((Qobs-mean(Qobs))^2))
#   return(NS)
# }
#
# R2_function <- function(obs, pred){
#   summary(lm(obs ~ pred))$r.squared}
#
# R2ad_function <- function(obs, pred){
#   summary(lm(obs ~ pred))$adj.r.squared}
#
#
# rmse_function <- function(obs, pred){
#   sqrt(mean((obs-pred)^2 ,na.rm=T))}


grph_file_header_names<-c("Days_In_Simulation",
                "Precipitation_in",
                "Average_detachment_tons_A",
                "Maximum_detachment_tons_A",
                "Point_of_maximum_detachment_ft",
                "Average_deposition_tons_A",
                "Maximum_deposition_tons_A",
                "Point_of_maximum_deposition_ft",
                "Sediment_Leaving_Profile_lbs_ft",
                "5_day_average_mimimum_temp_F",
                "5_day_average_maximum_temp_F",
                "daily_mimimum_temp_F",
                "daily_maximum_temp_F",
                "Irrigation_depth_in",
                "Irrigation_volume_supplied_unit_area_in",
                "Runoff_in",
                "Interrill_net_soil_loss_tons_A",
                "Canopy_height_ft",
                "Canopy_cover",
                "Leaf_area_index",
                "Interrill_cover",
                "Rill_cover",
                "Above_ground_live_biomass_tons_A",
                "Live_root_mass_for_OFE_tons_A",
                "Live_root_mass_0_15cm_depth_tons_A",
                "Live_root_mass_15_30_cm_depth_tons_A",
                "Live_root_mass_30_60_cm_depth_tons_A",
                "Root_depth_in",
                "Standing_dead_biomass_tons_A",
                "Current_residue_mass_on_ground_tons_A",
                "Previous_residue_mass_on_ground_tons_A",
                "Old_residue_mass_on_the_ground_tons_A",
                "Current_submerged_residue_mass_tons_A",
                "Previous_submerged_residue_mass_tons_A",
                "Old_submerged_residue_mass_tons_A",
                "Current_dead_root_mass_tons_A",
                "Previous_dead_root_mass_tons_A",
                "Old_dead_root_mass_tons_A",
                "Porosity",
                "Bulk_density_lbs_ft3",
                "Effective_hydraulic_conductivity_in_hr",
                "Suction_across_wetting_front_in",
                "Evapotranspiration_in",
                "Drainage_flux_in_day",
                "Depth_to_drainable_zone_ft",
                "Effective_intensity_in_h",
                "Peak_runoff_in_h",
                "Effective_runoff_duration_h",
                "Enrichment_ratio",
                "Adjusted_Ki_millions_lb_s_ft4",
                "Adjusted_Kr_x_1000_s_ft",
                "Adjusted_Tauc_lbs_ft2",
                "Rill_width_in",
                "Plant_Transpiration_in",
                "Soil_Evaporation_in",
                "Seepage_in",
                "Water_stress",
                "Temperature_stress",
                "Total_soil_water_in",
                "Soil_water_in_layer_1_in",
                "Soil_water_in_layer_2_in",
                "Soil_water_in_layer_3_in",
                "Soil_water_in_layer_4_in",
                "Soil_water_in_layer_5_in",
                "Soil_water_in_layer_6_in",
                "Soil_water_in_layer_7_in",
                "Soil_water_in_layer_8_in",
                "Soil_water_in_layer_9_in",
                "Soil_water_in_layer_10_in",
                "Random_roughness_in",
                "Ridge_height_in",
                "Frost_depth_in",
                "Thaw_depth_in",
                "Snow_depth_in",
                "Water_from_snow_melt_in",
                "Snow_density_lbs_ft3",
                "Rill_cover_fric_fac_crop",
                'Fric_fac_due_to_live_plant',
                "Rill_total_fric_fac_crop",
                "Composite_area_total_friction_factor",
                "Rill_cov_fric_fac",
                'Live_basal_area_fric_fac',
                "Live_plant_canopy_fric_fac",
                "Days_since_last_disturbance",
                "Current_crop_type",
                "Current_residue_on_ground_type",
                "Previous_residue_on_ground_type",
                "Old_residue_on_ground_type",
                "Current_dead_root_type",
                "Previous_dead_root_type",
                "Old_dead_root_type",
                "Sediment_leaving_OFE_lbs_ft",
                "Evaporation_from_residue_in",
                "Total_frozen_soil_water_in",
                "Frozen_soil_water_in_layer_1_in",
                "Frozen_soil_water_in_layer_2_in",
                "Frozen_soil_water_in_layer_3_in",
                "Frozen_soil_water_in_layer_4_in",
                "Frozen_soil_water_in_layer_5_in",
                "Frozen_soil_water_in_layer_6_in",
                "Frozen_soil_water_in_layer_7_in",
                "Frozen_soil_water_in_layer_8_in",
                "Frozen_soil_water_in_layer_9_in",
                "Frozen_soil_water_in_layer_10_in")


totwatsed_names <- c("Julian",	"Year",	"Area (m^2)",
                     "Precip Vol (m^3)",	"Rain + Melt Vol (m^3)",
                     "Transpiration Vol (m^3)",	"Evaporation Vol (m^3)",
                     "Percolation Vol (m^3)",	"Runoff Vol (m^3)",
                     "Lateral Flow Vol (m^3)",	"Storage Vol (m^3)",
                     "Sed. Det. (kg)",	"Sed. Dep. (kg)",
                     "Sed. Del (kg)",	"Class 1",	"Class 2",	"Class 3",
                     "Class 4",	"Class 5")
