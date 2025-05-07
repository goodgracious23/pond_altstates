library(tidyverse)

#List of monitored ponds by the Hua and Wilkinson Labs
monitored = read_csv("R:/LTER Research/Alternative States/pond monitoring list.csv") %>% select(-pond)

# Data from Daniel Trovillion's thesis - downloaded from EDI
# Package ID: knb-lter-ntl.423.2 Cataloging System:https://pasta.edirepository.org.
# Data set title: Survey of pond habitats and aquatic diversity of Madison, Wisconsin from May to Aug 2019 and 2020 Data Set 
trovillion = read_csv("R:/LTER Research/Alternative States/Copy of Trovillion Data Set from EDI.csv") %>%
  select(-Time.Start:-Survey.Time, -Developed200m:-Water1000m, -SiteRound,
         -Observers, -Easting, -Northing, -HUC10.Name:-Nearest, 
         -Property.Owner, - Round, -Air.Temp:-Clouds, -Day,
         -In.OutFlow, -Canopy.Cover,
         -av.DO:-av.Chlor, -People, -Total.Rich.Native:-Zannichellia.palustris,
         -Substrate:-Notes.2019) %>%
  rename(sitecode = Site.Code,
         pond = Name,
         sampledate = Date,
         year = Year, month = Month,
         pond_surround = Pond.Type,
         latitude = Lat,
         longitdue = Long,
         area = Area,
         elevation = Elevevation,
         year_built = Pond.Made,
         pond_use = Source,
         no3 = Nitrate,
         openwater_perc = Perc.Open.Water,
         emergent_perc = Perc.Emergent,
         floating_perc = Perc.Total.Floating,
         submergent_perc = Perc.Submergent,
         substrate_perc = Perc.Substrate,
         chloride = Chloride,
         water_temp = Water.Temp,
         conductivity = Conductivity,
         spCond = Sp.Condunctivity,
         do_sat = DO.p,
         do_mgl = DO.mg, 
         salinity = Salinity,
         turbidity = Turbidity) %>%
  mutate(sampledate = mdy(sampledate)) %>%
  select(-pond_surround:-pond_use)

# Extracting the static pond characteristics from Trovillion's data set
pond_static = read_csv("R:/LTER Research/Alternative States/Copy of Trovillion Data Set from EDI.csv") %>%
  select(-Round, -SiteRound, -Date:-Observers, 
         -Air.Temp:-Zannichellia.palustris, - Elevevation) %>%
  rename(sitecode = Site.Code,
         pond = Name,
         landuse_type = Pond.Type,
         latitude = Lat,
         longitdue = Long,
         area = Area,
         year_built = Pond.Made,
         pond_source = Source,
         property_owner = Property.Owner,
         HUC10 = HUC10.Name,
         HUC12 = HUC12.Name,
         easting = Easting, northing = Northing) %>%
  distinct_all(.keep_all = FALSE)

# Data from Adrianna Gorsky's Dissertation - downloaded from EDI
# Package ID: knb-lter-ntl.433.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Greenhouse gas and water chemistry data from urban ponds in Madison, Wisconsin during the summer and under-ice period of 2021-2022.
# Data set creator:  Adrianna Gorsky - University of Wisconsin-Madison
gorsky = read_csv("R:/LTER Research/Alternative States/gorsky_urban_pond_data.csv") %>%
  select(-avsnow_cm:-whiteice_cm, -latitude:-perimeter_m, 
         -tic_mgL, -toc_mgL) %>%
  rename(sitecode = pond,
         pond = name,
         no3 = no3no2_ugL,
         chloride = cl_mgL,
         so4 = so4_mgL,
         dic = dic_mgL,
         doc = doc_mgL,
         nh4 = nh4_ugL,
         srp = srp_ugL,
         chlorophyll_extracted = chla_ugL, 
         tss = tss_gL) %>%
  mutate(month = month(sampledate),
         year = year(sampledate),
         tss = tss*1000) %>%
  mutate(openwater_perc = case_when(openwater_perc == "no survey" ~ NA,
                                    TRUE ~ openwater_perc),
         openwater_perc = as.numeric(openwater_perc),
         emergent_perc = case_when(emergent_perc == "no survey" ~ NA,
                                    TRUE ~ emergent_perc),
         emergent_perc = as.numeric(emergent_perc),
         floating_perc = case_when(floating_perc == "no survey" ~ NA,
                                   TRUE ~ floating_perc),
         floating_perc = as.numeric(floating_perc),
         submergent_perc = case_when(submergent_perc == "no survey" ~ NA,
                                   TRUE ~ submergent_perc),
         submergent_perc = as.numeric(submergent_perc),
         algae_perc = case_when(algae_perc == "no survey" ~ NA,
                                     TRUE ~ algae_perc),
         algae_perc = as.numeric(algae_perc))

# Data from Adrianna Gorsky's Dissertation - downloaded from EDI
# Package ID: knb-lter-ntl.433.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Greenhouse gas and water chemistry data from urban ponds in Madison, Wisconsin during the summer and under-ice period of 2021-2022.
# Data set creator:  Adrianna Gorsky - University of Wisconsin-Madison
gorsky_profiles = read_csv("R:/LTER Research/Alternative States/gorsky_urban_pond_tempdo.csv") %>%
  rename(sitecode = pond,
         do_sat = o2sat, 
         do_mgl = o2,
         spCond = specCond,
         water_temp = wtemp) %>%
  mutate(sampledate = mdy(sampledate)) %>%
  filter(depth == 0.5) %>% #selected 0.5m to match Trovillion's 0.4m sampling
  select(-depth) 

gorsky_combo = full_join(gorsky, gorsky_profiles, 
                         by = c('sampledate', 'sitecode'))

# Combine the Trovillion and Gorsky sampling data sets
combo_pond = full_join(trovillion, gorsky_combo) %>%
  mutate(pond = case_when(pond == "Upper Manitou" ~ "Manitou Upper Pond",
                          pond == "Lower Mannitou" ~ "Manitou Lower Pond",
                          TRUE ~ pond))

# Combine the sample data sets with the list of monitored ponds
combo_monitored = left_join(combo_pond, monitored, by = 'sitecode')

# Briggs and Community Water Monitoring Network Data
# Current chem data through January 2025
briggs = read_csv("R:/LTER Research/Alternative States/briggs and cwmn pond chemistry.csv") %>%
  select(-x254:-x555, -sampleID, -dilution, -vss_mgl) %>%
  mutate(date = mdy(date)) %>%
  rename(sampledate = date, 
         chloride = chloride_mgl,
         dic = dic_mgl,
         doc = doc_mgl,
         tn = tn_ugl, 
         tp = tp_ugl,
         tss = tss_mgl,
         nh4 = nh4_ugl,
         no3 = no23_ugl,
         srp = srp_ugl, 
         so4 = sulfate_mgl) %>%
  filter(!sampletype == "BP", !sampletype == "BW", !sampletype == "B") %>%
  filter(!pond == "F") %>%
  mutate(pond = case_when(pond == "S" ~ "Strickers Pond",
                          pond == "T" ~ "Tiedemans Pond",
                          pond == "M" ~ "McKee Farms Park Pond",
                          pond == "G" ~ "Graber Pond",
                          pond == "N" ~ "Owen Park Pond West",
                          pond == "D" ~ "Orchid South",
                          pond == "E" ~ "Elver Pond",
                          pond == "L" ~ "Lakeview Park Pond", 
                          pond == "O" ~ "Orchid North",
                          pond == "0" ~ "Orchid North",
                          pond == "K" ~ "Kettle Pond",
                          pond == "W" ~ "West Lakeview"),
         sitecode = case_when(pond == "Strickers Pond" ~ "STR",
                              pond == "Tiedemans Pond" ~ "TIE",
                              pond == "McKee Farms Park Pond" ~ "MCK",
                              pond == "Graber Pond" ~ "GRA",
                              pond == "Owen Park Pond West" ~ "OPP-W",
                              pond == "Orchid South" ~ "O-S",
                              pond == "Elver Pond" ~ "ELV",
                              pond == "Lakeview Park Pond" ~ "LKV",
                              pond == "Orchid North" ~ "O-N",
                              pond == "Kettle Pond" ~ "KP",
                              pond == "West Lakeview" ~ "WLV"))
  
briggs_profile = read_csv("R:/LTER Research/Alternative States/briggs profile data.csv") %>%
  filter(depth_m == 0.5, !pond == "Forebay") %>% 
  select(-depth_m, -time, -sampleID, -`...14`) %>%
  mutate(pond = case_when(pond == "Strickers" ~ "Strickers Pond",
                          pond == "Tiedemans" ~ "Tiedemans Pond",
                          pond == "Elver" ~ "Elver Pond",
                          pond == "Lakeview" ~ "Lakeview Park Pond",
                          pond == "McKee" ~ "McKee Farms Park Pond",
                          pond == "OrchidNorth" ~ "Orchid North",
                          pond == "LakeviewWest" ~ "West Lakeview"),
         sitecode = case_when(pond == "Strickers Pond" ~ "STR",
                              pond == "Tiedemans Pond" ~ "TIE",
                              pond == "McKee Farms Park Pond" ~ "MCK",
                              pond == "Elver Pond" ~ "ELV",
                              pond == "Lakeview Park Pond" ~ "LKV",
                              pond == "Orchid North" ~ "O-N",
                              pond == "West Lakeview" ~ "WLV")) %>%
  rename(water_temp = temp_C,
         spCond = spcond_uScm,
         chlorophyll_probe = chl_ugl, 
         phycocyanin_probe = pc_ugl) %>%
  mutate(spCond = as.numeric(spCond))

briggs_obs = read_csv("R:/LTER Research/Alternative States/briggs field observations.csv") %>%
  filter(!pond == "Forebay") %>%
  select(-sampleid, -time, -secchi_bottom, -zoopdepth_m:-notes) %>%
  mutate(secchi_m = (secchi_lower_m + secchi_raise_m)/2,
         year = case_when(year == "0:00" ~ "2022",
                          TRUE ~ year),
         year = as.numeric(year)) %>%
  select(-secchi_lower_m, -secchi_raise_m) %>%
  rename(depth_m = maxdepth_m) %>%
  mutate(pond = case_when(pond == "Strickers" ~ "Strickers Pond",
                          pond == "Stricker's" ~ "Strickers Pond",
                          pond == "Tiedemans" ~ "Tiedemans Pond",
                          pond == "Tiedeman's" ~ "Tiedemans Pond",
                          pond == "Tiedmans" ~ "Tiedemans Pond",
                          pond == "Elver" ~ "Elver Pond",
                          pond == "Lakeview" ~ "Lakeview Park Pond",
                          pond == "McKee" ~ "McKee Farms Park Pond",
                          pond == "Orchid" ~ "Orchid North",
                          pond == "LakeviewWest" ~ "West Lakeview"),
         sitecode = case_when(pond == "Strickers Pond" ~ "STR",
                              pond == "Tiedemans Pond" ~ "TIE",
                              pond == "McKee Farms Park Pond" ~ "MCK",
                              pond == "Elver Pond" ~ "ELV",
                              pond == "Lakeview Park Pond" ~ "LKV",
                              pond == "Orchid North" ~ "O-N",
                              pond == "West Lakeview" ~ "WLV"))
  
briggs_field = full_join(briggs_profile, briggs_obs)
briggs_combo = full_join(briggs, briggs_field) %>%
  select(-doy, -site, -sampletype, -pc_cyanofluor_avg, -chl_cyanofluor_avg,
         -yearfrac)

combo2_monitored = full_join(combo_monitored, briggs_combo) %>%
  mutate(monitor = case_when(is.na(monitor) ~ "Trovillion",
                             TRUE ~ monitor))

ggplot(combo2_monitored %>% 
         filter(month >= 5,
                monitor == "Hua" | monitor == "Both"), 
       aes(x = floating_perc, y = submergent_perc, color = month)) +
  geom_point(size = 2) + 
  scale_color_gradient(low = "#ff006e",
                       high = "#ffb703") +
  facet_wrap(facet = 'pond') +
  theme_bw() + xlim(0,100) + ylim(0,100) +
  geom_vline(xintercept = 50, linetype = "dashed") +
  geom_hline(yintercept = 50, linetype = "dashed") + 
  xlab("") + 
  ylab("") +
  ggtitle("Trovillion + Gorsky Data (2019-2021)") +
  labs(y = "Suberged % Cover",
       x = "Floating % Cover")
