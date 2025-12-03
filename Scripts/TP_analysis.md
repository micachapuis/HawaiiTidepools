TP_analysis
================
Micaela Chapuis
2025-06-30

## Load Libraries

``` r
library(tidyverse)
library(here)
library(seacarb)
library(lubridate)
library(hms)
library(car)
library(lubridateExtras)
library(lme4)
library(scales)
library(ggeffects)
library(sjPlot)
library(broom)
```

## Load in Data

``` r
chem <- read_csv(here("Data", "tidepool_chemistry.csv"))
params <- read_csv(here("Data", "tidepool_parameters.csv"))
pHSlope <- read_csv(here("Data", "pHSlope.csv"))
TA <- read_csv(here("Data", "total_alkalinity.csv"))
testdata <- read_csv(here("Test Sampling", "Data", "testdata.csv"))
pool_volumes <- read_csv(here("Data", "pool_volumes.csv"))
fdom <- read_csv(here("Data", "fDOM_indices.csv"))

community_comp <- read_csv(here("Data", "comm_comp_summary.csv"))

kb_wind <- read_csv(here("Data", "Climate", "KB_0551_20250101_20251108.csv"))
sb_wind <- read_csv(here("Data", "Climate", "SB_0531_20250101_20251108.csv"))
```

## Wind data

``` r
# apprend sb data to kb data +  select only the time, site and wind speed columns
wind <- bind_rows(kb_wind, sb_wind) %>% select("Timestamp", "Station ID", "Wind speed, scalar average (m/s)")
```

``` r
# make date, time and site their own columns
wind <- wind %>% mutate(date = date(wind$Timestamp), 
                        time = as_hms(wind$Timestamp), 
                        site = case_when(`Station ID` == "0531" ~ "Sandy Beach", 
                                         `Station ID` == "0551" ~ "Kaihalulu Beach")) %>% 
                 rename(wind_speed = `Wind speed, scalar average (m/s)`)
```

Function to Calculate FCO2 (Fugosity of CO2)

``` r
# Function for calculating FCO2, code by Nyssa (FCo2Calc in her code)
FCO2_Calc <- function(temp = 25, sal = 35, u = 0, CO2_water, CO2_air = 427, density = 1025){
  # calculate gas transfer velocity from Ho et al. 2006 parameterization
  k <- (0.266*u^2)/100 # m/hr
  
  # calculate solubility of CO2 from temp and salinity from seacarb (Weiss et al. 1974)
  s <- as.numeric(K0(S = sal, T = temp, P = 0, Patm = 1, warn = T))/1000  # mmol kg-1 uatm-1
  
  # calculate FCO2
  FOC2 <- (k * s * density * (CO2_water - CO2_air)) # in mmol m-2 hr-1
  
  return(FOC2)
}

# FCO2 = (k * s * density*(CO2 water - CO2 air))
# k is the gas transfer velocity (m/hr) --> calculated based on wind measurements using parameterization by Ho et al. 2006
# s is solubility of CO2 in seawater, calculated from in situ temperature and salinity
# CO2 in seawater is calculated above using seacarb
# CO2 in air assumed to be 427 ppm based on Mauna Loa Observatory estimates
```

## Data cleaning and joining

Make all pool numbers factors

``` r
chem$pool_number <- factor(chem$pool_number, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32, "ocean"))
params$pool_number <- factor(params$pool_number)
pool_volumes$pool_number <- factor(pool_volumes$pool_number)
```

Remove junk samples from TA Data

``` r
junks <- c("junk1", "junk2", "junk3")
TA <- TA %>% filter(!sample_ID %in% junks)
```

Join in TA data

``` r
data <- left_join(chem, TA, by = "sample_ID")
```

Add in pH calculated from tris, physical parameters, and pool volumes

``` r
data <- left_join(data, pHSlope)
```

    ## Joining with `by = join_by(sample_ID, date)`

``` r
data <- left_join(data, (params %>% select(site, pool_number, substrate, perimeter_m, surface_area_m2)), by = c("site", "pool_number"))
data <- left_join(data, pool_volumes)
```

    ## Joining with `by = join_by(pool_number)`

Selecting columns

``` r
data <- data %>% select(-observers, -notes, -Sample.Index)
```

Adding in “ocean” as substrate

``` r
data <- data %>% mutate(substrate = replace_na(substrate, "ocean"))
```

Removing Kaihalulu Data collected on 2025/06/08 due to rain

``` r
data <- data %>% filter(!date == "2025/06/08")
```

Join Community Comp Summary Data

``` r
# make pool number a factor in the community comp data first
community_comp$pool_number <- factor(community_comp$pool_number)

# then join
data <- left_join(data, community_comp, by = c("pool_number", "date", "site", "substrate"))
```

### fDOM Cleaning

``` r
fdom <- fdom %>% select(!(c("sample", "run_name", "UniqueID"))) %>%
  mutate(m_to_c = CobleM/CobleC) %>% # looks like it was calculated backwards
  rename("ultra_violet_humic_like" = "CobleA",
         "visible_humic_like" = "CobleC",
         "marine_humic_like" = "CobleM",
         "tryptophan_like" = "CobleT",
         "tyrosine_like" = "CobleB",
         "phenylalanine_like" = "Fpeak") %>%
  mutate(humics = ultra_violet_humic_like + visible_humic_like + marine_humic_like, # sum of all humic-like
         prot = tryptophan_like + tyrosine_like + phenylalanine_like) # sum of all protienaceous-like
```

``` r
data <- left_join(data, fdom)
```

    ## Joining with `by = join_by(sample_ID)`

Append test data

``` r
data <- bind_rows(data, testdata)
```

    ## New names:
    ## • `...1` -> `...30`

Calculate DIC using seacarb

``` r
CO2 <- carb(flag = 8, # pH and ALK given
            data$pH, # first variable given
            data$TA/1000000, # value of second variable in mol/kg
            S = data$salinity_field, #which salinity should I use?? using field for consistency
            T = data$temp_pool, # temp
            Patm = 1, # surface atmo pressure in atm, default is 1 atm
            P = 0, # hydrostatic pressure in bar (surface = 0)
            Pt = 0, # concentration of total phosphate in mol/kg --> use 0 if NA
            Sit = 0, # concentration of total silicate in mol/kg --> use 0 if NA
            k1k2 = "x", # x is default
            kf = "x", # x is default
            ks = "d", # d is for Ks from Dickson 1990 (default)
            pHscale = "T", # total scale
            b = "u74", # total boron concentration, u74 is default
            gas = "potential",  # default (indicates it's references to 1 atm pressure and potential temp)
            warn = "y", # show warnings when T or S go beyond valid range for constants
            eos = "eos80") # to specify T and S
```

Convert units back to micromol

``` r
CO2 <- CO2 %>%
  mutate(ALK = ALK*1000000,
         CO2 = CO2*1000000,
         CO3 = CO3*1000000,
         DIC = DIC*1000000,
         HCO3 = HCO3*1000000) %>% # convert everything back to umol %>%
select(pH, DIC, pCO2 = pCO2insitu, CO2, CO3, HCO3, OmegaCalcite, OmegaAragonite) 
```

Re-Join with rest of data using pH as join column

``` r
data <- left_join(data, CO2)
```

    ## Joining with `by = join_by(pH)`

Get Mean Ocean Salinity Value for normalizing TA + DIC

``` r
ocn_data <- data %>% filter(substrate %in% "ocean")
avg_ocn_sal <- mean(ocn_data$salinity_pool, na.rm = TRUE)
```

Salinity Normalize TA + DIC and create TA/DIC cols

``` r
data <- data %>% mutate(TA_norm = TA*salinity_lab/avg_ocn_sal,
                        DIC_norm = DIC*salinity_lab/avg_ocn_sal,
                        TA_DIC = TA/DIC)
```

Subtract Volume Removed and convert to m^3

``` r
data <- data %>% mutate(pool_volume = (mean_volume - vol_removed_l)*0.001) # from the calculated pool volume, subtract the water that was removed each sampling point (400ml at each timepoint) to get actual volume at sampling time
```

Old Calculate Deltas Calculate delta pH, delta TA, delta DIC, delta
temp, delta time

``` r
#delta_calc <- data %>% 
 # select(date, site, pool_number, substrate, time_point, temp_pool, sample_time, pH, TA_norm, DIC_norm, dissolved_oxygen)  %>%
  #group_by(date, site, pool_number, substrate) %>%  # group by metadata
  #arrange(time_point, .by_group = TRUE) %>%  # ensure correct order
  #reframe(
  #  delta_pH = pH[time_point == 3] - pH[time_point == 1],   # (Time 3 - Time 1)
    
  #  delta_TA = (TA_norm[time_point == 3] - TA_norm[time_point == 1]),  # (Time 3 - Time 1)
    
  #  delta_DIC = (DIC_norm[time_point == 3] - DIC_norm[time_point == 1]), # (Time 3 - Time 1)

  #  delta_temp = (temp_pool[time_point == 3] - temp_pool[time_point == 1]),  # (Time 3 - Time 1)

  #  delta_time = (as.numeric(difftime(sample_time[time_point == 3], sample_time[time_point == 1], units = "mins"))),  # Time 3 - Time 1
    
  #  delta_oxygen = (dissolved_oxygen[time_point == 3] - dissolved_oxygen[time_point == 1])
    
#  ) %>% 
#    distinct(date, site, pool_number, substrate, .keep_all = TRUE)  # Ensure only one row per pool
```

Normalize delta pH, TA, and DIC to by hour

``` r
#delta_calc <- delta_calc %>% mutate(delta_pH_norm = (delta_pH/delta_time)*60,
#                                    delta_TA_norm = (delta_TA/delta_time)*60,
#                                    delta_DIC_norm = (delta_DIC/delta_time)*60,
#                                    delta_O2_norm = (delta_oxygen/delta_time)*60)
```

### Wind Speed

Average Wind Speed Over Sample Period

``` r
# for each site, get the first and last sample time
sample_times <- data %>%
  group_by(site, date) %>%
  summarise(
    start_time = min(sample_time, na.rm = TRUE),
    end_time   = max(sample_time, na.rm = TRUE),
    .groups = "drop")
```

``` r
# for each site and date, calculate the average wind speed (in m/s) during the sample time
mean_wind <- wind %>%
  inner_join(sample_times, by = c("site", "date")) %>%
  filter(time >= start_time, time <= end_time) %>% # filter to only keep wind speeds during the time of sampling
  group_by(site, date) %>%
  summarise(mean_wind_speed = mean(wind_speed, na.rm = TRUE), .groups = "drop")
```

``` r
# join mean wind speeds to tidepool data
data <- data %>% left_join(mean_wind, by = c("site", "date"))
```

Calculate FCO2 from the wind data

``` r
data <- data %>% mutate(FCO2 = FCO2_Calc(temp = temp_pool, sal = salinity_pool, u = mean_wind_speed, CO2_water = pCO2))
```

### NEC

Calculate Delta TA and Delta DIC between two consecutive timepoints

``` r
#deltas <- data %>% 
#  filter(pool_number %in% c("ocean", 27, 30)) %>%
#  select(date, site, pool_number, substrate, day_night, surface_area_m2, pool_volume, time_point, temp_pool, sample_time, pH, TA_norm, DIC_norm)  %>%

#  group_by(date, site, pool_number, substrate, day_night, surface_area_m2) %>%  # group by metadata
  
#  arrange(time_point, .by_group = TRUE) %>%  # ensure correct order
  
#  reframe(
#    delta_TA1 = (TA_norm[time_point == 2] - TA_norm[time_point == 1]),  # (Time 1 - Time 2)
#    delta_TA2 = (TA_norm[time_point == 3] - TA_norm[time_point == 2]),
    
#    delta_DIC1 = (DIC_norm[time_point == 2] - DIC_norm[time_point == 1]), # (Time 2 - Time 1)
#   delta_DIC2 = (DIC_norm[time_point == 3] - DIC_norm[time_point == 2]),

#    delta_temp1 = (temp_pool[time_point == 2] - temp_pool[time_point == 1]),  # (Time 2 - Time 1)
#    delta_temp2 = (temp_pool[time_point == 3] - temp_pool[time_point == 2]),  

#    delta_time1 = (as.numeric(difftime(sample_time[time_point == 2], sample_time[time_point == 1], units = "hours"))),  # Time 2 - Time 1
#    delta_time2 = (as.numeric(difftime(sample_time[time_point == 3], sample_time[time_point == 2], units = "hours"))),
    
#    vol_avg1 = mean(c(pool_volume[time_point == 2], pool_volume[time_point ==1]), na.rm = TRUE), 
#    vol_avg2 = mean(c(pool_volume[time_point == 3], pool_volume[time_point ==2]), na.rm = TRUE)
    
#  ) %>% 
#    distinct(date, site, pool_number, substrate, day_night, surface_area_m2, .keep_all = TRUE) %>%  # Ensure only one row per pool per date
  
#    pivot_longer(
#      cols = matches("delta_|vol_avg"), # finds every column that stars with either "delta_" or "vol_avg"
#      names_to = c(".value", "interval"), # interval will be 1 or 2 (1 is from T1 to T2 and 2 is from T2 to T3), .value takes the name of whichever value we get the delta for
#      names_pattern = "(.*?)([12])$") # this renames all the columns to remove the 1s or 2s
```

### Calculate Deltas

``` r
deltas <- data %>%
  
  select(date, site, pool_number, substrate, surface_area_m2, mean_wind_speed, pool_volume, time_point, sample_time, temp_pool, pH, TA_norm, DIC_norm, FCO2, dissolved_oxygen, OmegaAragonite) %>%
  
  group_by(date, site, pool_number, substrate, surface_area_m2) %>%
  
  arrange(time_point, .by_group = TRUE) %>%
  
  pivot_wider(
    names_from = time_point,
    values_from = c(pH, TA_norm, DIC_norm, temp_pool, sample_time, pool_volume, FCO2, dissolved_oxygen, OmegaAragonite),
    names_glue = "{.value}{time_point}") %>%
  
  mutate(
    delta_pH1 = pH2 - pH1, # change in pH between two sampling points
    delta_pH2 = pH3 - pH2,
    mean_pH1 = mean(pH2, pH1, na.rm = TRUE), 
    mean_pH2 = mean(pH3, pH2, na.rm = TRUE),
    delta_TA1 = TA_norm2 - TA_norm1, # change in TA between two sampling points
    delta_TA2 = TA_norm3 - TA_norm2,
    delta_DIC1 = DIC_norm2 - DIC_norm1, # change in DIC between two sampling points
    delta_DIC2 = DIC_norm3 - DIC_norm2,
    delta_temp1 = temp_pool2 - temp_pool1, # change in temperature between two sampling points 
    delta_temp2 = temp_pool3 - temp_pool2,
    delta_oxygen1 = dissolved_oxygen2 - dissolved_oxygen1,
    delta_oxygen2 = dissolved_oxygen3 - dissolved_oxygen2,
    delta_time1 = as.numeric(difftime(sample_time2, sample_time1, units = "hours")), # time between two sampling points
    delta_time2 = as.numeric(difftime(sample_time3, sample_time2, units = "hours")),
    delta_aragonite1 = OmegaAragonite2 - OmegaAragonite1,
    delta_aragonite2 = OmegaAragonite3 - OmegaAragonite2,
    mean_FCO21 = mean(FCO22, FCO21, na.rm = TRUE), # calculate the average FCO2 between two sampling points
    mean_FCO22 = mean(FCO23, FCO22, na.rm = TRUE),
    mean_vol1 = rowMeans(cbind(pool_volume1, pool_volume2), na.rm = TRUE), # average volume between two sampling points
    mean_vol2 = rowMeans(cbind(pool_volume2, pool_volume3), na.rm = TRUE)) %>%
  
   pivot_longer(
      cols = matches("delta_|mean_"), # finds every column that stars with either "delta_" or "vol_avg"
      names_to = c(".value", "interval"), # interval will be 1 or 2 (1 is from T1 to T2 and 2 is from T2 to T3), .value takes the name of whichever value we get the delta for
      names_pattern = "(.*?)([12])$") %>%  # this renames all the columns to remove the 1s or 2s
  select(!(pH1:OmegaAragonite3)) %>%
  filter(!is.na(interval))
```

Normalize delta pH, TA, and DIC to by hour

``` r
deltas <- deltas %>% mutate(delta_pH_norm = (delta_pH/delta_time),
                            delta_TA_norm = (delta_TA/delta_time),
                            delta_DIC_norm = (delta_DIC/delta_time),
                            delta_O2_norm = (delta_oxygen/delta_time))
```

Calculate NEC

``` r
deltas <- deltas %>% 
  mutate(NEC = ((-1)*(delta_TA/2)*1025*(mean_vol/surface_area_m2)*(1/delta_time))/100)

# NEC = (-1) * (delta TA/2 * density * volume/surface area) * (1 / delta time) / 100

# delta TA/2 is difference in TA betwen two consecutive timepoints (in mmol/kg)
# density of seawater is 1023 kg/m3
# volume is volume of water in the pool at each time point (m3) (avg vol between the two timepoints)
# SA is bottom surface area of the tidepool (m2)
# t is time between sampling points (hr)
# divide that all by 100 to get mmols m2 hr and multiply by -1 so positive values show calcification
```

### NEP

Calculate NEP

``` r
deltas <- deltas %>% 
  mutate(NEP = (((-delta_DIC)*1025*(mean_vol/surface_area_m2)*(1/delta_time))/100) - NEC + mean_FCO2)

# NEP = ((delta DIC * density * volume/surface area) * (1 / delta time)) - NEC - FCO2

# delta DIC is the difference in salinity normalized DIC between consecutive timepoints (mmol/kg)
# using - delta DIC so positive values are net photosynthesis
# all other values are same as above
# NEC is subtracted to account for changes in DIC by the precipitation and dissolution of CaCO3
# FCO2 is subtracted to account for the air-sea flux of CO2 and is in mmol/m2 hr
# divided by 100 before subtracting NEC and FCO2 so mmols m2 hr  
# positive values are net photosynthesis 
#(the equation is - fugosity, but because it is time 2-1 it is plus here)
```

## Data Viz

``` r
# checking field salinity (with Orion) to lab salinity (with Orion) measurements
plot(data$salinity_pool, data$salinity_lab)
```

![](TP_analysis_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

### Pool Temperature

``` r
data %>% filter(site %in% "Kaihalulu Beach") %>% ggplot(aes(x = sample_time, y = temp_pool, color = pool_number)) +
  facet_wrap(~site, scales = "free_x") + geom_line(linewidth = 0.8) + geom_point() + theme_minimal() + labs(title = "Pool Temperature") + theme(axis.text.x = element_text(angle = 30))
```

![](TP_analysis_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

``` r
data %>% filter(site %in% "Sandy Beach") %>% ggplot(aes(x = sample_time, y = temp_pool, color = pool_number)) +
  facet_wrap(~site, scales = "free_x") + geom_line(linewidth = 0.8) + geom_point() + theme_minimal() + labs(title = "Pool Temperature") + theme(axis.text.x = element_text(angle = 30))
```

![](TP_analysis_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

``` r
data %>% ggplot(aes(x = sample_time, y = temp_pool)) +
    facet_wrap(~substrate) + geom_smooth() + geom_point() + theme_minimal() + labs(title = "Pool Temp")  + theme(axis.text.x = element_text(angle = 30))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](TP_analysis_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

``` r
data %>% ggplot(aes(x = sample_time, y = temp_pool, color = substrate)) +
    facet_wrap(~substrate, 
               labeller = as_labeller(c(basalt = "Basalt", limestone = "Limestone", ocean = "Ocean"))) + 
  geom_smooth(method = "gam") + #, se=FALSE) + 
  geom_point() + theme_bw() +
  labs(x = "Sample Time", y = "Temperature (°C)")  + 
  guides(color = "none") +
  theme(axis.text.x = element_text(angle = 25)) 
```

    ## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'

![](TP_analysis_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

### Pool Salinity

``` r
data %>% filter(site %in% "Kaihalulu Beach") %>% ggplot(aes(x = sample_time, y = salinity_pool, color = pool_number)) +   facet_wrap(~site, scales = "free_x") + geom_line(linewidth = 0.8) + geom_point() + theme_minimal() + labs(title = "Salinity (pool)")  + theme(axis.text.x = element_text(angle = 30))
```

![](TP_analysis_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

``` r
data %>% filter(site %in% "Sandy Beach") %>% ggplot(aes(x = sample_time, y = salinity_pool, color = pool_number)) +   facet_wrap(~site, scales = "free_x") + geom_line(linewidth = 0.8) + geom_point() + theme_minimal() + labs(title = "Salinity (pool)")  + theme(axis.text.x = element_text(angle = 30))
```

    ## Warning: Removed 55 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

    ## Warning: Removed 66 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

``` r
data %>% ggplot(aes(x = sample_time, y = salinity_field)) +
    facet_wrap(~substrate) + geom_smooth() + geom_point() + theme_minimal() + labs(title = "Salinity (Field)")  + theme(axis.text.x = element_text(angle = 30))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](TP_analysis_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

``` r
data %>% ggplot(aes(x = sample_time, y = salinity_field, color = substrate)) +
    facet_wrap(~substrate, 
               labeller = as_labeller(c(basalt = "Basalt", limestone = "Limestone", ocean = "Ocean"))) + 
  geom_smooth(method = "gam")+ #, se=FALSE) + 
  geom_point() + theme_bw() +
  labs(x = "Sample Time", y = "Salinity (psu)")  + 
  guides(color = "none") +
  theme(axis.text.x = element_text(angle = 25)) 
```

    ## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'

![](TP_analysis_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

### pH

``` r
data %>% filter(site %in% "Kaihalulu Beach") %>% ggplot(aes(x = sample_time, y = pH, color = pool_number)) +
    facet_wrap(~site, scales = "free_x") + geom_line(linewidth = 0.8) + geom_point() + theme_minimal() + labs(title = "pH (tris)")  + theme(axis.text.x = element_text(angle = 30))
```

![](TP_analysis_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

``` r
data %>% filter(site %in% "Sandy Beach") %>% ggplot(aes(x = sample_time, y = pH, color = pool_number)) +
    facet_wrap(~site, scales = "free_x") + geom_line(linewidth = 0.8) + geom_point() + theme_minimal() + labs(title = "pH (tris)")  + theme(axis.text.x = element_text(angle = 30))
```

![](TP_analysis_files/figure-gfm/unnamed-chunk-44-1.png)<!-- -->

``` r
data %>% filter(site %in% "Kaihalulu Beach") %>% ggplot(aes(x = sample_time, y = pH_probe, color = pool_number)) +
    facet_wrap(~site, scales = "free_x") + geom_line(linewidth = 0.8) + geom_point() + theme_minimal() + labs(title = "pH (probe)")  + theme(axis.text.x = element_text(angle = 30))
```

![](TP_analysis_files/figure-gfm/unnamed-chunk-45-1.png)<!-- -->

``` r
data %>% filter(site %in% "Sandy Beach") %>% ggplot(aes(x = sample_time, y = pH_probe, color = pool_number)) +
    facet_wrap(~site, scales = "free_x") + geom_line(linewidth = 0.8) + geom_point() + theme_minimal() + labs(title = "pH (probe)")  + theme(axis.text.x = element_text(angle = 30))
```

![](TP_analysis_files/figure-gfm/unnamed-chunk-46-1.png)<!-- -->

``` r
data %>% ggplot(aes(x = sample_time, y = pH)) +
    facet_wrap(~substrate) + geom_smooth() + geom_point() + theme_minimal() + labs(title = "pH")  + theme(axis.text.x = element_text(angle = 30))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](TP_analysis_files/figure-gfm/unnamed-chunk-47-1.png)<!-- -->

``` r
data %>% ggplot(aes(x = sample_time, y = pH, color = site)) +
    facet_wrap(~substrate) + geom_smooth() + geom_point() + theme_minimal() + labs(title = "pH")  + theme(axis.text.x = element_text(angle = 30))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : span too small.  fewer data values than degrees of freedom.

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : pseudoinverse used at 4710.9

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : neighborhood radius 17489

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : reciprocal condition number 0

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : There are other near singularities as well. 1.5949e+08

    ## Warning in predLoess(object$y, object$x, newx = if (is.null(newdata)) object$x
    ## else if (is.data.frame(newdata))
    ## as.matrix(model.frame(delete.response(terms(object)), : span too small.  fewer
    ## data values than degrees of freedom.

    ## Warning in predLoess(object$y, object$x, newx = if (is.null(newdata)) object$x
    ## else if (is.data.frame(newdata))
    ## as.matrix(model.frame(delete.response(terms(object)), : pseudoinverse used at
    ## 4710.9

    ## Warning in predLoess(object$y, object$x, newx = if (is.null(newdata)) object$x
    ## else if (is.data.frame(newdata))
    ## as.matrix(model.frame(delete.response(terms(object)), : neighborhood radius
    ## 17489

    ## Warning in predLoess(object$y, object$x, newx = if (is.null(newdata)) object$x
    ## else if (is.data.frame(newdata))
    ## as.matrix(model.frame(delete.response(terms(object)), : reciprocal condition
    ## number 0

    ## Warning in predLoess(object$y, object$x, newx = if (is.null(newdata)) object$x
    ## else if (is.data.frame(newdata))
    ## as.matrix(model.frame(delete.response(terms(object)), : There are other near
    ## singularities as well. 1.5949e+08

    ## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning
    ## -Inf

![](TP_analysis_files/figure-gfm/unnamed-chunk-48-1.png)<!-- -->

``` r
data %>% ggplot(aes(x = sample_time, y = pH, color = substrate)) +
    facet_wrap(~substrate, 
               labeller = as_labeller(c(basalt = "Basalt", limestone = "Limestone", ocean = "Ocean"))) + 
  geom_smooth(method = "gam") + #, se=FALSE) + 
  geom_point() + theme_bw() +
  labs(x = "Sample Time", y = "pH")  + 
  guides(color = "none") +
  theme(axis.text.x = element_text(angle = 25))
```

    ## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'

![](TP_analysis_files/figure-gfm/unnamed-chunk-49-1.png)<!-- -->

Shift times so 6:30 am is the new start time

``` r
start <- as.numeric(hms::as_hms("06:00:00"))
data <- data %>%
  mutate(
    time_sec   = as.numeric(sample_time),
    time_shift = (time_sec - start) %% (24 * 60 * 60)  # 0 at 06:00, increases to 05:59 next day
  )
```

Now for the vertical line for sunset

``` r
# 18:30 in seconds since midnight
line_time <- as.numeric(hms::as_hms("18:00:00"))

# Shifted position relative to 05:30
line_shift <- (line_time - start) %% (24 * 60 * 60)
```

#### pH (not temperature normalized)

``` r
data %>% ggplot(aes(x = time_shift, y = pH, color = substrate)) +
    facet_wrap(~substrate, 
               labeller = as_labeller(c(basalt = "Basalt", limestone = "Limestone", ocean = "Ocean"))) + 
  geom_smooth(method = "gam") + #method = "lm", se=FALSE) + 
  geom_point() + 
  theme_bw() +
  guides(color = "none") +
  theme(axis.text.x = element_text(angle = 25)) +
  geom_vline(xintercept = line_shift, linetype = "dashed", color = "gray40") + # vertical line at sunset
  labs(x = "Sample Time") +
   scale_x_continuous(
    limits = c(0, 24 * 60 * 60 - 1),      # full day: 06:30 -> 06:29 next day
    expand = c(0, 0),
    breaks = seq(0, 24 * 60 * 60 - 1, by = 3 * 3600),  # every 3 hours (avoid 86400 endpoint)
    labels = function(x) format(hms::as_hms((x + start) %% (24 * 60 * 60)), "%H:%M")
  )
```

    ## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'

![](TP_analysis_files/figure-gfm/unnamed-chunk-52-1.png)<!-- --> \####
pH (Temperature Normalized)

``` r
# temp normalize: pH*temp_pool/25.2
data %>% ggplot(aes(x = time_shift, y = pH*temp_pool/25.2, color = substrate)) +
    facet_wrap(~substrate, 
               labeller = as_labeller(c(basalt = "Basalt", limestone = "Limestone", ocean = "Ocean"))) + 
  geom_smooth(method = "gam") + #method = "lm", se=FALSE) + 
  geom_point() + 
  theme_bw() +
  guides(color = "none") +
  theme(axis.text.x = element_text(angle = 25)) +
  geom_vline(xintercept = line_shift, linetype = "dashed", color = "gray40") + # vertical line at sunset
  labs(x = "Sample Time") +
   scale_x_continuous(
    limits = c(0, 24 * 60 * 60 - 1),      # full day: 06:30 -> 06:29 next day
    expand = c(0, 0),
    breaks = seq(0, 24 * 60 * 60 - 1, by = 3 * 3600),  # every 3 hours (avoid 86400 endpoint)
    labels = function(x) format(hms::as_hms((x + start) %% (24 * 60 * 60)), "%H:%M")
  )
```

    ## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'

![](TP_analysis_files/figure-gfm/unnamed-chunk-53-1.png)<!-- -->

``` r
data %>% ggplot(aes(x = time_shift, y = pH, color = substrate)) +
    facet_wrap(~substrate, 
               labeller = as_labeller(c(basalt = "Basalt", limestone = "Limestone", ocean = "Ocean"))) + 
  geom_smooth(method = "gam") + #method = "lm", se=FALSE) + 
  geom_point() + 
  theme_bw() +
  guides(color = "none") +
 # theme(axis.text.x = element_text(angle = 25)) +
  theme(strip.text = element_text(size = 14),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank()) +
  geom_vline(xintercept = line_shift, linetype = "dashed", color = "gray9") + # vertical line at sunset
  scale_x_continuous(
    limits = c(0, 24 * 60 * 60 - 1),      # full day: 06:30 -> 06:29 next day
    expand = c(0, 0),
    breaks = seq(0, 24 * 60 * 60 - 1, by = 3 * 3600),  # every 3 hours (avoid 86400 endpoint)
    labels = function(x) format(hms::as_hms((x + start) %% (24 * 60 * 60)), "%H:%M")
  )
```

    ## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'

![](TP_analysis_files/figure-gfm/unnamed-chunk-54-1.png)<!-- -->

``` r
#ggsave(here("Output", "pH_over_time.png"), width = 6, height = 2)
```

### TA

``` r
data %>% filter(site %in% "Kaihalulu Beach") %>% ggplot(aes(x = sample_time, y = TA_norm, color = pool_number)) +   facet_wrap(~site, scales = "free_x") + geom_line(linewidth = 0.8) + geom_point() + theme_minimal() + labs(title = "TA normalized")  + theme(axis.text.x = element_text(angle = 30))
```

![](TP_analysis_files/figure-gfm/unnamed-chunk-55-1.png)<!-- -->

``` r
data %>% filter(site %in% "Sandy Beach") %>% ggplot(aes(x = sample_time, y = TA_norm, color = pool_number)) +   facet_wrap(~site, scales = "free_x") + geom_line(linewidth = 0.8) + geom_point() + theme_minimal() + labs(title = "TA normalized")  + theme(axis.text.x = element_text(angle = 30))
```

![](TP_analysis_files/figure-gfm/unnamed-chunk-56-1.png)<!-- -->

``` r
data %>% ggplot(aes(x = sample_time, y = TA_norm, color = site)) +
    facet_wrap(~substrate) +
  geom_smooth() + 
  geom_point() + 
  theme_minimal() + 
  labs(title = "TA Norm")  +
  theme(axis.text.x = element_text(angle = 30))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : span too small.  fewer data values than degrees of freedom.

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : pseudoinverse used at 4710.9

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : neighborhood radius 17489

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : reciprocal condition number 0

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : There are other near singularities as well. 1.5949e+08

    ## Warning in predLoess(object$y, object$x, newx = if (is.null(newdata)) object$x
    ## else if (is.data.frame(newdata))
    ## as.matrix(model.frame(delete.response(terms(object)), : span too small.  fewer
    ## data values than degrees of freedom.

    ## Warning in predLoess(object$y, object$x, newx = if (is.null(newdata)) object$x
    ## else if (is.data.frame(newdata))
    ## as.matrix(model.frame(delete.response(terms(object)), : pseudoinverse used at
    ## 4710.9

    ## Warning in predLoess(object$y, object$x, newx = if (is.null(newdata)) object$x
    ## else if (is.data.frame(newdata))
    ## as.matrix(model.frame(delete.response(terms(object)), : neighborhood radius
    ## 17489

    ## Warning in predLoess(object$y, object$x, newx = if (is.null(newdata)) object$x
    ## else if (is.data.frame(newdata))
    ## as.matrix(model.frame(delete.response(terms(object)), : reciprocal condition
    ## number 0

    ## Warning in predLoess(object$y, object$x, newx = if (is.null(newdata)) object$x
    ## else if (is.data.frame(newdata))
    ## as.matrix(model.frame(delete.response(terms(object)), : There are other near
    ## singularities as well. 1.5949e+08

    ## Warning in max(ids, na.rm = TRUE): no non-missing arguments to max; returning
    ## -Inf

![](TP_analysis_files/figure-gfm/unnamed-chunk-57-1.png)<!-- --> \####
TA (Salinity Normalized)

``` r
data %>% ggplot(aes(x = sample_time, y = TA_norm, color = substrate)) +
    facet_wrap(~substrate, 
               labeller = as_labeller(c(basalt = "Basalt", limestone = "Limestone", ocean = "Ocean"))) + 
  geom_smooth(method = "gam") + #, se=FALSE) + 
  geom_point() + theme_bw() +
  labs(x = "Sample Time", y = "Total Alkalinity")  + 
  guides(color = "none") +
  theme(axis.text.x = element_text(angle = 25)) 
```

    ## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'

![](TP_analysis_files/figure-gfm/unnamed-chunk-58-1.png)<!-- --> \####
TA (not salinity normalized)

``` r
# for TA over time plot use raw data, not normalized
data %>% ggplot(aes(x = time_shift, y = TA, color = substrate)) +
    facet_wrap(~substrate, 
               labeller = as_labeller(c(basalt = "Basalt", limestone = "Limestone", ocean = "Ocean"))) + 
  geom_smooth(method = "gam") + #method = "lm", se=FALSE) + 
  geom_point() + 
  theme_bw() +
  guides(color = "none") +
  theme(axis.text.x = element_text(angle = 25)) +
  geom_vline(xintercept = line_shift, linetype = "dashed", color = "gray40") + # vertical line at sunset
  labs(x = "Sample Time", y = "Total Alkalinty") +
   scale_x_continuous(
    limits = c(0, 24 * 60 * 60 - 1),      # full day: 06:30 -> 06:29 next day
    expand = c(0, 0),
    breaks = seq(0, 24 * 60 * 60 - 1, by = 3 * 3600),  # every 3 hours (avoid 86400 endpoint)
    labels = function(x) format(hms::as_hms((x + start) %% (24 * 60 * 60)), "%H:%M")
  )
```

    ## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'

![](TP_analysis_files/figure-gfm/unnamed-chunk-59-1.png)<!-- -->

TA Not normalized to salinity

``` r
data %>% ggplot(aes(x = sample_time, y = TA, color = substrate)) +
    facet_wrap(~substrate, 
               labeller = as_labeller(c(basalt = "Basalt", limestone = "Limestone", ocean = "Ocean"))) + 
  geom_smooth() + #method = "lm", se=FALSE) + 
  geom_point() + theme_bw() +
  labs(x = "Sample Time", y = "Total Alkalinity Regular")  + 
  guides(color = "none") +
  theme(axis.text.x = element_text(angle = 25)) 
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](TP_analysis_files/figure-gfm/unnamed-chunk-60-1.png)<!-- -->

### O2

``` r
data %>% filter(site %in% "Kaihalulu Beach") %>% ggplot(aes(x = sample_time, y = dissolved_oxygen, color = pool_number)) + facet_wrap(~site, scales = "free_x") + geom_line(linewidth = 0.8) + geom_point() + theme_minimal() + labs(title = "DO")  + theme(axis.text.x = element_text(angle = 30))
```

![](TP_analysis_files/figure-gfm/unnamed-chunk-61-1.png)<!-- -->

``` r
data %>% filter(site %in% "Sandy Beach") %>% ggplot(aes(x = sample_time, y = dissolved_oxygen, color = pool_number)) +   facet_wrap(~site, scales = "free_x") + geom_line(linewidth = 0.8) + geom_point() + theme_minimal() + labs(title = "DO")  + theme(axis.text.x = element_text(angle = 30))
```

    ## Warning: Removed 55 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

    ## Warning: Removed 66 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-62-1.png)<!-- -->

``` r
data %>% ggplot(aes(x = sample_time, y = dissolved_oxygen)) +
    facet_wrap(~substrate) +
  geom_smooth(method = "lm", se=FALSE) + 
  geom_point() + 
  theme_minimal() + 
  labs(title = "DO")  +
  theme(axis.text.x = element_text(angle = 30))
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 117 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 117 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-63-1.png)<!-- -->

``` r
data %>% ggplot(aes(x = sample_time, y = dissolved_oxygen, color = site)) +
    facet_wrap(~substrate, 
               labeller = as_labeller(c(basalt = "Basalt", limestone = "Limestone", ocean = "Ocean"))) + 
  geom_smooth(method = "lm", se=FALSE) + 
  geom_point() + theme_bw() +
  labs(x = "Sample Time", y = "Dissolved Oxygen")  + 
  guides(color = "none") +
  theme(axis.text.x = element_text(angle = 25)) 
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 117 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 117 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-64-1.png)<!-- -->

### Deltas

``` r
deltas %>% ggplot(aes(x = substrate, y = delta_pH_norm, color = substrate)) + #, label = pool_number)) + 
  geom_boxplot(alpha = 0.7) + 
  geom_point() + 
 # geom_text(hjust=0, vjust=0) +
  theme_bw() + 
  scale_color_manual(values = c("gray20", "sienna2", "palegreen3", "dodgerblue3")) +
  guides(color = "none") +
  labs(x = "Substrate", y = "Delta pH") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")
```

    ## Warning: Removed 22 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

    ## Warning: Removed 22 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-65-1.png)<!-- -->

``` r
deltas %>% ggplot(aes(x = substrate, y = delta_TA_norm, color = substrate)) + #, label = pool_number)) + 
  geom_boxplot(alpha = 0.7) + 
  geom_point() + 
 # geom_text(hjust=0, vjust=0) +
  theme_bw() + 
  scale_color_manual(values = c("gray20", "sienna2", "palegreen3", "dodgerblue3")) +
  guides(color = "none") +
  labs(x = "Substrate", y = "Delta TA") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")
```

    ## Warning: Removed 22 rows containing non-finite outside the scale range
    ## (`stat_boxplot()`).

    ## Warning: Removed 22 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-66-1.png)<!-- -->

### Delta TA vs Delta pH

``` r
# not time normalized
deltas %>% 
  ggplot(aes(x = delta_pH, y = delta_TA)) + 
  geom_point(size = 2, alpha = 0.7) + 
  geom_smooth(method = "lm", color = "sienna3") + 
  facet_wrap(~substrate,
               labeller = as_labeller(c(basalt = "Basalt", limestone = "Limestone", ocean = "Ocean"))) +
  theme_bw() + 
  labs(x = expression(paste(Delta, " pH")), 
       y = expression(paste(Delta, " Total Alkalinity"))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  scale_y_reverse() + # so calcification increases up 
  theme(strip.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(hjust = 0.9, angle = 45))
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 22 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 22 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-67-1.png)<!-- -->

``` r
#ggsave(here("Output", "delta_TA_vs_delta_pH.png"), width = 4, height = 8)
```

#### Delta pH vs Delta TA (time normalized)

``` r
deltas %>% 
  filter(!substrate %in% "ocean") %>%
  ggplot(aes(x = delta_pH_norm, y = delta_TA_norm, color = substrate)) + 
  geom_point(size = 2, alpha = 0.7) + 
  geom_smooth(method = "lm", aes(fill = substrate)) + 
  #facet_wrap(~substrate, nrow = 3,
 #              labeller = as_labeller(c(basalt = "Basalt", limestone = "Limestone"))) +
  theme_bw() + 
  labs(x = expression(paste(Delta, " pH")), 
       y = expression(paste(Delta, " Total Alkalinity")),
       color = "Substrate") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_y_reverse() + # so calcification increases up 
  theme(strip.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 13),
        legend.position = "bottom",
        legend.text=element_text(size=16),
        legend.title = element_text(size = 16)
        ) +
  scale_colour_manual(labels = c("Basalt", "Limestone"), values = c("#5EABD6", "#e75b4a")) +
  scale_fill_manual(values = c("#5EABD6", "#e75b4a")) + 
  guides(fill="none", color = "none")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 20 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 20 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-68-1.png)<!-- -->

``` r
ggsave(here("Output", "delta_TA_vs_delta_pH_onepanel.png"), width = 4, height = 4)
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 20 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).
    ## Removed 20 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

``` r
data %>% 
  filter(!substrate %in% "ocean") %>% 
  ggplot(aes(x = pH, y = TA_norm, color = factor(date), shape = day_night)) + 
  geom_point(size = 2, alpha = 0.7) + 
  geom_smooth(method = "lm") + 
  facet_wrap(~substrate, 
               labeller = as_labeller(c(basalt = "Basalt", limestone = "Limestone"))) +
  theme_bw() + 
  labs(x = "pH", y = "Total Alkalinity") 
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](TP_analysis_files/figure-gfm/unnamed-chunk-69-1.png)<!-- -->

``` r
# committee meeting figure
deltas %>% 
  filter(substrate == "limestone" | substrate == "basalt") %>%
  ggplot(aes(x = delta_pH_norm, y = delta_TA_norm)) + 
  facet_wrap(~substrate, 
               labeller = as_labeller(c(basalt = "Basalt", limestone = "Limestone", ocean = "Ocean"))) +
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_bw() + 
  labs(x = "Delta pH/Hour", y = "Delta TA/Hour") 
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 20 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 20 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-70-1.png)<!-- -->

#### Delta DIC vs Delta pH (Time Normalized)

``` r
deltas %>% 
  filter(!substrate %in% "ocean") %>%
  ggplot(aes(x = delta_DIC_norm, y = delta_pH_norm, color = substrate)) + 
  geom_point(size = 2, alpha = 0.7) + 
  geom_smooth(method = "lm", aes(fill = substrate)) +#, color = "palegreen4") + 
  #facet_wrap(~substrate, nrow = 3,
         #      labeller = as_labeller(c(basalt = "Basalt", limestone = "Limestone", ocean = "Ocean"))) +
  theme_bw() + 
  labs(y = expression(paste(Delta, " pH")), 
       x = expression(paste(Delta, " Dissolved Inorganic Carbon")),
       color = "Substrate") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_x_reverse() + # so production increases to the right
  theme(strip.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 13)) +
  scale_colour_manual(labels = c("Basalt", "Limestone"), values = c("#5EABD6", "#e75b4a")) +
  scale_fill_manual(values = c("#5EABD6", "#e75b4a")) + 
  guides(fill="none", color = "none")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 20 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 20 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-71-1.png)<!-- -->

``` r
ggsave(here("Output", "delta_DIC_vs_delta_pH_onepanel.png"), width = 4, height = 4)
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 20 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).
    ## Removed 20 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

------------------------------------------------------------------------

Pool 1 salinity lab vs TA reg

``` r
data %>% 
  filter(!substrate %in% c("ocean", "basalt")) %>% 
  filter(pool_number == 1) %>%
  ggplot(aes(x = salinity_pool, y = TA)) + 
  geom_point(size = 2, alpha = 0.7) + 
  geom_smooth(method = "lm") + 
  facet_wrap(~substrate, 
               labeller = as_labeller(c(basalt = "Basalt", limestone = "Limestone"))) +
  theme_bw() + 
  labs(x = "Pool Salinity", y = "Reg Total Alkalinity") 
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](TP_analysis_files/figure-gfm/unnamed-chunk-72-1.png)<!-- -->

### Salinity vs temp

``` r
data %>% 
  ggplot(aes(x = temp_field, y = salinity_field)) + 
  geom_point(size = 2, alpha = 0.7) + 
  geom_smooth(method = "lm") + 
  facet_wrap(~substrate, scales = "free_x",
               labeller = as_labeller(c(basalt = "Basalt", limestone = "Limestone"))) +
  theme_bw() + 
  labs(x = "Temperature", y = "Field Salinity") 
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](TP_analysis_files/figure-gfm/unnamed-chunk-73-1.png)<!-- -->

### DO and pH

``` r
data %>% 
  #filter(!substrate %in% c("ocean")) %>% 
  ggplot(aes(x = pH, y = dissolved_oxygen)) + 
  geom_point(size = 2, alpha = 0.7) + 
  geom_smooth(method = "lm") + 
  facet_wrap(~substrate, 
               labeller = as_labeller(c(basalt = "Basalt", limestone = "Limestone"))) +
  theme_bw() + 
  labs(x = "pH", y = "Dissolved Oxygen") 
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 117 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 117 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-74-1.png)<!-- -->

#### Delta pH vs Delta O2 (Time Normalized)

``` r
deltas %>% 
  filter(!substrate %in% "ocean") %>%
  ggplot(aes(x = delta_pH_norm, y = delta_O2_norm, color = substrate)) + 
  geom_point(size = 2, alpha = 0.7) + 
  geom_smooth(method = "lm", aes(fill = substrate)) + 
  facet_wrap(~substrate,
               labeller = as_labeller(c(basalt = "Basalt", limestone = "Limestone"))) +
  theme_bw() + 
  labs(x = expression(paste(Delta, " pH")), 
       y = expression(paste(Delta, " Dissolved Oxygen")),
       color = "Substrate") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  theme(strip.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 13),
        legend.position = "bottom",
        legend.text=element_text(size=16),
        legend.title = element_text(size = 16)
        ) +
  scale_colour_manual(labels = c("Basalt", "Limestone"), values = c("#5EABD6", "#e75b4a")) +
  scale_fill_manual(values = c("#5EABD6", "#e75b4a")) + 
  guides(fill="none", color = "none")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 83 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 83 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-75-1.png)<!-- -->

``` r
# Time normalized delta ph vs delta o2
deltas %>% 
  filter(!substrate %in% "ocean") %>%
  ggplot(aes(x = delta_pH_norm, y = delta_O2_norm)) + 
  geom_point(size = 2, alpha = 0.7) + 
  geom_smooth(method = "lm") + 
  #facet_wrap(~substrate, nrow = 3,
 #              labeller = as_labeller(c(basalt = "Basalt", limestone = "Limestone"))) +
  theme_bw() + 
  labs(x = expression(paste(Delta, " pH")), 
       y = expression(paste(Delta, " Dissolved Oxygen"))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  theme(strip.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 13),
        legend.position = "bottom",
        legend.text=element_text(size=16),
        legend.title = element_text(size = 16)
        ) 
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 83 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 83 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-76-1.png)<!-- -->

### TA/DIC Plot

``` r
data %>%
  ggplot(aes(x = DIC, y = TA, color = site)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "lm",se = FALSE) +
  facet_wrap(~substrate, scales = "free_x",
               labeller = as_labeller(c(basalt = "Basalt", limestone = "Limestone", ocean = "Ocean"))) +
  guides(color = "none") +
  labs(x = expression(paste("DIC (",mu,"mol kg"^-1,")")),
       y = expression(paste("TA (",mu,"mol kg"^-1,")"))) +
  theme_bw() 
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](TP_analysis_files/figure-gfm/unnamed-chunk-77-1.png)<!-- -->

### NEC + NEP

``` r
deltas %>% ggplot(aes(x = delta_pH, y = NEC)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  #facet_wrap(~substrate) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  theme_bw() +
  labs(y = expression(paste("NEC (mmol m"^-2," h"^-1,")")))
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 115 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 115 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-78-1.png)<!-- -->

``` r
deltas %>% ggplot(aes(x = mean_pH, y = NEC)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  #facet_wrap(~substrate) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  theme_bw() +
  labs(y = expression(paste("NEC (mmol m"^-2," h"^-1,")")))
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 115 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 115 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-79-1.png)<!-- -->

``` r
deltas %>% ggplot(aes(x = mean_pH, y = NEC, color = substrate)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  #facet_wrap(~substrate) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  theme_bw() +
  labs(y = expression(paste("NEC (mmol m"^-2," h"^-1,")")))
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 115 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 115 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-80-1.png)<!-- -->

``` r
deltas %>% ggplot(aes(x = NEP, y = delta_pH)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  #facet_wrap(~substrate) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  theme_bw() +
  labs(x = expression(paste("NEP (mmol m"^-2," h"^-1,")")))
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 115 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 115 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-81-1.png)<!-- -->

``` r
deltas %>% ggplot(aes(x = NEP, y = mean_pH)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  #facet_wrap(~substrate) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  theme_bw() +
  labs(x = expression(paste("NEP (mmol m"^-2," h"^-1,")")))
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 115 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 115 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-82-1.png)<!-- -->

``` r
deltas %>% ggplot(aes(x = NEP, y = mean_pH, color = substrate)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  #facet_wrap(~substrate) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  theme_bw() +
  labs(x = expression(paste("NEP (mmol m"^-2," h"^-1,")")))
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 115 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 115 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-83-1.png)<!-- -->

``` r
deltas %>% ggplot(aes(x = NEP, y = NEC)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  #facet_wrap(~substrate) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  theme_bw() +
  labs(x = expression(paste("NEP (mmol m"^-2," h"^-1,")")),
       y = expression(paste("NEC (mmol m"^-2," h"^-1,")")))
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 115 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 115 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-84-1.png)<!-- -->

``` r
deltas %>% ggplot(aes(x = NEP, y = NEC, color = substrate)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  #facet_wrap(~substrate) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  theme_bw() +
  labs(x = expression(paste("NEP (mmol m"^-2," h"^-1,")")),
       y = expression(paste("NEC (mmol m"^-2," h"^-1,")")))
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 115 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 115 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-85-1.png)<!-- -->

### Aragonite Saturation State

``` r
deltas %>% ggplot(aes(x = delta_pH, y = delta_aragonite)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  #facet_wrap(~substrate) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +

  theme_bw()
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 22 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 22 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-86-1.png)<!-- -->

``` r
deltas %>% ggplot(aes(x = delta_aragonite, y = NEC)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  #facet_wrap(~substrate) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  theme_bw() +
  labs(y = expression(paste("NEC (mmol m"^-2," h"^-1,")")))
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 115 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 115 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-87-1.png)<!-- -->

### fDOM

``` r
data %>% 
  filter(!(is.na(m_to_c))) %>%
  ggplot(aes(x = sample_time, y = m_to_c, color = pool_number)) +
  geom_point() +
  geom_line() + 
  theme_bw()
```

![](TP_analysis_files/figure-gfm/unnamed-chunk-88-1.png)<!-- -->

``` r
# M:C is lower in the tidepools than in the ocean
# decreases over time (at night at least so far) indicating consumption of marine humics, probably by the microbes
# expectation is this would increase during the day 
```

``` r
data %>%
  filter(!(is.na(humics))) %>%
  ggplot(aes(x = sample_time, y = humics, color = pool_number)) +
  geom_point() +
  geom_line() +
  theme_bw()
```

![](TP_analysis_files/figure-gfm/unnamed-chunk-89-1.png)<!-- -->

``` r
# total humics higher in the pools than in the ocean on average
```

``` r
# pretty strong relationship between oxygen and M:C
data %>%
  filter(!(pool_number == "ocean")) %>% # just look at the pools
  ggplot(aes(x = dissolved_oxygen,y = m_to_c))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title = "Primary productivity is driving composition of marine to terrestrial humics",
       subtitle = "Higher O2 = higher production of marine humics\n or more consumption with more microbial respiration since at night") +
  theme_bw() #+ 
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 232 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 232 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-90-1.png)<!-- -->

``` r
  #facet_wrap(~substrate)
```

## Residuals Stuff

### pH

Fit regression for both substrate types combined

``` r
pH_mod <- lm(pH ~ sample_time, data = (data %>% filter(!substrate == "ocean")))
summary(pH_mod)
```

    ## 
    ## Call:
    ## lm(formula = pH ~ sample_time, data = (data %>% filter(!substrate == 
    ##     "ocean")))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.63184 -0.22171 -0.01937  0.20005  0.79984 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 7.842e+00  3.538e-02  221.62   <2e-16 ***
    ## sample_time 9.041e-06  7.687e-07   11.76   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2904 on 260 degrees of freedom
    ## Multiple R-squared:  0.3473, Adjusted R-squared:  0.3448 
    ## F-statistic: 138.3 on 1 and 260 DF,  p-value: < 2.2e-16

``` r
data_resid_pH <- augment(pH_mod, (data %>% filter(!substrate == "ocean")))
```

``` r
resid_ph <- lm(.resid ~ substrate, data_resid_pH)
summary(resid_ph)
```

    ## 
    ## Call:
    ## lm(formula = .resid ~ substrate, data = data_resid_pH)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.63838 -0.21856 -0.01521  0.19817  0.79331 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)         0.006534   0.023390   0.279    0.780
    ## substratelimestone -0.015852   0.036431  -0.435    0.664
    ## 
    ## Residual standard error: 0.2903 on 260 degrees of freedom
    ## Multiple R-squared:  0.0007277,  Adjusted R-squared:  -0.003116 
    ## F-statistic: 0.1893 on 1 and 260 DF,  p-value: 0.6638

``` r
Anova(resid_ph)
```

    ## Anova Table (Type II tests)
    ## 
    ## Response: .resid
    ##           Sum Sq  Df F value Pr(>F)
    ## substrate  0.016   1  0.1893 0.6638
    ## Residuals 21.906 260

``` r
resid_ph_comp <- lm(.resid ~ producer_pcover + producer_density, data_resid_pH)
summary(resid_ph_comp)
```

    ## 
    ## Call:
    ## lm(formula = .resid ~ producer_pcover + producer_density, data = data_resid_pH)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.5828 -0.2354  0.0004  0.2324  0.8146 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)       0.1122944  0.0525502   2.137  0.03414 * 
    ## producer_pcover  -0.0013235  0.0007328  -1.806  0.07279 . 
    ## producer_density -0.0047574  0.0016962  -2.805  0.00567 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3005 on 159 degrees of freedom
    ##   (100 observations deleted due to missingness)
    ## Multiple R-squared:  0.07988,    Adjusted R-squared:  0.0683 
    ## F-statistic: 6.901 on 2 and 159 DF,  p-value: 0.001336

``` r
Anova(resid_ph_comp)
```

    ## Anova Table (Type II tests)
    ## 
    ## Response: .resid
    ##                   Sum Sq  Df F value   Pr(>F)   
    ## producer_pcover   0.2946   1  3.2622 0.072787 . 
    ## producer_density  0.7105   1  7.8662 0.005665 **
    ## Residuals        14.3611 159                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
data_resid_pH %>% 
  ggplot(aes(x = producer_pcover, y = .resid, color = substrate)) + 
  geom_point() + 
  geom_smooth() +
  facet_wrap(~substrate)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 97 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 97 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-95-1.png)<!-- -->

``` r
data_resid_pH %>% 
  ggplot(aes(x = producer_density, y = .resid, color = substrate)) + 
  geom_point() + 
  geom_smooth() +
  facet_wrap(~substrate, scales = "free_x")
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 100 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 100 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-96-1.png)<!-- -->

### TA

Fit regression for both substrate types combined

``` r
TA_mod <- lm(TA ~ sample_time, data = (data %>% filter(!substrate == "ocean")))
summary(TA_mod)
```

    ## 
    ## Call:
    ## lm(formula = TA ~ sample_time, data = (data %>% filter(!substrate == 
    ##     "ocean")))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1083.08  -194.56   -61.84   115.06  1884.94 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  2.755e+03  4.258e+01  64.711   <2e-16 ***
    ## sample_time -8.422e-03  9.250e-04  -9.105   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 349.4 on 260 degrees of freedom
    ## Multiple R-squared:  0.2418, Adjusted R-squared:  0.2388 
    ## F-statistic:  82.9 on 1 and 260 DF,  p-value: < 2.2e-16

``` r
data_resid_TA <- augment(TA_mod, (data %>% filter(!substrate == "ocean")))
```

``` r
resid_TA <- lm(.resid ~ substrate, data_resid_TA)
summary(resid_TA)
```

    ## 
    ## Call:
    ## lm(formula = .resid ~ substrate, data = data_resid_TA)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -984.75 -184.13  -28.49  142.76 1744.74 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -98.32      26.51  -3.709 0.000254 ***
    ## substratelimestone   238.53      41.29   5.777 2.16e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 328.9 on 260 degrees of freedom
    ## Multiple R-squared:  0.1138, Adjusted R-squared:  0.1104 
    ## F-statistic: 33.38 on 1 and 260 DF,  p-value: 2.161e-08

``` r
Anova(resid_TA)
```

    ## Anova Table (Type II tests)
    ## 
    ## Response: .resid
    ##             Sum Sq  Df F value    Pr(>F)    
    ## substrate  3611703   1  33.379 2.161e-08 ***
    ## Residuals 28132697 260                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
resid_TA_comp <- lm(.resid ~ calcifying_pcover + calcifying_density, data_resid_TA)
summary(resid_TA_comp)
```

    ## 
    ## Call:
    ## lm(formula = .resid ~ calcifying_pcover + calcifying_density, 
    ##     data = data_resid_TA)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1087.96  -156.71   -27.67   125.23  1010.54 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         1.86068   27.46250   0.068    0.946    
    ## calcifying_pcover   0.26690    1.35523   0.197    0.844    
    ## calcifying_density  0.23812    0.01932  12.322   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 282.7 on 159 degrees of freedom
    ##   (100 observations deleted due to missingness)
    ## Multiple R-squared:  0.491,  Adjusted R-squared:  0.4846 
    ## F-statistic:  76.7 on 2 and 159 DF,  p-value: < 2.2e-16

``` r
Anova(resid_TA_comp)
```

    ## Anova Table (Type II tests)
    ## 
    ## Response: .resid
    ##                      Sum Sq  Df  F value Pr(>F)    
    ## calcifying_pcover      3099   1   0.0388 0.8441    
    ## calcifying_density 12131296   1 151.8321 <2e-16 ***
    ## Residuals          12704009 159                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
data_resid_TA %>% 
  ggplot(aes(x = calcifying_pcover, y = .resid, color = substrate)) + 
  geom_point() + 
  geom_smooth() +
  facet_wrap(~substrate)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 97 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 97 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-101-1.png)<!-- -->

``` r
data_resid_TA %>% 
  ggplot(aes(x = log(calcifying_density), y = .resid, color = substrate)) + 
  geom_point() + 
  geom_smooth() +
  facet_wrap(~substrate)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 100 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 100 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TP_analysis_files/figure-gfm/unnamed-chunk-102-1.png)<!-- -->
