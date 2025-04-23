Second Analysis
================
Micaela Chapuis
2025-04-08

## Load Libraries

``` r
library(tidyverse)
library(here)
library(seacarb)
library(lubridate)
library(car)
library(lubridateExtras)
library(lme4)
```

## Load in Data

``` r
chem <- read_csv(here("Test Sampling", "Data", "tidepool_chemistry.csv"))
params <- read_csv(here("Test Sampling", "Data", "tidepool_parameters.csv"))
pHSlope <- read_csv(here("Test Sampling", "Data", "pHSlope.csv"))
TA <- read_csv(here("Test Sampling", "Data", "total_alkalinity.csv"))
```

## Data cleaning and joining

Make all pool numbers factors

``` r
chem$pool_number <- factor(chem$pool_number)
params$pool_number <- factor(params$pool_number)
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

Add in pH calculated from tris and physical parameters

``` r
data <- left_join(data, pHSlope)
```

    ## Joining with `by = join_by(date, sample_ID)`

``` r
data <- left_join(data, (params %>% select(site, pool_number, substrate, perimeter_m, surface_area_m2)), by = c("site", "pool_number"))
```

Selecting columns

``` r
data <- data %>% select(-observers, -notes, Sample.Index)
```

Removing DH Pool 1 (limestone + basalt)

``` r
data <- data %>% filter(!pool_ID == "dh_p1")
```

Adding in “ocean” as substrate

``` r
data <- data %>% mutate(substrate = replace_na(substrate, "ocean"))
```

Salinity Normalize TA

``` r
data <- data %>% mutate(TA_norm = TA*salinity_lab/35)
```

Rounding sample time

``` r
data <- data %>% mutate(round_time = round_hms(sample_time, "hour"),
                        rounded_time = hour(round_time))
```

Calculate delta pH and delta TA

``` r
delta_calc <- data %>% 
  select(date, site, pool_number, pool_ID, substrate, day_night, time_point, temp_pool, sample_time, pH, TA_norm)  %>%
  group_by(date, site, pool_number, pool_ID, substrate, day_night) %>%  # group by metadata
  arrange(time_point, .by_group = TRUE) %>%  # ensure correct order
  reframe(
    delta_pH = if_else((date == "2025-02-16"), 
                       (pH[time_point = 2] - pH[time_point = 1]), # Sandy Beach 1st time (Time 2 - Time 1)
                       (pH[time_point = 3] - pH[time_point = 1])),   # (Time 3 - Time 1)
    
    delta_TA = if_else((date == "2025-02-16"), 
                       (TA_norm[time_point = 2] - TA_norm[time_point = 1]), # Sandy Beach 1st time (Time 2 - Time 1)
                       (TA_norm[time_point = 3] - TA_norm[time_point = 1])),  # (Time 3 - Time 1)

    delta_temp = if_else((date == "2025-02-16"), 
                       (temp_pool[time_point = 2] - temp_pool[time_point = 1]), # Sandy Beach 1st time (Time 2 - Time 1)
                       (temp_pool[time_point = 3] - temp_pool[time_point = 1])),  # (Time 3 - Time 1)

    delta_time = if_else((date == "2025-02-16"), 
                       (as.numeric(difftime(sample_time[time_point = 2], sample_time[time_point = 1], units = "mins"))), # Sandy Beach 1st time (Time 2 - Time 1)
                       (as.numeric(difftime(sample_time[time_point = 3], sample_time[time_point = 1], units = "mins"))))  # Time 3 - Time 1
    
  ) %>% 
    distinct(date, site, pool_number, substrate, .keep_all = TRUE)  # Ensure only one row per pool
```

## Data Viz

``` r
plot(data$salinity_field, data$salinity_lab)
```

![](second_analysis_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
data %>% filter(!substrate %in% "ocean") %>% ggplot(aes(x = substrate, y = pH)) + geom_boxplot() + geom_point() + facet_wrap(~rounded_time)
```

![](second_analysis_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
data %>% filter(!substrate %in% "ocean") %>% ggplot(aes(x = substrate, y = TA_norm)) + geom_boxplot() + geom_point() + facet_wrap(~rounded_time)
```

![](second_analysis_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
# mean pH for each tidepool
data %>% group_by(site, pool_number) %>% mutate(pH_mean = mean(pH)) %>%
  filter(!substrate %in% "ocean") %>% ggplot(aes(x = substrate, y = pH_mean)) + geom_boxplot() + geom_point() + facet_wrap(~day_night)
```

![](second_analysis_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
# mean pH for each date
data %>% group_by(date, site) %>% mutate(pH_mean = mean(pH)) %>%
  filter(!substrate %in% "ocean") %>% ggplot(aes(x = substrate, y = pH_mean)) + geom_boxplot() + geom_point() + facet_wrap(~day_night)
```

![](second_analysis_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
# for night the basalt point at the same ph as limestone is DH basalt, for day DH basalt is higher point ~8.4 and ~8.2
```

``` r
delta_calc %>% filter(!substrate %in% "ocean") %>% ggplot(aes(x = substrate, y = delta_TA)) + geom_boxplot() + geom_point() + facet_wrap(~day_night)
```

![](second_analysis_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
delta_calc %>% filter(!substrate %in% "ocean") %>% ggplot(aes(x = substrate, y = delta_pH)) + geom_boxplot() + geom_point() + facet_wrap(~day_night)
```

![](second_analysis_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
data %>% ggplot(aes(x = sample_time, y = temp_pool)) +
    facet_wrap(~substrate) + geom_smooth() + geom_point() + theme_minimal() + labs(title = "Pool Temp")  + theme(axis.text.x = element_text(angle = 30))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](second_analysis_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
data %>% ggplot(aes(x = sample_time, y = salinity_field)) +
    facet_wrap(~substrate) + geom_smooth() + geom_point() + theme_minimal() + labs(title = "Salinity (Field)")  + theme(axis.text.x = element_text(angle = 30))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](second_analysis_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
data %>% ggplot(aes(x = sample_time, y = salinity_field, color= site)) +
    facet_wrap(~substrate) + geom_smooth() + geom_point() + theme_minimal() + labs(title = "Salinity (Field)")  + theme(axis.text.x = element_text(angle = 30))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](second_analysis_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
data %>% ggplot(aes(x = sample_time, y = pH)) +
    facet_wrap(~substrate) + geom_smooth() + geom_point() + theme_minimal() + labs(title = "pH (tris)")  + theme(axis.text.x = element_text(angle = 30))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](second_analysis_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
data %>% ggplot(aes(x = sample_time, y = TA_norm)) +
    facet_wrap(~substrate) + geom_smooth() + geom_point() + theme_minimal() + labs(title = "TA Norm")  + theme(axis.text.x = element_text(angle = 30))
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](second_analysis_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
data %>% filter(!substrate %in% "ocean") %>% ggplot(aes(x = pH, y = TA_norm)) + geom_point() + geom_smooth(method = "lm") + facet_wrap(~substrate)
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](second_analysis_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
delta_calc %>% 
  filter(substrate == "limestone" | substrate == "basalt") %>%
  ggplot(aes(x = delta_pH, y = delta_TA)) + 
  facet_wrap(~substrate) +
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_bw() + 
  scale_color_manual(values = c("gray20", "sienna2")) +
  guides(color = "none") +
  labs(x = "Delta pH", y = "Delta TA") 
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](second_analysis_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

------------------------------------------------------------------------

``` r
producers <- read_csv(here("Test Sampling", "Data", "prodcover.csv"))
data <- left_join(data, producers, by = "pool_ID")
delta_calc <- left_join(delta_calc, producers, by = "pool_ID")
```

``` r
mod1 <- lmer(delta_pH ~ prods_pcover + (1|pool_ID), data = delta_calc)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
summary(mod1)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: delta_pH ~ prods_pcover + (1 | pool_ID)
    ##    Data: delta_calc
    ## 
    ## REML criterion at convergence: 7.5
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -1.46343 -0.87086  0.06126  0.56450  2.58601 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  pool_ID  (Intercept) 0.00000  0.000   
    ##  Residual             0.04754  0.218   
    ## Number of obs: 25, groups:  pool_ID, 8
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error t value
    ## (Intercept)  0.112447   0.110718   1.016
    ## prods_pcover 0.001761   0.002392   0.736
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## prods_pcovr -0.919
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## boundary (singular) fit: see help('isSingular')
