First Analysis
================
Micaela Chapuis
2025-02-25

## Load Libraries

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(here)
```

    ## here() starts at /Users/micachapuis/GitHub/HawaiiTidepools

``` r
library(seacarb)
```

    ## Loading required package: oce
    ## Loading required package: gsw
    ## Loading required package: SolveSAPHE

## Load in Data

``` r
chem <- read_csv(here("Test Sampling", "Data", "tidepool_chemistry.csv"))
params <- read_csv(here("Test Sampling", "Data", "tidepool_parameters.csv"))
pHSlope <- read_csv(here("Test Sampling", "Data", "pHSlope.csv"))
TA_Sandy <- read_csv(here("Test Sampling", "Data", "TA", "TA_SB_20250216.csv"))
TA_DH <- read_csv(here("Test Sampling", "Data", "TA", "TA_DH_20250223.csv"))
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
TA_Sandy <- TA_Sandy %>% filter(!SampleID %in% junks)
TA_DH <- TA_DH %>% filter(!SampleID %in% junks)
```

Sandy TA data needs to have “sb\_” added to the sample ID

``` r
TA_Sandy$SampleID <- str_c("sb_", TA_Sandy$SampleID)
```

``` r
colnames(TA_Sandy)[2] <- "TA1" # rename the column so I can later coalesce them together

data <- plyr::join_all(list(chem,(TA_Sandy %>% select(SampleID, TA1)),(TA_DH %>% select(SampleID, TA))), by='SampleID', type='left') # join all 3 dfs by sample ID. From the TA dfs pick which columns i want to keep

data$alkalinity <- coalesce(data$TA1, data$TA) # combine the TA1 (from sandy) and TA (from dh) into one alkalinity column
```

Add in pH calculated from tris and physical parameters

``` r
data <- left_join(data, pHSlope)
```

    ## Joining with `by = join_by(date, SampleID)`

``` r
data <- left_join(data, (params %>% select(date, site, pool_number, substrate, perimeter_m, surface_area_m2)))
```

    ## Joining with `by = join_by(date, site, pool_number)`

Selecting columns

``` r
data <- data %>% select(-observers, -notes, -TA1, -TA)
```

Separating Sandy Beach and Diamond Head

``` r
sandy_data <- data %>% filter(site %in% "Sandy Beach") 
dh_data <- data %>% filter(site %in% "Diamond Head")
```

## Data Viz!

### Combined

``` r
data %>% ggplot(aes(x = time_point, y = temp_pool, color = pool_number)) + facet_wrap(~site, scales = "free_x") + geom_line(linewidth = 0.8) +  geom_point() + theme_minimal() + labs(title = "Temperature")
```

![](first_analysis_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
data %>% ggplot(aes(x = sample_time, y = temp_pool, color = pool_number, shape = site)) + geom_line(linewidth = 0.8) + geom_point() + theme_minimal() + labs(title = "temperature")
```

![](first_analysis_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
data %>% ggplot(aes(x = time_point, y = pH, color = pool_number)) + facet_wrap(~site, scales = "free_x") + geom_line(linewidth = 0.8) + geom_point() + theme_minimal() + labs(title = "pH")
```

![](first_analysis_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
data %>% ggplot(aes(x = sample_time, y = pH, color = pool_number, shape = site)) + geom_line(linewidth = 0.8) + geom_point() + theme_minimal() + labs(title = "pH")
```

![](first_analysis_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
data %>% ggplot(aes(x = time_point, y = alkalinity, color = pool_number)) + facet_wrap(~site, scales = "free_x") + geom_line(linewidth = 0.8) + geom_point() + theme_minimal() + labs(title = "TA")
```

![](first_analysis_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
data %>% ggplot(aes(x = sample_time, y = alkalinity, color = pool_number, shape = site)) + geom_line(linewidth = 0.8) + geom_point() + theme_minimal() + labs(title = "Alkalinity")
```

![](first_analysis_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
data %>% ggplot(aes(x = time_point, y = Salinity_lab, color = pool_number)) + facet_wrap(~site, scales = "free_x") + geom_line(linewidth = 0.8) + geom_point() + theme_minimal() + labs(title = "Salinity")
```

![](first_analysis_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
data %>% ggplot(aes(x = sample_time, y = Salinity_lab, color = pool_number, shape = site)) + geom_line(linewidth = 0.8) + geom_point() + theme_minimal() + labs(title = "Salinity")
```

![](first_analysis_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

### Sandy Beach

``` r
sandy_data %>% ggplot(aes(x = time_point, y = temp_pool, color = pool_number)) + geom_line(linewidth = 0.8) + geom_point() + theme_minimal() + labs(title = "Temperature - Sandy Beach")
```

![](first_analysis_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
sandy_data %>% ggplot(aes(x = time_point, y = pH, color = pool_number)) + geom_line(linewidth = 0.8) + geom_point() + theme_minimal() + labs(title = "pH - Sandy Beach")
```

![](first_analysis_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
sandy_data %>% ggplot(aes(x = time_point, y = alkalinity, color = pool_number)) + geom_line(linewidth = 0.8) + geom_point() + theme_minimal() + labs(title = "TA - Sandy Beach")
```

![](first_analysis_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
sandy_data %>% ggplot(aes(x = time_point, y = Salinity_lab, color = pool_number)) + geom_line(linewidth = 0.8) + geom_point() + theme_minimal() + labs(title = "Salinity - Sandy Beach")
```

![](first_analysis_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

### Diamond Head

``` r
dh_data %>% ggplot(aes(x = time_point, y = temp_pool, color = pool_number)) + geom_line(linewidth = 0.8) + geom_point() + theme_minimal() + labs(title = "Temperature - Diamond Head")
```

![](first_analysis_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
dh_data %>% ggplot(aes(x = time_point, y = pH, color = pool_number)) + geom_line(linewidth = 0.8) + geom_point() + theme_minimal() + labs(title = "pH - Diamond Head")
```

![](first_analysis_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
dh_data %>% ggplot(aes(x = time_point, y = alkalinity, color = pool_number)) + geom_line(linewidth = 0.8) + geom_point() + theme_minimal() + labs(title = "TA - Diamond Head")
```

![](first_analysis_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
dh_data %>% ggplot(aes(x = time_point, y = Salinity_lab, color = pool_number)) + geom_line(linewidth = 0.8) + geom_point() + theme_minimal() + labs(title = "Salinity - Diamond Head")
```

![](first_analysis_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->
