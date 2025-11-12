Volume_Calculation
================
Micaela Chapuis
2025-10-16

## Load Libraries

``` r
library(tidyverse)
library(nlme)
library(broom)
library(ggpmisc)
library(here)
```

## Load the Data

``` r
curve <- read_csv(here("Data", "Volume", "nyssas_curve.csv"))
tp_absorbance <- read_csv(here("Data", "Volume", "tidepool_absorbances.csv"))
```

## Calculation Code Taken from Silbiger Lab Protocols

Analyze Data

``` r
curve %>% # this is the dataframe
  group_by(Machine,DyeVolume) %>% # grouping by machine and dye volume
  ggplot(aes(x= Absorbance, y= Volume))+   # setup plot with x and y data
  geom_line() + # adding lines
  facet_wrap(~Machine*DyeVolume) # dividing plots by machine and dye volume 
```

![](Volume_Calculation_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Write a power function for each curve

``` r
pool_coefs <- curve %>%
              group_by(Machine,DyeVolume) %>% #grouping by machine and dye volume
              nest()%>% # nest everything by machine and dye volume
              mutate( # mutate the dataframe
                fit = map(data,~nls(Volume~b*Absorbance^z, start = list(b=-1, z=-1), data = .)), # run the regression model
                tidied = map(fit,tidy)) %>% # make it clean 
              unnest(tidied) %>% # unnest the data so that it is a dataframe again
              select(c(Machine, DyeVolume, term, estimate)) %>% # only select the parameters that we want
              spread(key = term, value =  estimate) %>% # spread the data so that each parameter has its own column
              filter(Machine %in% "SS3000") # filter out Machine Gen5
```

Plot the Results

``` r
formula <- y~I(b*x^z) # power function
ggplot(curve, aes(x = Absorbance, y = Volume))+
  geom_point()+
  geom_smooth(method="nls",  # add best fit line
              formula=formula, # this is an nls argument
              method.args = list(start=c(b=-1, z=-1)), # this too
              se=FALSE, fullrange = TRUE)+
  ylab('Volume (L)')+
  stat_fit_tidy(method = "nls", #add the values for each model
                method.args = list(formula = formula, start=c(b=-1, z=-1)),
                label.x = "left",
                label.y = 1.5,
                mapping = aes(label = paste("b~`=`~", signif(..b_estimate.., digits = 3),
                                  "%+-%", signif(..b_se.., digits = 2),
                                  "~~~~z~`=`~", signif(..z_estimate.., digits = 3),
                                  "%+-%", signif(..z_se.., digits = 2),
                                  sep = "")),
                parse = TRUE) +
  facet_wrap(~Machine*DyeVolume, labeller = label_both)+
  theme_bw()
```

    ## Warning: The dot-dot notation (`..b_estimate..`) was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `after_stat(b_estimate)` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](Volume_Calculation_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
ggsave(here("Data", "Volume", "DyeStandardCurves.png"), device = 'png',width = 9, height = 6)
```

## Now Calculate Pool Volumes

Join the data and calculate pool volume, then take The formula is
A=bV^z, where A is absorbance and V is volume We are using V = (A /
b)^(1/z)

``` r
volume_calc <- tp_absorbance %>%
              left_join(pool_coefs, by = c("dye_volume_ml" = "DyeVolume")) %>%
              mutate(pool_volume = (calculated_absorbance / b)^(1 / z))
```

Take the average volume for each tidepool and calculate SE and SD

``` r
pool_volumes <- volume_calc %>%
                    group_by(pool_number) %>%
                    summarise(
                      mean_volume = mean(pool_volume, na.rm = TRUE),
                      sd_volume = sd(pool_volume, na.rm = TRUE),
                      se_volume = sd(pool_volume, na.rm = TRUE) / sqrt(n()))
```

``` r
write_csv(pool_volumes, here("Data", "pool_volumes.csv"))
```
