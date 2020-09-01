suppressPackageStartupMessages(library(tidyverse))
library(glue)
library(ggrepel)
library(here)


dates <- read.csv(here("analysis/data/raw_data/Radiocarbon dates.csv"))


dates_clean <-
  dates %>%
  mutate(age = parse_number(as.character(age)),
         error = parse_number(as.character(error))) %>%
  filter(age < 50000, method=='AMS')

# devtools::install_github('ahb108/rcarbon')
library(rcarbon)
dates_calibrated <-
  calibrate(x=dates_clean$age,
            errors=dates_clean$error,
            calCurves='intcal13')

dates_calibrated_spd <-  spd(dates_calibrated, timeRange = c( 45000, 1000))
plot(dates_calibrated_spd)


# Testing Observed SPDs against theoretical models
## recalibrate dates without normalisation to avoid artificial peaks
spd_dates<- calibrate(x=dates_clean$age,errors=dates_clean$error,normalised=FALSE,verbose=F)

### this code takes long time
spd_test <- modelTest(spd_dates,errors=dates_clean$error,timeRange=c(45000,1000),model='exponential',nsim=200,ncores=3)

summary(spd_test)
plot(spd_test)

uni_test <- modelTest(spd_dates,errors=dates_clean$error,timeRange=c(45000,1000), model='uniform',nsim=200,ncores=3)
plot(uni_test)

lin_test <- modelTest(spd_dates,errors=dates_clean$error,timeRange=c(45000,1000), model='linear',nsim=200,ncores=3)
plot(lin_test)



# zoom in on the 40-30ka period GP is trying
spd_test_zoom <- modelTest(spd_dates,errors=dates_clean$error,timeRange=c(40000,30000),model='exponential',nsim=200,ncores=3)
summary(spd_test_zoom)
plot(spd_test_zoom)

# zoom in on the 40-30ka period, testing other models
uni_test_zoom <- modelTest(spd_dates,errors=dates_clean$error,timeRange=c(40000,30000), model='uniform',nsim=200,ncores=3)
plot(uni_test)

lin_test_zoom <- modelTest(spd_dates,errors=dates_clean$error,timeRange=c(40000,30000), model='linear',nsim=200,ncores=3)
plot(lin_test)



### testing Riris 2019
library(drc)

fit <- drm(y ~ x,
           data=data.frame(
             x=time,
             y=dates_calibrated_spd$grid$PrDens[dates_calibrated_spd$grid$calBP >= min(time) & dates_calibrated_spd$grid$calBP <= max(time)]),
           fct=L.3())
View(fit)




AIC(fit, spd_test$fitobject, lin_test$fitobject,  k=2)

dates_calibrated_spd_zoom <-  spd(dates_calibrated, timeRange = c( 40000, 30000))
fit_zoom <- drm(y ~ x,
                data=data.frame(
                  x=time,
                  y=dates_calibrated_spd_zoom$grid$PrDens[dates_calibrated_spd_zoom$grid$calBP >= min(time) & dates_calibrated_spd_zoom$grid$calBP <= max(time)]),
                fct=L.3())

AIC(fit_zoom, spd_test_zoom$fitobject, lin_test_zoom$fitobject, k=2)
