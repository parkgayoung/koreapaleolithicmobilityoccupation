suppressPackageStartupMessages(library(tidyverse))
library(glue)
library(ggrepel)
library(here)

dates <-
  read.csv(here("analysis/data/raw_data/Radiocarbon dates.csv"))

dates_clean <-
  dates %>%
  mutate(age = parse_number(as.character(age)),
         error = parse_number(as.character(error))) %>%
  # filter age range here
  filter(age < 50500,
         age >= 10000,
         method == 'AMS')

# devtools::install_github('ahb108/rcarbon')
library(rcarbon)
dates_calibrated <-
  calibrate(x = dates_clean$age,
            errors = dates_clean$error,
            calCurves = 'intcal20')

dates_calibrated_spd <-
  spd(dates_calibrated,
      timeRange = c(50000, 10000))
# plot(dates_calibrated_spd)

# Testing Observed SPDs against theoretical models
## recalibrate dates without normalisation to avoid artificial peaks

spd_dates <-
  calibrate(
    x = dates_clean$age,
    errors = dates_clean$error,
    normalised = FALSE,
    verbose = F
  )

#-------------------------------------------------
### this section of code takes long time, fyi
nsim = 200

# exponential model
exp_test <-
  modelTest(
    spd_dates,
    errors = dates_clean$error,
    timeRange = c(50000, 10000),
    model = 'exponential',
    nsim = nsim,
    ncores = 3
  )

# uniform model, does not produce anything for $fitobject
uni_test <-
  modelTest(
    spd_dates,
    errors = dates_clean$error,
    timeRange = c(50000, 10000),
    model = 'uniform',
    nsim = nsim,
    ncores = 3
  )


# linear model
lin_test <-
  modelTest(
    spd_dates,
    errors = dates_clean$error,
    timeRange = c(50000, 10000),
    model = 'linear',
    nsim = nsim,
    ncores = 3
  )


# logistical model

# rcarbon pkg version of logistical growth model, accprding to instructions here
# https://cran.r-project.org/web/packages/rcarbon/vignettes/rcarbon.html#testing-against-custom-growth-models

ko.bins = binPrep(sites=dates_clean$ID,
                  ages=dates_clean$age,
                  h=100)

# Generate a smoothed SPD
ko.spd.smoothed = spd(spd_dates,
                      timeRange=c(50000, 10000),
                      bins=ko.bins,
                      runm=100)

# Start values should be adjusted depending on the observed SPD
logFit <- nls(PrDens ~ SSlogis(calBP, Asym, xmid, scale),
              data = ko.spd.smoothed$grid,
              control = nls.control(maxiter = 200),
              start = list(Asym = 0.2, xmid = 30000, scale = -100))

# Generate a data frame containing the fitted values
logFitDens <- data.frame(calBP=ko.spd.smoothed$grid$calBP,
                         PrDens=SSlogis(input=ko.spd.smoothed$grid$calBP,
                                        Asym=coefficients(logFit)[1],
                                        xmid=coefficients(logFit)[2],
                                        scal=coefficients(logFit)[3]))

# Use the modelTest function
log_test  <- modelTest(spd_dates,
                     errors = dates_clean$error,
                     bins = ko.bins,
                     nsim = nsim,
                     timeRange = c(50000, 10000),
                     model = "custom",
                     predgrid = logFitDens,
                     runm = 100,
                     raw = TRUE)


# look at p-values for the models
exp_test$pval
uni_test$pval
lin_test$pval
log_test$pval
# they are all the same!

## rank models by AIC score with subset of ages

library(drc)
time <- seq(50000, 10000, -1) # calBP

# logistical growth model
logfit <- drm (y ~ x,
  data = data.frame (x = time,
                    y = dates_calibrated_spd$grid$PrDens[dates_calibrated_spd$grid$calBP >= min(time) &
                                                        dates_calibrated_spd$grid$calBP <= max(time)]),
  fct = L.3())


# smallest AIC value indicates the best model
aic <- AIC(
  logfit,
  exp_test$fitobject,
  lin_test$fitobject,
 # uni_test$fitobject,
  k = 2)

# create a table

df_aic <- data.frame (Model = c("logistic",
                                "exponential",
                                "linear"
                               # "uniform"
                                ),
                     df = c(aic$df),
                     AIC = c(aic$AIC)) %>% arrange(-AIC)
# Model df       AIC
# logistic  4 -363176.8
#  exponential  3 -368376.7
#     linear  3 -368425.3


## plots for all three models.
df_exp <- 3
aic_exp <- -368376.7

df_log <- 4
aic_log <- -363176.8

df_lin <- 3
aic_lin <- -368425.3

par(mar=c(2,2,1,1))
par(mfrow=c(4,1))

#make a string with the model stats for each plot title
plot(exp_test, main = paste0("Exponential Model (df = ", df_exp, ", AIC = ", aic_exp,")"))
plot(uni_test, main = "Uniform Model")
plot(lin_test, main = paste0("Linear Model (df = ", df_lin, ", AIC = ", aic_lin,")"))
plot(log_test, main = paste0("Logistic Model (df = ", df_log, ", AIC = ", aic_log,")"))

#dev.off()

png(here::here("analysis/figures/008-summed-probability-distribution-models.png"))
#ggsave(here::here("analysis/figures/008-summed-probability-distribution-models.png"))


#save a table of AIC result as csv
# write.csv(df_aic, file="analysis/figures/008-SPD-AIC.csv", row.names = FALSE)

# knitr::kable(df_aic)

