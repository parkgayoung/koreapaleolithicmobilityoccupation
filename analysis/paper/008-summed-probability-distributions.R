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
  filter(age < 50000, method == 'AMS')

# devtools::install_github('ahb108/rcarbon')
library(rcarbon)
dates_calibrated <-
  calibrate(x = dates_clean$age,
            errors = dates_clean$error,
            calCurves = 'intcal13')

dates_calibrated_spd <-
  spd(dates_calibrated,
      timeRange = c(45000, 1000))
plot(dates_calibrated_spd)

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
# rank models with all ages

### this code takes long time
exp_test <-
  modelTest(
    spd_dates,
    errors = dates_clean$error,
    timeRange = c(45000, 1000),
    model = 'exponential',
    nsim = 200,
    ncores = 3
  )

plot(exp_test, main="Exponential Model")

uni_test <-
  modelTest(
    spd_dates,
    errors = dates_clean$error,
    timeRange = c(45000, 1000),
    model = 'uniform',
    nsim = 200,
    ncores = 3
  )

plot(uni_test, main="Uniform Model")

lin_test <-
  modelTest(
    spd_dates,
    errors = dates_clean$error,
    timeRange = c(45000, 1000),
    model = 'linear',
    nsim = 200,
    ncores = 3
  )
plot(lin_test, main="Linear Model")

## plot for all three models
#library(cowplot)
#plot_grid(
#          ncol = 1)

png(here::here("analysis/figures/008-summed-probability-distribution-models.png"))

par(mfrow=c(3,1))
plot(exp_test, main="Exponential Model")
plot(uni_test, main="Uniform Model")
plot(lin_test, main="Linear Model")


dev.off()

#ggsave(here::here("analysis/figures/008-summed-probability-distribution-models.png"))

## rank models with subset of ages

library(drc)

time <- seq(45000, 1000, -1) # calBP

fit <- drm (y ~ x,
  data = data.frame (x = time,
                    y = dates_calibrated_spd$grid$PrDens[dates_calibrated_spd$grid$calBP >= min(time) &
                                                        dates_calibrated_spd$grid$calBP <= max(time)]),
  fct = L.3())

View(fit)

# smallest AIC value indicates the best model
aic <- AIC(fit,
    exp_test$fitobject,
    lin_test$fitobject,
    k = 2)

#                 df       AIC
#fit                4 -423672.2
#exp.fit$fitobject  3 -412132.5
#lin.fit$fitobject  3 -414265.5

# Exponential model provides most parsimonious fit for 45000 - 1000 cal BP (deltaAIC = 0).

# create a table

df_aic <- data.frame (Model = c("fit", "exponential", "linear"),
                     df = c(aic$df),
                     AIC = c(aic$AIC)) %>% arrange(-AIC)
print(df_aic)

#save a table of AIC result as csv
write.csv(df_aic, file="analysis/figures/008-SPD-AIC.csv", row.names = FALSE)

knitr::kable(df_aic)

#save the table as image file
#library(gtable)

#g <- tableGrob(df_aic[1:3, 1:3], rows = NULL)
#g <- gtable_add_grob(g,
#                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
#                     t = 2, b = nrow(g), l = 1, r = ncol(g))
#g <- gtable_add_grob(g,
#                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
#                     t = 1, l = 1, r = ncol(g))
#grid.draw(g)


#png(here::here("analysis/figures/008-SPD-AIC.png"))
#grid.draw(g)
#dev.off()



#-------------------------------------------------
#-------------------------------------------------

##### zoom in on the 40-30ka period ####
#dates_calibrated_spd_zoom <-
#  spd(dates_calibrated,
#      timeRange = c(40000, 30000))

#spd_test_zoom <-
#  modelTest(
#    spd_dates,
#    errors = dates_clean$error,
#    timeRange = c(40000, 30000),
#    model = 'exponential',
#    nsim = 200,
#    ncores = 3 )

#summary(spd_test_zoom)
#plot(spd_test_zoom)

#uni_test_zoom <-
#  modelTest(
#    spd_dates,
#    errors = dates_clean$error,
#    timeRange = c(40000, 30000),
#    model = 'uniform',
#    nsim = 200,
#    ncores = 3)

#plot(uni_test)

#lin_test_zoom <-
#  modelTest(
#    spd_dates,
#    errors = dates_clean$error,
#    timeRange = c(40000, 30000),
#    model = 'linear',
#    nsim = 200,
#    ncores = 3)

#plot(lin_test)

#time <- seq(40000, 30000, -1)

#fit_zoom <- drm(
#  y ~ x,
#  data = data.frame(x = time,
#                    y = dates_calibrated_spd_zoom$grid$PrDens[dates_calibrated_spd_zoom$grid$calBP >= min(time) &
#                                                                dates_calibrated_spd_zoom$grid$calBP <= max(time)]),
#  fct = L.3()
#)

#AIC(fit_zoom,
#    spd_test_zoom$fitobject,
#    lin_test_zoom$fitobject,
#    k = 2)
