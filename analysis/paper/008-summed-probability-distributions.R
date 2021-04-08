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
nsim = 10000 # set to 10 while developing, then to 10000 for the final run

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
                     AIC = c(aic$AIC)) %>%
  arrange(-AIC)
# Model df       AIC
#  logistic  4     -363176.8
#  exponential  3  -368376.7
#  linear  3       -368425.3


## plots for all three models.

df_exp <-  df_aic %>% filter(Model == 'exponential') %>% pull(df)
aic_exp <- df_aic %>% filter(Model == 'exponential') %>% pull(AIC) # -368376.7

df_log <-  df_aic %>% filter(Model == 'logistic') %>% pull(df) # 4
aic_log <- df_aic %>% filter(Model == 'logistic') %>% pull(AIC) # -363176.8

df_lin <-  df_aic %>% filter(Model == 'linear') %>% pull(df) # 3
aic_lin <- df_aic %>% filter(Model == 'linear') %>% pull(AIC) # -368425.3

#make a string with the model stats for each plot title : basic figure
# par(mar=c(2,2,2,2))
# par(mfrow=c(4,1))
#
# plot(exp_test, main = paste0("Exponential Model (df = ", df_exp, ", AIC = ", aic_exp,")"))
# plot(uni_test, main = "Uniform Model")
# plot(lin_test, main = paste0("Linear Model (df = ", df_lin, ", AIC = ", aic_lin,")"))
# plot(log_test, main = paste0("Logistic Model (df = ", df_log, ", AIC = ", aic_log,")"))



#move the title inside the plot area
# Codes from https://zenodo.org/record/4322979#.X-4bH-lKgWo
ymax <- max(ko.spd.smoothed$grid$PrDens)

# remove the x-axis



# define a function that emits the desired plot
p1 <- function() {

# begin drawing the plot
layout(
  matrix(c(1, 2, 3, 4), 4, 1, byrow = TRUE),
  widths = 6,
  heights = c(1.8, 1.8, 1.8, 2.6)
)

# first plot: linear
par(mar = c(0, 1, 1, 1)) #c(bottom, left, top, right)

plot(
  lin_test,
  ylim = c(0, ymax),
  xlim = c(50000, 10000),
  drawaxes = FALSE
)
legend(
  x = 49000,
  ymax * 0.94,
  legend = c(
    "radiocarbon ages",
    "95% MC envelope",
    "positive deviation",
    "negative deviation"
  ),
  col = c("black", "lightgrey", rgb(0.7, 0, 0, 0.2), rgb(0, 0, 0.7, 0.2)),
  lwd = c(0.5, 5, 5, 5),
  cex = 0.6,
  bg = "white",
  title = ""
)
text(
  x = 48000,
  ymax * 0.90,
  labels = "Linear Model",
  font = 2,
  cex = 0.6,
  adj = c(0, 0.7)
)
text(
  x = 41000,
  ymax * 0.90,
  cex = 0.6,
  font = 2,
  adj = c(0, 0),
  labels = paste0("AIC = ", round(aic_lin, digits = 0))
)


box()

# Second plot: exponential
par(mar = c(0, 1, 0, 1))

plot(
  exp_test,
  ylim = c(0, ymax),
  xlim = c(50000, 10000),
  drawaxes = FALSE
)
# draw Exp plot: legend
legend(
  x = 49000,
  ymax * 0.94,
  legend = c(
    "radiocarbon ages",
    "95% MC envelope",
    "positive deviation",
    "negative deviation"
  ),
  col = c("black", "lightgrey", rgb(0.7, 0, 0, 0.2), rgb(0, 0, 0.7, 0.2)),
  lwd = c(0.5, 5, 5, 5),
  cex = 0.6,
  bg = "white",
  title = ""
)
# draw Exp plot: title
text(
  x = 48000,
  ymax * 0.90,
  labels = "Exponential Model",
  font = 2,
  cex = 0.6,
  adj = c(0, 0.7)
)
# draw Exp plot: model stats
text(
  x = 41000,
  ymax * 0.90,
  cex = 0.6,
  font = 2,
  adj = c(0, 0),
  labels = paste0("AIC = ", round(aic_exp, digits = 0))
)



box()

# Third plot: logistic
par(mar = c(0, 1, 0, 1))

plot(
  log_test,
  ylim = c(0, ymax),
  xlim = c(50000, 10000),
  drawaxes = FALSE
)

legend(
  x = 49000,
  ymax * 0.94,
  legend = c(
    "radiocarbon ages",
    "95% MC envelope",
    "positive deviation",
    "negative deviation"
  ),
  col = c("black", "lightgrey", rgb(0.7, 0, 0, 0.2), rgb(0, 0, 0.7, 0.2)),
  lwd = c(0.5, 5, 5, 5),
  cex = 0.6,
  bg = "white",
  title = ""
)


text(
  x = 48000,
  ymax * 0.90,
  labels = "Logistic Model",
  font = 2,
  cex = 0.6,
  adj = c(0, 0.7)
)
text(
  x = 41000,
  ymax * 0.90,
  cex = 0.6,
  font = 2,
  adj = c(0, 0),
  labels = paste0("AIC = ", round(aic_log, digits = 0))
)

box()


#forth plot: uniform
par(mar = c(6, 1, 0, 1))

plot(
  uni_test,
  ylim = c(0, ymax),
  xlim = c(50000, 10000),
  drawaxes = FALSE
)
# draw Uni plot: legend
legend(
  x = 49000,
  ymax * 0.94,
  legend = c(
    "radiocarbon ages",
    "95% MC envelope",
    "positive deviation",
    "negative deviation"
  ),
  col = c("black", "lightgrey", rgb(0.7, 0, 0, 0.2), rgb(0, 0, 0.7, 0.2)),
  lwd = c(0.5, 5, 5, 5),
  cex = 0.6,
  bg = "white",
  title = ""
)
# draw Uni plot: title
text(
  x = 48000,
  ymax * 0.90,
  labels = "Uniform Model",
  font = 2,
  cex = 0.6,
  adj = c(0, 0.7)
)
box()



xticks <- seq(50000, 10000, -10000)
axis(
  side = 1,
  at = xticks,
  labels = xticks,
  cex.axis = 0.8
)
mtext(
  "Years cal BP",
  1,
  2.3,
  at = 34000,
  adj = 0,
  font = 2,
  cex = 0.8
)
}

ggdraw(p1)

ggsave(here::here("analysis/figures/008-summed-probability-distribution-models.png"),
       h = 15,
       w = 13,
       dpi = 1000,
       units = "cm")

# # start the file that will hold the plot, then draw the plot
# # after this...
# png(here::here("analysis/figures/008-summed-probability-distribution-models.png"),
#     h = 15,
#     w = 13,
#     res = 1000,
#     units = "cm")
#
# p1
#
# # now close the connection to the PNG file so we can take a look at it
# dev.off()

# the figure is 008-summed-probability-distribution-models.png


#save linear model data for another figure with MAT
save(lin_test, aic_lin, file = "spd_lin.RData")
# To load the data again
load("spd_lin.RData")



