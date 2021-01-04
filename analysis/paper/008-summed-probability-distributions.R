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

#make a string with the model stats for each plot title : basic figure
par(mar=c(2,2,2,2))
par(mfrow=c(4,1))

plot(exp_test, main = paste0("Exponential Model (df = ", df_exp, ", AIC = ", aic_exp,")"))
plot(uni_test, main = "Uniform Model")
plot(lin_test, main = paste0("Linear Model (df = ", df_lin, ", AIC = ", aic_lin,")"))
plot(log_test, main = paste0("Logistic Model (df = ", df_log, ", AIC = ", aic_log,")"))

png(here::here("analysis/figures/008-summed-probability-distribution-models.png"),
    h = 5,
    w = 4,
    units = "px")
#ggsave(here::here("analysis/figures/008-summed-probability-distribution-models.png"))

dev.off()

#


# Codes from https://zenodo.org/record/4322979#.X-4bH-lKgWo
# fig 2
# layout(matrix(c(1,2,3,4,5,6,7), 7, 1, byrow=TRUE), widths=6, heights=c(1,1,1,1,1,1,1.8))
# par(mar=c(0, 1, 0, 1))
# par(yaxs="i")
# par(xaxs="i")
#
ymax <- max(ko.spd.smoothed$grid$PrDens)

plot(exp_test, ylim=c(0,ymax), xlim=c(50000,10000), drawaxes=FALSE )
legend(x=49000, ymax*0.94, legend=c("SPD", "95% MC envelope","positive deviation","negative deviation"),col=c( "black","lightgrey",rgb(0.7,0,0,0.2),rgb(0,0,0.7,0.2)), lwd=c(0.5,5,5,5), cex=0.6, bg="white", title="")
text(x=48000, ymax*0.90, labels="Exponential Model", font=2, cex=0.6, adj=c(0,0.7))
text(x=48000, ymax*0.54, cex=0.5, font=2, adj=c(0,0),labels=paste0("(df = ", df_exp, ", AIC = ", aic_exp,")"))
box()
xticks <- seq(50000,10000,-10000)
axis(side=1, at=xticks, labels=xticks, cex.axis=0.8)
mtext("Years cal BP",1, 2.3, at=34000, adj=0, font=2, cex=0.8)

dev.off()



plot(exp_test, main = paste0("Exponential Model (df = ", df_exp, ", AIC = ", aic_exp,")"))
plot(uni_test, main = "Uniform Model")
plot(lin_test, main = paste0("Linear Model (df = ", df_lin, ", AIC = ", aic_lin,")"))
plot(log_test, main = paste0("Logistic Model (df = ", df_log, ", AIC = ", aic_log,")"))


# lines(expnull$fit$calBP,expnull$fit$PrDens, col="black", lty="dashed", lwd=0.5)
# abline(v=seq(14000,2500,-500), lty="dotted", col="white")
# legend(x=13800, ymax*0.94, legend=c("SPD (dates not normalised)","Exponential Model", "95% MC envelope","positive deviation","negative deviation"),col=c(1, "black","lightgrey",rgb(0.7,0,0,0.2),rgb(0,0,0.7,0.2)), lty=c(1,2,1,1,1), lwd=c(0.5,0.5,5,5,5), cex=0.6, bg="white", title="")
# text(x=13700, ymax*0.92, labels="d. Exponential Fit", font=2, cex=0.8, adj=c(0,0.7))
# text(x=13400, ymax*0.52, cex=0.7, adj=c(0,0.7), labels=substitute(paste(italic(p), "=", x, sep=""), list(x=round(expnull$pval,4))))
# box()
# xticks <- seq(14000,2450,-500)
# axis(side=1, at=xticks, labels=xticks, las=2, cex.axis=0.8)# add BP axis
#
# axis(side=1, at=xticks-50, labels=xticks-2000, las=2, cex.axis=0.8,pos=-0.35)# add BC/AD axis
# mtext("BC",1, 5.3, at=13725, adj=0, font=2, cex=0.5, las=2)
# dev.off()
#

# plot(perm, focalm="1", xlim=c(14000,2400),col.obs="brown", lwd.obs=1, drawaxes=FALSE)
# abline(v=seq(13500,3000,-500), lty="dotted", col="white")
# legend(x=13800, y=ymax*0.90,legend=c("SPD","95% MC envelope","positive deviation","negative deviation"),col=c(1,"lightgrey",rgb(0.7,0,0,0.2),rgb(0,0,0.7,0.2)),lty=c(1,1,1,1),lwd=c(0.5,5,5,5),cex=0.6, bg="white", title=expression(bold("a. Anatolia")))
# text(x=13800, y=ymax*0.42, labels=paste("n=",nrow(datesp[datesp$Region==1,]),", sites=",length(unique(datesp$SiteName[datesp$Region==1])),", bins=",length(unique(bins[datesp$Region==1])),sep=""), font=1, cex=0.6, adj=c(0,0.7))
# text(x=13800, y=ymax*0.35, cex=0.6, adj=c(0,0.7), labels=substitute(paste(italic(p), "=", x, sep=""), list(x=round(min(perm$pValueList["1"],1-perm$pValueList["1"]),3))))
# text(x=12500, y=ymax*0.14, labels="Epipalaeolithic",lty=c("solid"), col=c("brown"), font=2, cex=0.8, adj=c(0.5,-0.5))
# segments(10300, ymax*0.15,10300,ymax*0.02, lty="solid", col="brown", lwd=(1.2))
# text(x=9000, ymax*0.01, labels="Neolithic",lty=c("solid"), col=c("brown"), font=2, cex=0.8, adj=c(0.5,-0.5))
# segments(8000, ymax*0.15,8000,ymax*0.02, lty="solid", col="brown", lwd=(1.2))
# text(x=6500, ymax*0.01, labels="Chalcolithic",lty=c("solid"), col=c("brown"), font=2, cex=0.8, adj=c(0.5,-0.5))
# segments(5000, ymax*0.15,5000,ymax*0.01, lty="solid", col="brown", lwd=(1.2))
# segments(4000, ymax*0.12,4000,ymax*0.01, lty="dotted", col="brown", lwd=(1.2))
# segments(3400, ymax*0.12,3400,ymax*0.01, lty="dotted", col="brown", lwd=(1.2))
# segments(3100, ymax*0.15,3100,ymax*0.02, lty="solid", col="brown", lwd=(1.2))
# text(x=4000, ymax*0.13, labels="Bronze Age",lty=c("solid"), col=c("brown"), font=2, cex=0.9, adj=c(0.5,-0.5))
# text(x=4500, ymax*0.01, labels="EBA",lty=c("solid"), col=c("brown"), font=2, cex=0.6, adj=c(0.5,-0.5))
# text(x=3750, ymax*0.01, labels="MBA",lty=c("solid"), col=c("brown"), font=2, cex=0.5, adj=c(0.5,-0.5))
# text(x=3250, ymax*0.01, labels="LBA",lty=c("solid"), col=c("brown"), font=2, cex=0.5, adj=c(0.5,-0.5))
# text(x=2850, 0, labels="Iron\nAge",lty=c("solid"), col=c("brown"), font=2, cex=0.8, adj=c(0.2,-0.2))
# box()
