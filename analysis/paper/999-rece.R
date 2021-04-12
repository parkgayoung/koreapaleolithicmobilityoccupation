suppressPackageStartupMessages(library(tidyverse))
library(glue)
library(ggrepel)
library(here)

# from https://github.com/wccarleton/megafauna-na/wiki/Replication
library(nimble)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(abind)
library(clam)
library(tibble)

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


# calibrate...
ndates <- dim(dates_clean)[1]
c14post_k <- lapply(1:ndates,
                          function(x){
                            capture.output(d <- calibrate(dates_clean[x,"age"],
                                                          dates_clean[x,"error"],
                                                          graph=F)$calib)
                            return(d)
                          })

# create Radiocarbon-dated Event Count Ensembles (RECEs)....

date_range_k <- range(
  unlist(
    lapply(
      c14post_k,
      function(x){
        range(x[,1])
      }
    )
  )
)

# approximation function that could conceivably resample (interpolate) any
# radiocarbon date density onto any grid, but in these examples
# the resolution of the grid (and the RECE) are the same
# as the resolution of the calibrated dates (i.e., annual).

calSampleApprox <- function(x,t1,t2,r){
  n <- length(x)
  funs <- lapply(x,approxfun)
  y_list <- lapply(1:n,function(j)funs[[j]](seq(t1,t2,r)))
  y_mat <- do.call(cbind,y_list)
  y_mat[which(is.na(y_mat))] <- 0
  return(y_mat)
}

nsamps <- 50
resolution <- 1

c14_matrix_k <- calSampleApprox(
  c14post_k,
  date_range_k[1],
  date_range_k[2],
  resolution
)


# build a data-frame to contain the RECE where each row corresponds
# to a single year in the grid and each column is going to be a single
# probable event-count sequence.

Dates_k <- seq(
  date_range_k[1],
  date_range_k[2],
  resolution
)
rece_sample_k <- data.frame(Date=Dates_k)



# iterate through a loop to build the RECE. Each iteration, we will
# sample each event in the c14_matrix exactly once. The sampling is
# random and weighted by the calibrated date density—so more likely
#event dates will be sampled more frequently. Then, for each sample
# of event dates, we count the number of events occurring in each of
#the grid bins (i.e., every year between the earliest and latest dates
# covered by all of the calibrated distributions).

for(a in 1:nsamps){
  count_sample <- apply(
    c14_matrix_k,
    2,
    function(x){
      sample(
        Dates_k,
        size=1,
        prob=x
      )
    }
  )
  count_df <- as.data.frame(table(count_sample))
  names(count_df) <- c("Date","Count")
  rece_sample_k <- merge(
    rece_sample_k,
    count_df,
    by="Date",
    all=T
  )
}

# These matrices need to be cleaned up a bit before we can use them, though.
# We need to set any NAs to zero (they reflect instances where a date was
# outside the sampled range of a given calibrated radiocarbon date density
# but inside the span of a given RECE—these should be effectively 0
# because the relevant densities would be extremely low for those dates).
# We can also relabel the columns with clearer names, and reverse the
# vertical order of the RECE so that time increases downward. This
# tidying up is coded like this:

rece_sample_k <- as.matrix(rece_sample_k[,-1])
rece_sample_k[which(is.na(rece_sample_k))] <- 0
colnames(rece_sample_k) <- 1:nsamps
rece_sample_k <- as.data.frame(cbind(Dates_k,rece_sample_k))
rece_sample_k <- rece_sample_k[with(rece_sample_k,order(-Dates_k)),]
names(rece_sample_k)[1] <- "Date"


# plotting

recelong <- gather(rece_sample_k,
                   key="Sample",
                   value="Count",
                   paste(1:nsamps))

ggplot(data=recelong) +
  geom_col(mapping=aes(y=Count,
                       x=Date),
           position="identity",
           alpha=0.25,
           colour=NA) +
  #xlim(c(20000,10000)) +
  labs(y="Count",x="Year BP") +
  theme_minimal()

# second slightly more involved way to plot a RECE involves a
# heatmap and an agreement index. The agreement index will indicate
# the number of RECEs that have the same event-count for a given
# time stamp. If we create a matrix to contain the values
# (agreement index for every count–time pairing) then we can use a
# heatmap plot design to help visualize the RECE with greater
# clarity than could be achieved with the simpler transparency method.


binaryRECE <- function(x,n){
  xlen <- length(x)
  newMatrix <- matrix(0,ncol=n,nrow=xlen)
  for(j in 1:xlen){
    if(x[j] > 0){
      newMatrix[j,x[j]] <- 1
    }
  }
  return(newMatrix)
}

rece_mat_list <- lapply(2:dim(rece_sample_k)[2],function(k){
  binaryRECE(rece_sample_k[,k],n=10)
})

rece_heat_array <- abind(rece_mat_list,along=3)

rece_heat_mat <- apply(rece_heat_array,c(1,2),sum)
rece_heat_df <- as.data.frame(cbind(rece_sample_k[,1],rece_heat_mat))
names(rece_heat_df) <- c("YBP",as.character(1:10))
rece_heat_long <- gather(rece_heat_df,
                         key="EventCount",
                         value="NMembers",
                         as.character(1:10))

rece_heat_long$EventCount <- as.numeric(rece_heat_long$EventCount)
rece_heat_tibble <- as_tibble(rece_heat_long)
rece_heat_tibble_sub <- subset(rece_heat_tibble,YBP <= 50000 & YBP >= 10000)

ggplot(data=rece_heat_tibble_sub) +
  geom_raster(data=rece_heat_tibble_sub,
              mapping=aes(x=YBP, y=EventCount, fill=NMembers),
              vjust=0) +
  scale_fill_viridis_c(
    option="B",
    na.value=rgb(0,0,0,0),
    begin=0.15,
    alpha=0.9,
    trans="log"
  ) +
  scale_y_continuous(breaks=c(1:10)) +
  coord_cartesian(ylim=c(0,5)) +
  xlim(c(50000,10000)) +
  labs(y="Count",x="Year BP") +
  theme_minimal()


### bring in climate data
ngrip <- readxl::read_excel("analysis/data/raw_data/NGRIP-d18O.xlsx")
names(ngrip) <- c("Depth","d18O","Dust","GICC05","MCE")

range_x <- range(c(range(ngrip$d18O)-(2*0.01),range(ngrip$d18O)+(2*0.01)))
x_grid <- seq(range_x[1],range_x[2],0.001)

dates <- cbind(ngrip$GICC05 - ngrip$MCE,
               ngrip$GICC05 + ngrip$MCE)
t_grid <- seq(10001,50000,100)
ntimes <- length(t_grid)
t_matrix <- t(apply(dates,1,function(x)dunif(x=t_grid,min=x[1],max=x[2])))
xs <- lapply(ngrip$d18O,function(x)dnorm(x=x_grid,mean=x,sd=0.01))
x_matrix <- do.call(rbind,xs)
Z <- dim(x_matrix)[1]
X <- matrix(nrow=dim(t_matrix)[2],ncol=dim(x_matrix)[2])

# some problem here
for(j in 1:ntimes){
  x_t <- lapply(1:Z,function(z,j)0.05*x_matrix[z,]*t_matrix[z,j],j=j)
  x_t <- do.call(rbind,x_t)
  X[j,] <- colSums(x_t)/(sum(0.05 * t_matrix[,j]))
}

ngrip_ensemble <- as.data.frame(
  do.call(
    cbind,
    lapply(1:100,
           function(j){
             apply(
               X,
               1,
               function(x){
                 sample(x_grid,
                        size=1,
                        prob=x
                 )
               }
             )
           }
    )
  )
)

ngrip_ensemble$B2K <- t_grid
ngrip_ensemble$BP <- ngrip_ensemble$B2K - 50
