

rm(list = ls())
library(dplyr)
library(randomForest)
library(foreach)
library(parallel)
library(doParallel)
library(MASS)
library(Matrix)
library(openxlsx)
library(stringr)
library(ggplot2)
library(tidyverse)
"%ni%" <- Negate("%in%")

registerDoParallel(detectCores())
laptop <- grepl("/home/andrew", getwd())
desktop <- grepl(":", getwd())
if(laptop){
  setwd("/home/andrew/Dropbox/USDA/Admin/journal_ranking_tool/data")  
}
if(desktop){
  setwd("D:/jrt/JRT")  
}



########################
# compute metrics the current way
dat <- read.csv(file = "cleaned_rankings_data.csv")[,-1]
ERSdat <- dat[dat$ERS == 1,]
# medians
idxnames <- c("repec_impact", "repec_h", "sjr", "sh", "tr_impact", "tr_ef")
econ_median <- apply(ERSdat[ERSdat$econ_flag==1,idxnames], 2, median, na.rm=T)
non_median <- apply(ERSdat[ERSdat$econ_flag==0,idxnames], 2, median, na.rm=T)

# tier
ERSdat$tierscore <- ERSdat$newtier <- NA
for (i in 1:nrow(ERSdat)){
  x <- ERSdat[i,]
  if (all(is.na(x[idxnames]))){
    ERSdat$newtier[i] <- "Unranked"
  } else {
    if (ERSdat$econ_flag[i] == 1){
      ERSdat$tierscore[i] <- mean((x[idxnames] - econ_median)>=0, na.rm=T)
    } else {
      ERSdat$tierscore[i] <- mean((x[idxnames] - non_median)>=0, na.rm=T)
    }    
  }
}
ERSdat$newtier[ERSdat$tierscore>.66 & !is.na(ERSdat$tierscore)] <- "Tier I"
ERSdat$newtier[ERSdat$tierscore<.34 & !is.na(ERSdat$tierscore)] <- "Tier III"
ERSdat$newtier[is.na(ERSdat$newtier)] <- "Tier II"

# which ones move?
movers <- ERSdat[ERSdat$newtier != ERSdat$oldtier & !is.na(ERSdat$oldtier),c("journal", "newtier", "oldtier")]
movers <- movers[with(movers, order(newtier)),]


# plot of missing values
# ERSdat[,c("repec_impact", "repec_h", "sjr", "sh", "tr_impact", "tr_ef")] %>%
#   log %>% scale %>% Matrix %>% image(abs = FALSE, aspect=2, lwd = 0) 
# dev.copy2pdf(file = "missplot.pdf", height = 4, width = 3)


#############################
# imputaton

# the code below will run for several hours on an 8-core machine. 
# it begins by randomly imputing the rankings that are missing, using observed values from that index
# it then iterates between rankings, modelling a given observed ranking as a function of other rankings,
# as well as a dummy matrix indicating whether a metric is missing or not
# after each model, imputaitons are updated
# the model is a random forest
# the procedure runs 8 times, from different starting points
# for each of these, it loops through each of the variables 20 times  
# the result is 8 imputed datasets, which are then averaged
maxtimes <- 20
alldat <- foreach(j = 1:8, .packages = c("randomForest")) %dopar%{
  set.seed(j, kind = "L'Ecuyer-CMRG")
  dati <- datf <- log(dat[, idxnames])
  # initial rough impute
  for (i in 1:ncol(dati)){
    datf[is.na(datf[,i]), i] <- sample(dati[!is.na(dati[,i]), i], sum(is.na(datf[,i])), replace = T)
    datf[,i][datf[,i] == -Inf] <- min(datf[,i][datf[,i] >-Inf])
    dati[,i][dati[,i] == -Inf] <- min(dati[,i][dati[,i] >-Inf])
  }
  mdat <- is.na(dati) # missingness variables
  colnames(mdat) <- paste0("M_", colnames(mdat))
  dati <- cbind(dati, mdat)
  datf <- cbind(datf, mdat)

  rflist <- vector("list", 6)
  for(times in 1:maxtimes){
    print(times)
    for (i in 1:(ncol(dati)/2)){
      y <- dati[!is.na(dati[,i]), i]
      x <- datf[!is.na(dati[,i]), -i]
      rf <- randomForest(x=x, y=y, importance = T, ntree = 100)
      if (times == maxtimes){
        rflist[[i]] <- rf
      }
      xp <- datf[is.na(dati[,i]), -i]
      oldyhat <- datf[is.na(dati[,i]), i]
      yhat <- predict(rf, newdata = xp)
      datf[is.na(dati[,i]), i] <- yhat
    }
  }
  return(list(impdat = datf, rflist = rflist))
}
saveRDS(alldat, file = "imputations_fullset.Rds")
alldat <- readRDS("imputations_fullset.Rds")



# make a figure showing how good the imputations are
plotdat <- foreach(i = 1:8, .combine = rbind) %do% {
  foreach(j = 1:6, .combine = rbind) %do% {
    d <- data.frame(pred = alldat[[i]]$rflist[[j]]$predicted,
                    obs = alldat[[i]]$rflist[[j]]$y,
                    v = idxnames[j],
                    imp = i)
    return(d)
  }
}
plotdat$imp <- as.factor(plotdat$imp)
ggplot(plotdat, aes(x=obs, y=pred, color=imp)) +
  geom_point() + facet_wrap(~v, nrow = 2, scales = "free")+geom_abline(intercept = 0,slope = 1)


# combine the datasets
impdat <- foreach(i = 1:8, .combine = "+") %do% {
  as.matrix(alldat[[i]]$impdat[,1:6])
}
impdat <- (impdat/8) %>% as.data.frame
head(impdat)

# PCA
pc <- prcomp(impdat, scale. = T, center = T)
plot(pc)
summary(pc)
par(mfrow = c(3,2))
for (i in 1:6){
  plot(impdat[,i], pc$x[,1], main = colnames(impdat)[i])
  abline(lm(pc$x[,1]~impdat[,i]), lty = 2, col = 'red')
}
for (i in 1:6){
  plot(impdat[,i], pc$x[,2], main = colnames(impdat)[i])
  abline(lm(pc$x[,2]~impdat[,i]), lty = 2, col = 'red')
}

# scores for all journals
all_rankings <- data.frame(journal = dat$journal, impdat)
dat$pc_score <- all_rankings$pc_score <- pc$x[,1]

# a few of them have duplicate titles.  remove them when they are ERS journals, but leave them in where they are not
dupes <- dat$journal[duplicated(dat$journal)] %>% unique
torm <- which(dat$journal %in% dupes & dat$ERS == 0)
dat <- dat[1:nrow(dat) %ni% torm,]
all_rankings <- all_rankings[1:nrow(all_rankings) %ni% torm,]


# identify econ and non-econ tiers
terciles_non <- dat$pc_score[dat$ERS == TRUE & !is.na(dat$econ_flag) & dat$econ_flag == 0] %>% quantile(probs = c(.33, .66)) %>% rev
terciles_econ <- dat$pc_score[dat$ERS == TRUE & !is.na(dat$econ_flag) & dat$econ_flag == 1] %>% quantile(probs = c(.33, .66)) %>% rev

dat$PC_tier <- NA
dat$PC_tier[dat$ERS == TRUE & dat$econ_flag == 1 & dat$pc_score> terciles_econ[1]] <- "Tier III"
dat$PC_tier[dat$ERS == TRUE & dat$econ_flag == 1 & dat$pc_score< terciles_econ[2]] <- "Tier I"
dat$PC_tier[dat$ERS == TRUE & dat$econ_flag == 1 & dat$pc_score> terciles_econ[2] & dat$pc_score< terciles_econ[1]] <- "Tier II"

dat$PC_tier[dat$ERS == TRUE & dat$econ_flag == 0 & dat$pc_score> terciles_non[1]] <- "Tier III"
dat$PC_tier[dat$ERS == TRUE & dat$econ_flag == 0 & dat$pc_score< terciles_non[2]] <- "Tier I"
dat$PC_tier[dat$ERS == TRUE & dat$econ_flag == 0 & dat$pc_score> terciles_non[2] & dat$pc_score< terciles_non[1]] <- "Tier II"

dat$econ_tier <- dat$nonecon_tier <- NA
dat$econ_tier[dat$ERS == FALSE & dat$pc_score> terciles_econ[1]] <- "Tier III"
dat$econ_tier[dat$ERS == FALSE & dat$pc_score< terciles_econ[2]] <- "Tier I"
dat$econ_tier[dat$ERS == FALSE & dat$pc_score> terciles_econ[2] & dat$pc_score< terciles_econ[1]] <- "Tier II"

dat$nonecon_tier[dat$ERS == FALSE & dat$pc_score> terciles_non[1]] <- "Tier III"
dat$nonecon_tier[dat$ERS == FALSE & dat$pc_score< terciles_non[2]] <- "Tier I"
dat$nonecon_tier[dat$ERS == FALSE & dat$pc_score> terciles_non[2] & dat$pc_score< terciles_econ[1]] <- "Tier II"

dat$pc_score <- dat$pc_score * -1

# save output
write.csv(dat, file = "journals_ranked_and_tiered.csv")
write.csv(all_rankings, file = "imputed_data.csv")




