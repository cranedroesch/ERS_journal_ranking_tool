

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

registerDoParallel(detectCores())
laptop <- grepl("/home/andrew", getwd())
desktop <- grepl(":", getwd())
if(laptop){
  setwd("/home/andrew/Dropbox/USDA/Admin/journal_ranking_tool/data")  
}
if(desktop){
  setwd("D:/jrt/JRT")  
}

# define convenience functions
losethe <- function(x){
  x[which(substring(x, 1,4) == "the ")] <- gsub("the ", "", x[which(substring(x, 1,4) == "the ")])
  return(x)
}
mse <- function(x, y){mean((x-y)^2)}

"%ni%" <- Negate("%in%")

spelling <- function(x){
  x <- gsub("jounral", "journal", x)
  x <- gsub("journcal", "journal", x)
  x <- gsub("j.", "journal", x, fixed = TRUE)
  x <- gsub("appliec", "applied", x)
  x <- gsub("&", "and", x)
  x <- gsub("-", "", x)
  x <- gsub("peerdisciplinary", "an interdisciplinary", x)
  x <- gsub(" bak ", " bank ", x)
  x <- gsub(" pespectives ", " perspectives ", x)
  x <- gsub(" developmentl ", " development ", x)
  x <- gsub(".", "", x, fixed = T)
  x <- gsub("agriuclture", "agriculture", x)
  x <- gsub(": a journal of economy and society", "", x) # for IR
  return(x)
}

clean <- function(x){
  x %>% tolower %>% losethe %>% spelling
}


########################
# load, clean, and merge rankings data
# before loading into R, I had to manually remove "   " strings from the spreadsheet.
# R could'n do it with gsub for some reason

## repec
repec <- read.xlsx("updated_rankings.xlsx", sheet = 2)
repec <- repec[-1,c(2,3)] # lose worthless colums
repec$journal <- strsplit(repec$Journal, split = ",") %>%
  sapply(function(x){unlist(x)[1]}) %>%
  clean

repec$repec_impact <- repec$Factor
# start main rankings frame
all_rankings <- repec[, c("journal", "repec_impact")]
rm(repec)


## repec h index
repech <- read.xlsx("updated_rankings.xlsx", sheet = 3)
repech <- repech[-1,c(2,3)] # lose worthless colums
repech$journal <- strsplit(repech$Journal, split = ",") %>%
  sapply(function(x){unlist(x)[1]}) %>%
  clean
repech$repec_h <- repech$Factor

all_rankings %>% dim
all_rankings <- full_join(all_rankings, repech[,c("repec_h", "journal")])
all_rankings %>% dim # added many.  possibly mis-spelled duplicates
rm(repech)

## SciImago
# sci <- read.xlsx("updated_rankings.xlsx", sheet = 4) #these data are from 2013... 
## 2016 scimago
sci <- read.xlsx("scimagojr 2016 Revised.xlsx", sheet = 1)
sci <- sci[,c(2,5, 7)] # lose worthless colums
sci$journal <- clean(sci$Title)
sci$Title <- NULL
colnames(sci)[1:2] <- c("sjr", "sh")
all_rankings <- full_join(all_rankings, sci)

## Thompson-Reuters
tr <- read.xlsx("updated_rankings.xlsx", sheet = 5)
tr <- tr[,c(1,2,4,5)]
tr$journal <- clean(tr$Full.Journal.Title)
colnames(tr)[3:4] <- c("tr_impact", "tr_ef")
tr$Full.Journal.Title <- tr$Rank <- NULL

all_rankings <- full_join(all_rankings, tr)
rm(tr)

head(all_rankings) #duplicates
all_rankings <- all_rankings[!duplicated(all_rankings),]
dim(all_rankings) 

# convert from character -- there are a couple of malformed ones
all_rankings$tr_impact <- as.numeric(all_rankings$tr_impact)
all_rankings$tr_ef <- as.numeric(all_rankings$tr_ef)

########################
# load/clean list of journals in which ERS has published

## recent year
ersj <- read.xlsx("ERS Journal Articles 2009-2017.xlsx", sheet = 1)
ersj <- clean(ersj[,1])
ersj <- ersj[!duplicated(ersj)]

## prev years
ersj_old <- read.xlsx("ERS Journal Articles 2009-2017.xlsx", sheet = 2)$Journal
ersj_old <- clean(ersj_old)
ersj_old <- ersj_old[!duplicated(ersj_old)]
ersj_old <- ersj_old[ersj_old %ni% ersj]
ersj <- c(ersj, ersj_old)

ersj_old <- read.xlsx("ERS Journal Articles 2009-2017.xlsx", sheet = 3)$Journal
ersj_old <- clean(ersj_old)
ersj_old <- ersj_old[!duplicated(ersj_old)]
ersj_old <- ersj_old[ersj_old %ni% ersj]
ersj <- c(ersj, ersj_old)

# clean...
fixtitles <- function(ersj){
  ersj <- ersj[!grepl("fy16|2015|white papers|selected paper|unknown", ersj)]
  ersj <- trimws(ersj)
  ersj <- gsub(" (april 2016) vol 50","", ersj, fixed= T)
  ersj <- gsub(" 2014): 320","", ersj, fixed= T)
  ersj[ersj == "agribusiness, an international journal"] <- "agribusiness"
  ersj[ersj == "agribusiness: an international journal"] <- "agribusiness"
  ersj[ersj == "food studies, an international journal"] <- "food studies: an international journal"
  ersj[ersj == "agricultural sciences journal"] <- "agricultural sciences"
  ersj[ersj == "applied economics perspectives and policy"] <- "applied economic perspectives and policy"
  ersj[ersj == "applied economic letters"] <- "applied economics letters"
  ersj[ersj == "climate change"] <- "climatic change"
  ersj[ersj == "current obesity reviews"] <- "current obesity reports"
  ersj[ersj == "economic letters"] <- "economics letters"
  ersj[ersj == "economics and human biology, science direct"] <- "economics and human biology"
  ersj[ersj == "journal of agriculture and applied economics"] <- "journal of agricultural and applied economics"
  ersj[ersj == "journal of applied and agricultural economics"] <- "journal of agricultural and applied economics"
  ersj[ersj == "journal of environment management"] <- "journal of environmental management"
  ersj[ersj == "journal of hunger and environment nutrition"] <- "journal of hunger and environmental nutrition"
  ersj[ersj == "nternational food and agribusiness management review"] <- "international food and agribusiness management review"
  ersj[ersj == "resources and energy economics"] <- "resource and energy economics"
  ersj[ersj == "world developmentl"] <- "world development"
  ersj[ersj == "applied poultry research"] <- "journal of applied poultry research"
  ersj[ersj == "archives of internal medicine"] <- "archives of internal medicine"
  ersj[ersj == "biobased and applied economics"] <- "biobased and applied economics journal"
  ersj[ersj == "cancer epidemiology, biomarker and prevention"] <- "cancer epidemiology, biomarkers and prevention"
  ersj[ersj == "journal of american water resources associations"] <- "journal of the american water resources association"
  ersj[ersj == "journal of association of environmental and resource economists"] <- "journal of the association of environmental and resource economists"
  ersj[ersj == "journal of international environmental agreements: politics, law and economics"] <- "international environmental agreements: politics, law and economics"
  ersj[ersj == "journal or agricultural and resource economics"] <- "journal of agricultural and resource economics"
  ersj[ersj == "journal policy modeling"] <- "journal of policy modeling"
  ersj[ersj == "proceedings of the national academy of sciences"] <- "proceedings of the national academy of sciences of the united states of america"
  ersj[ersj == "risk analysis: an international journal"] <- "risk analysis"
  ersj[ersj == "australian journal of agricultural economics"] <- "australian journal of agricultural and resource economics"
  ersj[ersj == "journal of economics and social measurement"] <- "journal of economic and social measurement"
  ersj[ersj == "social science review"] <- "social service review"
  ersj[ersj == "economic and cultural change"] <- "economic development and cultural change"
  ersj[ersj == "environmental and psychological measurement"] <- "journal of environmental psychology"
  ersj[ersj == "food studies an interdisciplinary journal"] <- "food studies: an interdisciplinary journal"
  ersj[ersj == "journal of family and marriage"] <- "journal of marriage and family"
  ersj[ersj == "journal of international law and trade policy"] <- "journal of international trade law and policy"
  ersj[ersj == "journal of natural resources policy review"] <- "journal of natural resources policy research"
  ersj[ersj == "forum for health and economic policy"] <- "forum for health economics and policy"
  ersj[ersj == "archives of internal medicine"] <- "annals of internal medicine"
  ersj[ersj == "maternal and child health"] <- "maternal and child health journal"
  ersj[ersj == "international journal of time use research"] <- "electronic international journal of time use research"
  ersj[ersj == "population resarch and policy review"] <- "population research and policy review"
  ersj[ersj == "journal of nutrition and education"] <- "journal of nutrition"
  return(ersj)
}
ersj <- sort(fixtitles(ersj))
ersj <- ersj[!duplicated(ersj)]

################################
# merge with rankings

# start with a full join to mop up name problems
full <- full_join(data.frame(journal = ersj, ERS = "ERS"), all_rankings)
# merge with scimago econ list
smec <- read.csv("scimago_econ.csv")[,1:2]
smec$Title <- clean(smec$Title)
colnames(smec)[colnames(smec) == "Title"] <- "journal"
colnames(smec)[colnames(smec) == "Rank"] <- "econ_flag"
smec$econ_flag <- smec$econ_flag^0
initial_flag <- left_join(full, smec)

# remove duplicates
initial_flag$journal[!is.na(initial_flag$ERS)]
initial_flag$note <- NA
initial_flag$note[initial_flag$journal == "health economics"] <- "There is more than one journal with this title, and they are ranked differently.  Find out which one ERS published in"
initial_flag <- initial_flag[initial_flag$journal != "international food and agribusiness management review",] # remove a duplicate 
initial_flag <- initial_flag[initial_flag$journal != "journal of the association of environmental and resource economics",] # remove a duplicate 
initial_flag$note[grepl("regional studies", initial_flag$journal)] <- "many similar titles"
initial_flag <- initial_flag[initial_flag$journal != "sustainability",] # remove a weirdo and put it in blank 
initial_flag <- rbind(initial_flag, c("sustainability", "ERS", NA, NA, NA, NA, NA, NA, NA, NA))
initial_flag$journal[grepl("information systems for biotechnology", initial_flag$journal)] <- "information systems for biotechnology"

# merge with previous tiers, after cleaning them
olddat <- read.csv("jrdat.csv")
colnames(olddat)[1:3] <- c("journal", "n_pubs", "tier")
colnames(olddat) <- gsub(".", "_", colnames(olddat), fixed = T) %>% tolower
olddat$journal <- olddat$journal %>% clean %>% fixtitles
olddat <- olddat[,c("journal", "tier")]
olddat <- olddat[!duplicated(olddat),]

dat <- full_join(olddat, initial_flag)
colnames(dat)[colnames(dat) == "tier"] <- "oldtier"
dat$ERS <- is.na(dat$ERS) == FALSE
write.csv(dat, file = "all_rankings_merged_jun14.csv")


################################
# farm them out for manual QC
# registerDoParallel(detectCores())
# laptop <- grepl("/home/andrew", getwd())
# desktop <- grepl(":", getwd())
# if(laptop){
#   setwd("/home/andrew/Dropbox/USDA/Admin/JRT/")
# }
# if(desktop){
#   setwd("D:/jrt/JRT")
# }
# 
# dat <- read.csv(file = "all_rankings_merged_may30.csv")[,-1]
# people <- c("abe", "andrew", "ephraim", "jeff", "john", "laura", "nick", "tim")
# 
# # figure out which ones already got put out
# already_assigned <- foreach(i = people, .combine = c) %do%{
#   x <- read.csv(paste0("forQC_", i, ".csv"))
#   x$journal %>% as.character
# }
# set.seed(12345, kind = "L'Ecuyer-CMRG")
# not_yet_assigned <- dat$journal[dat$ERS == TRUE & dat$journal %ni% already_assigned] %>% sample
# toadd <- people[(round(runif(1, 0, 10000))+ 1:length(not_yet_assigned)) %%8 +1]
# 
# for (i in people){
#   x <- read.csv(paste0("forQC_", i, ".csv"))
#   old <- x$journal %>% as.character
#   new <- not_yet_assigned[people == i] %>% as.character
#   towrite <- dat[dat$journal %in% c(old, new),]
#   towrite <- towrite[with(towrite, order(journal)),]
#   write.csv(towrite, paste0("forQC_v3_", i, ".csv"))
# }
# 
# 
# jnames <- foreach(i = people, .combine = c) %do%{
#   x <- read.csv(paste0("forQC_v3_", i, ".csv"))
#   old <- x$journal %>% as.character
#   old
# }

########################
# read the QC files back in
fi <- list.files(path = paste0(getwd(), "/QC"), pattern = "input", full.names = T)
QC <- foreach(i = fi, .combine = rbind) %do% {
  d <- read.csv(i)[,2:11]
  return(d)
}
QC <- QC[with(QC, order(journal)),]
QC <- QC[!is.na(QC$econ_flag),]

dat <- dat[dat$ERS == 0,]
dat$note <- NULL
dat <- rbind(QC, dat)
write.csv(dat, "cleaned_rankings_data.csv")


