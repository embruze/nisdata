options("scipen"=100)
library(foreign)
library(Hmisc)
library(ggplot2)
library(ade4)
library(dplyr)
library(directlabels)

nisdf <- read.csv("~/nisImport/nisdf.csv")
nisdf2 <- read.csv("~/nisImport/nisdf2.csv")

# mean.k function
mean.k=function(x) {
  if (is.numeric(x)) round(mean(x, na.rm=TRUE), digits = 2)
  else "N*N"
}

# median.k function
median.k=function(x) {
  if (is.numeric(x)) round(median(x, na.rm=TRUE), digits = 2)
  else "N*N"
}

# sd.k function
sd.k=function(x) {
  if (is.numeric(x)) round(sd(x, na.rm=TRUE), digits = 2)
  else "N*N"
}

# min.k function
min.k=function(x) {
  if (is.numeric(x)) round(min(x, na.rm=TRUE), digits = 2)
  else "N*N"
}

# max.k function
max.k=function(x) {
  if (is.numeric(x)) round(max(x, na.rm=TRUE), digits = 2)
  else "N*N"
}

###########################################################

# sumstats function #

sumstats=function(x) {  # start function sumstats
  sumtable = cbind(as.matrix(colSums(!is.na(x))),
                   sapply(x,mean.k),
                   sapply(x,median.k),
                   sapply(x,sd.k),
                   sapply(x,min.k),
                   sapply(x,max.k))
  sumtable=as.data.frame(sumtable)
  names(sumtable)=c("Obs","Mean","Median","Std.Dev","min","MAX")
  sumtable
}                       # end function sumstats
sapply(nisdf2,class)
summary(nisdf2)
source('sumstats.r')
sumstats(nisdf2)


require(xtable)                   # xtable package helps create LaTeX code from R.
xtable(sumstats(nisdf2))

#Summarize variable 'BF' for women above poverty from 2011 onwards.
summary(subset(nisdf2, YEAR >= 2011 & INCPOV1 == "ABOVE POVERTY", select=BF_ENDR06))

#Create tx val
nisdf$treatstate<- revalue(nisdf2$STATE, c("ALABAMA"= 0,
                                          "ALASKA"= 0,
                                          "ARIZONA"=0,
                                          "ARKANSAS"=0,
                                          "CALIFORNIA"=NA,
                                          "COLORADO"=0,
                                          "CONNECTICUT"=0,
                                          "DELAWARE"=0,
                                          "DISTRICT OF COLUMBIA"=0,
                                          "FLORIDA"=0,
                                          "GEORGIA"=0,
                                          "HAWAII"=0,
                                          "IDAHO"=0,
                                          "ILLINOIS"=0,
                                          "INDIANA"=0,
                                          "IOWA"=0,
                                          "KANSAS"=0,
                                          "KENTUCKY"=0,
                                          "LOUISIANA"=0,
                                          "MAINE"=0,
                                          "MARYLAND"=0,
                                          "MASSACHUSETTS"=0,
                                          "MICHIGAN"=0,
                                          "MINNESOTA"=0,
                                          "MISSISSIPPI"=0,
                                          "MISSOURI"=0,
                                          "MONTANA"=0,
                                          "NEBRASKA"=0,
                                          "NEVADA"=0,
                                          "NEW HAMPSHIRE"=0,
                                          "NEW JERSEY"=1,
                                          "NEW MEXICO"=0,
                                          "NEW YORK"=0,
                                          "NORTH CAROLINA"=0,
                                          "NORTH DAKOTA"=0,
                                          "OHIO"=0,
                                          "OKLAHOMA"=0,
                                          "OREGON"=0,
                                          "PENNSYLVANIA"=0,
                                          "RHODE ISLAND"=0,
                                          "SOUTH CAROLINA"=0,
                                          "SOUTH DAKOTA"=0,
                                          "TENNESSEE"=0,
                                          "TEXAS"=0,
                                          "UTAH"=0,
                                          "VERMONT"=0,
                                          "VIRGINIA"=0,
                                          "WASHINGTON"=0,
                                          "WEST VIRGINIA"=0,
                                          "WYOMING"=0,
                                          "WISCONSIN"=0,
                                          "GUAM"=NA, 
                                          "U.S.  VIRGIN ISLANDS"=NA,
                                          "PUERTO RICO"=NA))


nisdf$treatstate<- nisdf$STATE
#nisdf$treatstate <- revalue(nisdf$treatstate, c("CALIFORNIA"="NA"))
#nisdf$treatstate<- ifelse(nisdf$STATE == "NEW JERSEY", 1, 0)

nisdf2$treatstate<- revalue(nisdf$STATE, c("ALABAMA"= 0,
                                          "ALASKA"= 0,
                                          "ARIZONA"=0,
                                          "ARKANSAS"=0,
                                          "CALIFORNIA"=NA,
                                          "COLORADO"=0,
                                          "CONNECTICUT"=0,
                                          "DELAWARE"=0,
                                          "DISTRICT OF COLUMBIA"=0,
                                          "FLORIDA"=0,
                                          "GEORGIA"=0,
                                          "HAWAII"=0,
                                          "IDAHO"=0,
                                          "ILLINOIS"=0,
                                          "INDIANA"=0,
                                          "IOWA"=0,
                                          "KANSAS"=0,
                                          "KENTUCKY"=0,
                                          "LOUISIANA"=0,
                                          "MAINE"=0,
                                          "MARYLAND"=0,
                                          "MASSACHUSETTS"=0,
                                          "MICHIGAN"=0,
                                          "MINNESOTA"=0,
                                          "MISSISSIPPI"=0,
                                          "MISSOURI"=0,
                                          "MONTANA"=0,
                                          "NEBRASKA"=0,
                                          "NEVADA"=0,
                                          "NEW HAMPSHIRE"=0,
                                          "NEW JERSEY"=1,
                                          "NEW MEXICO"=0,
                                          "NEW YORK"=0,
                                          "NORTH CAROLINA"=0,
                                          "NORTH DAKOTA"=0,
                                          "OHIO"=0,
                                          "OKLAHOMA"=0,
                                          "OREGON"=0,
                                          "PENNSYLVANIA"=0,
                                          "RHODE ISLAND"=0,
                                          "SOUTH CAROLINA"=0,
                                          "SOUTH DAKOTA"=0,
                                          "TENNESSEE"=0,
                                          "TEXAS"=0,
                                          "UTAH"=0,
                                          "VERMONT"=0,
                                          "VIRGINIA"=0,
                                          "WASHINGTON"=0,
                                          "WEST VIRGINIA"=0,
                                          "WYOMING"=0,
                                          "WISCONSIN"=0,
                                          "GUAM"=NA, 
                                          "U.S.  VIRGIN ISLANDS"=NA,
                                          "PUERTO RICO"=NA))

aggnisdf$treatstate<- revalue(aggnisdf$STATE, c("ALABAMA"= 0,
                                           "ALASKA"= 0,
                                           "ARIZONA"=0,
                                           "ARKANSAS"=0,
                                           "CALIFORNIA"=NA,
                                           "COLORADO"=0,
                                           "CONNECTICUT"=0,
                                           "DELAWARE"=0,
                                           "DISTRICT OF COLUMBIA"=0,
                                           "FLORIDA"=0,
                                           "GEORGIA"=0,
                                           "HAWAII"=0,
                                           "IDAHO"=0,
                                           "ILLINOIS"=0,
                                           "INDIANA"=0,
                                           "IOWA"=0,
                                           "KANSAS"=0,
                                           "KENTUCKY"=0,
                                           "LOUISIANA"=0,
                                           "MAINE"=0,
                                           "MARYLAND"=0,
                                           "MASSACHUSETTS"=0,
                                           "MICHIGAN"=0,
                                           "MINNESOTA"=0,
                                           "MISSISSIPPI"=0,
                                           "MISSOURI"=0,
                                           "MONTANA"=0,
                                           "NEBRASKA"=0,
                                           "NEVADA"=0,
                                           "NEW HAMPSHIRE"=0,
                                           "NEW JERSEY"=1,
                                           "NEW MEXICO"=0,
                                           "NEW YORK"=0,
                                           "NORTH CAROLINA"=0,
                                           "NORTH DAKOTA"=0,
                                           "OHIO"=0,
                                           "OKLAHOMA"=0,
                                           "OREGON"=0,
                                           "PENNSYLVANIA"=0,
                                           "RHODE ISLAND"=0,
                                           "SOUTH CAROLINA"=0,
                                           "SOUTH DAKOTA"=0,
                                           "TENNESSEE"=0,
                                           "TEXAS"=0,
                                           "UTAH"=0,
                                           "VERMONT"=0,
                                           "VIRGINIA"=0,
                                           "WASHINGTON"=0,
                                           "WEST VIRGINIA"=0,
                                           "WYOMING"=0,
                                           "WISCONSIN"=0,
                                           "GUAM"=NA, 
                                           "U.S.  VIRGIN ISLANDS"=NA,
                                           "PUERTO RICO"=NA))
ggplot(data=aggnisdf, aes(x=YEAR, y=BF_ENDR06, group=STATE, color=treatstate)) +
  geom_line() +
  geom_point() +
  xlab("Year") + ylab("Days BF") +
  ggtitle("breast feeding rate by year")

ggplot(data=aggnisdf, aes(x=YEAR, y=BF_ENDR06, group=STATE, color=STATE)) +
  geom_line() +
  geom_point() +
  xlab("Year") + ylab("Breast feeding rate") +
  ggtitle("breast feeding rate by year")

nisdf$TIME<-ifelse(nisdf$YEAR >= 2011, 1,0)
nisdf$treatstate <- as.numeric(nisdf$treatstate)
nisdf$DID <-nisdf$TIME*nisdf$treatstate
DIDREG_BF <- lm(BF_ENDR06 ~ treatstate + TIME +  DID, data=nisdf)
summary(DIDREG_BF)
DIDREG_E <- lm(BF_EXCLR06 ~ treatstate + TIME + DID, data=nisdf)
summary(DIDREG_E)

DIDREG_BF2 <- lm(BF_ENDR06 ~ treatstate + TIME +  DID + INCPOV1 , data=nisdf2)
summary(DIDREG_BF2)
DIDREG_E2 <- lm(BF_EXCLR06 ~ treatstate + TIME + DID+ INCPOV1, data=nisdf2)
summary(DIDREG_E2)



#stratified by income
povdf <- dplyr::filter(nisdf, INCPOV1 == "BELOW POVERTY")
DIDREG_BF3 <- lm(BF_ENDR06 ~ treatstate + TIME +  DID, data=nisdf)
summary(DIDREG_BF3)
DIDREG_E3 <- lm(BF_EXCLR06 ~ treatstate + TIME + DID, data=nisdf)
summary(DIDREG_E3)


#grouped data
attach(nisdf2)
agg <- aggregate(nisdf2, by=list(STATE, YEAR), FUN=mean, na.rm=TRUE)
detach(nisdf2)
agg <- dplyr::select(agg, Group.1, Group.2, BF_ENDR06, BF_EXCLR06)
agg <- rename(agg, c("Group.1"="STATE", "Group.2"="YEAR"))
#HERES THE REAL AGG
nisdf3 <- select(nisdf2, CBF_01, CWIC_01, CWIC_02, EDUC1, I_HISP_K, INCPOV1, 
                 LANGUAGE, M_AGEGRP, MARITAL2, RACEETHK)
nisdf4 <- acm.disjonctif(nisdf3)
nisdf5 <- select(nisdf2, STATE, YEAR)
nisdf6 <- bind_cols(nisdf5, nisdf4)
attach(nisdf6)
agg2 <- aggregate(nisdf6, by=list(STATE, YEAR), FUN=mean, na.rm=TRUE)
detach(nisdf6)
agg2[, "STATE"] <- NULL
agg2[, "YEAR"] <- NULL
agg3 <- bind_cols(agg2, agg)

agg3 <- plyr::rename(agg3, c("Group.1"="STATE", "Group.2"="YEAR"))
write.csv(agg3, file = "aggNisdf.csv")


ggplot(data=agg3, aes(x=YEAR, y=CBF_01.YES, group=STATE, color=STATE, label=STATE)) +
  geom_line() +
  geom_point() +
  geom_dl(aes(label = STATE), method = list(dl.combine("last.points"), cex = 0.5))+ 
  xlab("Year") + ylab("Child ever BF rate") +
  ggtitle("Child ever BF rate")

agg3$treatstate<- plyr::revalue(agg2$STATE, c("ALABAMA"= 0,
                                                "ALASKA"= 0,
                                                "ARIZONA"=0,
                                                "ARKANSAS"=0,
                                                "CALIFORNIA"=NA,
                                                "COLORADO"=0,
                                                "CONNECTICUT"=0,
                                                "DELAWARE"=0,
                                                "DISTRICT OF COLUMBIA"=0,
                                                "FLORIDA"=0,
                                                "GEORGIA"=0,
                                                "HAWAII"=0,
                                                "IDAHO"=0,
                                                "ILLINOIS"=0,
                                                "INDIANA"=0,
                                                "IOWA"=0,
                                                "KANSAS"=0,
                                                "KENTUCKY"=0,
                                                "LOUISIANA"=0,
                                                "MAINE"=0,
                                                "MARYLAND"=0,
                                                "MASSACHUSETTS"=0,
                                                "MICHIGAN"=0,
                                                "MINNESOTA"=0,
                                                "MISSISSIPPI"=0,
                                                "MISSOURI"=0,
                                                "MONTANA"=0,
                                                "NEBRASKA"=0,
                                                "NEVADA"=0,
                                                "NEW HAMPSHIRE"=0,
                                                "NEW JERSEY"=1,
                                                "NEW MEXICO"=0,
                                                "NEW YORK"=0,
                                                "NORTH CAROLINA"=0,
                                                "NORTH DAKOTA"=0,
                                                "OHIO"=0,
                                                "OKLAHOMA"=0,
                                                "OREGON"=0,
                                                "PENNSYLVANIA"=0,
                                                "RHODE ISLAND"=0,
                                                "SOUTH CAROLINA"=0,
                                                "SOUTH DAKOTA"=0,
                                                "TENNESSEE"=0,
                                                "TEXAS"=0,
                                                "UTAH"=0,
                                                "VERMONT"=0,
                                                "VIRGINIA"=0,
                                                "WASHINGTON"=0,
                                                "WEST VIRGINIA"=0,
                                                "WYOMING"=0,
                                                "WISCONSIN"=0,
                                                "GUAM"=NA, 
                                                "U.S.  VIRGIN ISLANDS"=NA,
                                                "PUERTO RICO"=NA))

agg3$TIME<-ifelse(agg3$YEAR >= 2011, 1,0)
agg3$treatstate <- as.numeric(agg3$treatstate)
agg3$DID <-agg3$TIME*agg3$treatstate
agg3$DID2 <-agg3$TIME*agg3$treatstate


DIDREG1 <- lm(BF_ENDR06 ~ treatstate + TIME +  DID, data=agg3)
summary(DIDREG1)

