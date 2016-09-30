


#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################


#DROP the weird extra state variable
NIS12[, "ESTIAP12"] <- NULL

##convert cont vars to int so they append correctly
NIS11$BFENDFL06 <- as.integer(NIS11$BFENDFL06)
NIS11$BFFORMFL06 <- as.integer(NIS11$BFFORMFL06)
NIS11[, "BFENDFL06"] <- NULL
NIS11[, "BFFORMFL06"] <- NULL
NIS11[, "INTRP"] <- NULL
NIS11[, "RENT_OWN"] <- NULL 

NIS12$BFENDFL06 <- as.integer(NIS12$BFENDFL06)
NIS12$BFFORMFL06 <- as.integer(NIS12$BFFORMFL06)
NIS12[, "BFENDFL06"] <- NULL
NIS12[, "BFFORMFL06"] <- NULL
NIS12[, "INTRP"] <- NULL
NIS12[, "RENT_OWN"] <- NULL 

NIS13$BFENDFL06 <- as.integer(NIS13$BFENDFL06)
NIS13$BFFORMFL06 <- as.integer(NIS13$BFFORMFL06)
NIS13[, "BFENDFL06"] <- NULL
NIS13[, "BFFORMFL06"] <- NULL
NIS13[, "INTRP"] <- NULL
NIS13[, "RENT_OWN"] <- NULL 

NIS14$BFENDFL06 <- as.integer(NIS14$BFENDFL06)
NIS14$BFFORMFL06 <- as.integer(NIS14$BFFORMFL06)
NIS14[, "BFENDFL06"] <- NULL
NIS14[, "BFFORMFL06"] <- NULL
NIS14[, "INTRP"] <- NULL
NIS14[, "RENT_OWN"] <- NULL 

nisALL <- bind_rows(NIS11, NIS12, NIS13, NIS14)


nispuf10 <- read.csv("~/Desktop/R/nispuf10.csv")
nispuf09 <- read.csv("~/Desktop/R/nispuf09.csv")
nispuf08 <- read.csv("~/Desktop/R/nispuf08.csv")
nispuf07 <- read.csv("~/Desktop/R/nispuf07.csv")
nispuf06 <- read.csv("~/Desktop/R/nispuf06.csv")
nispuf05 <- read.csv("~/Desktop/R/nispuf05.csv")
nispuf04 <- read.csv("~/Desktop/R/nispuf04.csv")
nispuf03 <- read.csv("~/Desktop/R/nispuf03.csv")

nispuf03[, "ALL4SHOT"] <- NULL 
nispuf04[, "ALL4SHOT"] <- NULL 
nispuf05[, "ALL4SHOT"] <- NULL 

nispuf03[, "ITRUEIAP"] <- NULL 
nispuf04[, "ITRUEIAP"] <- NULL 

##FIX insurance vars
nispuf03$INS_1 <- NA
nispuf04$INS_1 <- NA
nispuf05$INS_1 <- NA
nispuf06$INS_1 <- NA

nispuf03$INS_2 <- NA
nispuf04$INS_2 <- NA
nispuf05$INS_2 <- NA
nispuf06$INS_2 <- NA

nispuf03$INS_3 <- NA
nispuf04$INS_3 <- NA
nispuf05$INS_3 <- NA
nispuf06$INS_3 <- NA

nispuf03$INS_3A <- NA
nispuf04$INS_3A <- NA
nispuf05$INS_3A<- NA
nispuf06$INS_3A <- NA


nispuf03$INS_4_5 <- NA
nispuf04$INS_4_5 <- NA
nispuf05$INS_4_5 <- NA
nispuf06$INS_4_5 <- NA
colnames(nispuf07)[which(colnames(nispuf07) == 'INS_4')] <- 'INS_4_5'
colnames(nispuf08)[which(colnames(nispuf08) == 'INS_4')] <- 'INS_4_5'

nispuf03$INS_6 <- NA
nispuf04$INS_6 <- NA
nispuf05$INS_6 <- NA
nispuf06$INS_6 <- NA

nispuf03$INS_11 <- NA
nispuf04$INS_11 <- NA
nispuf05$INS_11 <- NA
nispuf06$INS_11 <- NA

##FIX bf names
colnames(nispuf03)[which(colnames(nispuf03) == 'BF_ENDR')] <- 'BF_ENDR06'
colnames(nispuf04)[which(colnames(nispuf04) == 'BF_ENDR')] <- 'BF_ENDR06'
colnames(nispuf05)[which(colnames(nispuf05) == 'BF_ENDR')] <- 'BF_ENDR06'

colnames(nispuf03)[which(colnames(nispuf03) == 'BF_EXCLR')] <- 'BF_EXCLR06'
colnames(nispuf04)[which(colnames(nispuf04) == 'BF_EXCLR')] <- 'BF_EXCLR06'
colnames(nispuf05)[which(colnames(nispuf05) == 'BF_EXCLR')] <- 'BF_EXCLR06'

nispuf03$INCQ298A <- NA
nispuf04$INCQ298A <- NA

nispuf03[, "BFENDFL"] <- NULL 
nispuf03[, "BFEXCLFL"] <- NULL 

nispuf04[, "BFENDFL"] <- NULL 
nispuf04[, "BFEXCLFL"] <- NULL 

nispuf05[, "BFENDFL"] <- NULL 
nispuf05[, "BFEXCLFL"] <- NULL 

colnames(nispuf03)[which(colnames(nispuf03) == 'INCPORAT')] <- 'INCPORAR'
colnames(nispuf04)[which(colnames(nispuf04) == 'INCPORAT')] <- 'INCPORAR'

colnames(nispuf06)[which(colnames(nispuf06) == 'BF_FORMR06')] <- 'BF_FORMR08'
colnames(nispuf07)[which(colnames(nispuf07) == 'BF_FORMR06')] <- 'BF_FORMR08'

nispuf03$BF_FORMR08 <- NA
nispuf04$BF_FORMR08 <- NA
nispuf05$BF_FORMR08 <- NA

nispuf07[, "INS_5"] <- NULL 
nispuf08[, "INS_5"] <- NULL 

colnames(nisALL)[which(colnames(nisALL) == 'MOBIL_I')] <- 'MOBIL'
colnames(nispuf03)[which(colnames(nispuf03) == 'INCPOV1R')] <- 'INCPOV1'
colnames(nispuf04)[which(colnames(nispuf04) == 'INCPOV1R')] <- 'INCPOV1'

nispuf03$BF_ENDR06 <- as.integer(nispuf03$BF_ENDR06)
nispuf04$BF_ENDR06 <- as.integer(nispuf04$BF_ENDR06)
nispuf05$BF_ENDR06 <- as.integer(nispuf05$BF_ENDR06)
nispuf06$BF_ENDR06 <- as.integer(nispuf06$BF_ENDR06)
nispuf07$BF_ENDR06 <- as.integer(nispuf07$BF_ENDR06)
nispuf08$BF_ENDR06 <- as.integer(nispuf08$BF_ENDR06)
nispuf09$BF_ENDR06 <- as.integer(nispuf09$BF_ENDR06)
nispuf10$BF_ENDR06 <- as.integer(nispuf10$BF_ENDR06)

nispuf03$BF_EXCLR06 <- as.integer(nispuf03$BF_EXCLR06)
nispuf04$BF_EXCLR06 <- as.integer(nispuf04$BF_EXCLR06)
nispuf05$BF_EXCLR06 <- as.integer(nispuf05$BF_EXCLR06)
nispuf06$BF_EXCLR06 <- as.integer(nispuf06$BF_EXCLR06)
nispuf07$BF_EXCLR06 <- as.integer(nispuf07$BF_EXCLR06)
nispuf08$BF_EXCLR06 <- as.integer(nispuf08$BF_EXCLR06)
nispuf09$BF_EXCLR06 <- as.integer(nispuf09$BF_EXCLR06)
nispuf10$BF_EXCLR06 <- as.integer(nispuf10$BF_EXCLR06)


##############################################################
##RECODE THE DUMB FACTOR LEVELS IN 03-04
nispuf03[nispuf03=="2  24 - 29 MONTHS"]<-"24 - 29 MONTHS"  
nispuf03$AGEGRP <- revalue(nispuf03$AGEGRP, c("1  19 - 23 MONTHS"="19 - 23 MONTHS", 
                                              "2  24 - 29 MONTHS"="24 - 29 MONTHS",
                                              "3  30 - 35 MONTHS"="30 - 35 MONTHS"))
nispuf04$AGEGRP <- revalue(nispuf04$AGEGRP, c("1  19 - 23 MONTHS"="19 - 23 MONTHS", 
                                              "2  24 - 29 MONTHS"="24 - 29 MONTHS",
                                              "3  30 - 35 MONTHS"="30 - 35 MONTHS"))


nispuf03$CBF_01 <- revalue(nispuf03$CBF_01, c("1  YES"="YES", 
                                              "2  NO"="NO"))

nispuf04$CBF_01 <- revalue(nispuf04$CBF_01, c("1  YES"="YES", 
                                              "2  NO"="NO"))

##
nispuf03$CWIC_01 <- revalue(nispuf03$CWIC_01, c("1  YES"="YES", 
                                                "2  NO"="NO",
                                                "3  NEVER HEARD OF WIC"="NEVER HEARD OF WIC",
                                                "6  DON'T KNOW"="DON'T KNOW",
                                                ".  MISSING"))

nispuf04$CWIC_01 <- revalue(nispuf04$CWIC_01, c("1  YES"="YES", 
                                                "2  NO"="NO",
                                                "3  NEVER HEARD OF WIC"="NEVER HEARD OF WIC",
                                                "6  DON'T KNOW"="DON'T KNOW",
                                                ".  MISSING"))

nispuf03$CWIC_02 <- revalue(nispuf03$CWIC_02, c("1  YES"="YES", 
                                                "2  NO"="NO",
                                                "3  NEVER HEARD OF WIC"="NEVER HEARD OF WIC",
                                                "6  DON'T KNOW"="DON'T KNOW",
                                                ".  MISSING"="MISSING"))

nispuf04$CWIC_02 <- revalue(nispuf04$CWIC_02, c("1  YES"="YES", 
                                                "2  NO"="NO",
                                                "3  NEVER HEARD OF WIC"="NEVER HEARD OF WIC",
                                                "6  DON'T KNOW"="DON'T KNOW",
                                                ".  MISSING"="MISSING"))

##
nispuf03$C1R <- revalue(nispuf03$C1R, c("1  1"="1", 
                                        "2  2"="2",
                                        "3  3"="3",
                                        "4  4"="4",
                                        "5  5"="5",
                                        "6  6"="6",
                                        "7  7"="7",
                                        "8  8+"="8"))

nispuf04$C1R <- revalue(nispuf04$C1R, c("1  1"="1", 
                                        "2  2"="2",
                                        "3  3"="3",
                                        "4  4"="4",
                                        "5  5"="5",
                                        "6  6"="6",
                                        "7  7"="7",
                                        "8  8+"="8"))

##
nispuf03$C5R <- revalue(nispuf03$C5R, c("1  MOTHER (STEP, FOSTER, ADOPTIVE) OR FEMALE GUARDIAN"="MOTHER (STEP, FOSTER, ADOPTIVE) OR FEMALE GUARDIAN", 
                                        "2  FATHER (STEP, FOSTER, ADOPTIVE) OR MALE GUARDIAN"="FATHER (STEP, FOSTER, ADOPTIVE) OR MALE GUARDIAN",
                                        "3  GRANDPARENT"="GRANDPARENT",
                                        "4  OTHER FAMILY MEMBER/FRIEND"="OTHER FAMILY MEMBER/FRIEND",
                                        "96  DON'T KNOW"="DON'T KNOW",
                                        "97  REFUSED"="REFUSED"))

nispuf04$C5R <- revalue(nispuf04$C5R, c("1  MOTHER (STEP, FOSTER, ADOPTIVE) OR FEMALE GUARDIAN"="MOTHER (STEP, FOSTER, ADOPTIVE) OR FEMALE GUARDIAN", 
                                        "2  FATHER (STEP, FOSTER, ADOPTIVE) OR MALE GUARDIAN"="FATHER (STEP, FOSTER, ADOPTIVE) OR MALE GUARDIAN",
                                        "3  GRANDPARENT"="GRANDPARENT",
                                        "4  OTHER FAMILY MEMBER/FRIEND"="OTHER FAMILY MEMBER/FRIEND",
                                        "96  DON'T KNOW"="DON'T KNOW",
                                        "97  REFUSED"="REFUSED"))

##
nispuf03$CEN_REG <- revalue(nispuf03$CEN_REG, c("1  NORTHEAST"="NORTHEAST",
                                                "2  MIDWEST"="MIDWEST", 
                                                "3  SOUTH"="SOUTH",
                                                "4  WEST"="WEST"))
nispuf04$CEN_REG <- revalue(nispuf04$CEN_REG, c("1  NORTHEAST"="NORTHEAST",
                                                "2  MIDWEST"="MIDWEST", 
                                                "3  SOUTH"="SOUTH",
                                                "4  WEST"="WEST"))

##
nispuf03$CHILDNM <- revalue(nispuf03$CHILDNM, c("1  1 CHILD"="ONE",
                                                "2  2-3 CHILDREN"="TWO OR THREE", 
                                                "3  4+ CHILDREN"="FOUR OR MORE"))
nispuf04$CHILDNM <- revalue(nispuf04$CHILDNM, c("1  1 CHILD"="ONE",
                                                "2  2-3 CHILDREN"="TWO OR THREE", 
                                                "3  4+ CHILDREN"="FOUR OR MORE"))
##
nispuf03$EDUC1 <- revalue(nispuf03$EDUC1, c("1  <12 YEARS"="> 12 YEARS, NON-COLLEGE GRAD",
                                            "2  12 YEARS"="12 YEARS", 
                                            "3  >12, NON COLLEGE GRADUATE"="> 12 YEARS, NON-COLLEGE GRAD",
                                            "4  COLLEGE GRADUATE"="COLLEGE GRADUATE"))

nispuf04$EDUC1 <- revalue(nispuf04$EDUC1, c("1  <12 YEARS"="> 12 YEARS, NON-COLLEGE GRAD",
                                            "2  12 YEARS"="12 YEARS", 
                                            "3  >12, NON COLLEGE GRADUATE"="> 12 YEARS, NON-COLLEGE GRAD",
                                            "4  COLLEGE GRADUATE"="COLLEGE GRADUATE"))

##
nispuf03$FRSTBRN <- revalue(nispuf03$FRSTBRN, c("2  YES"="YES", 
                                                "1  NO"="NO"))

nispuf04$FRSTBRN <- revalue(nispuf04$FRSTBRN, c("2  YES"="YES", 
                                                "1  NO"="NO"))

##
nispuf03$INCPORAR <- revalue(nispuf03$INCPORAR, c(".  MISSING"="MISSING"))

nispuf04$INCPORAR <- revalue(nispuf04$INCPORAR, c(".  MISSING"="MISSING"))


##
nispuf03$INCPOV1 <- revalue(nispuf03$INCPOV1, c("2  < 1.0 (BELOW)"="BELOW POVERTY",
                                                "1  >= 1.0 (ABOVE)"="ABOVE POVERTY",
                                                "3  UNKNOWN"="UNKNOWN"))

nispuf04$INCPOV1 <- revalue(nispuf04$INCPOV1, c("2  < 1.0 (BELOW)"="BELOW POVERTY",
                                                "1  >= 1.0 (ABOVE)"="ABOVE POVERTY",
                                                "3  UNKNOWN"="UNKNOWN"))
##
nispuf03$LANGUAGE <- revalue(nispuf03$LANGUAGE, c("1  ENGLISH"="ENGLISH",
                                                  "2  SPANISH"="SPANISH",
                                                  "3  OTHER LANGUAGE"="OTHER LANGUAGE"))

nispuf04$LANGUAGE <- revalue(nispuf04$LANGUAGE, c("1  ENGLISH"="ENGLISH",
                                                  "2  SPANISH"="SPANISH",
                                                  "3  OTHER LANGUAGE"="OTHER LANGUAGE"))


##
nispuf03$MARITAL <- revalue(nispuf03$MARITAL, c("1  WIDOWED/DIVORCED/SEPARATED/DECEASED"="NEVER MARRIED/WIDOWED/DIVORCED/SEPARATED/DECEASED",
                                                "2  NEVER MARRIED"="NEVER MARRIED/WIDOWED/DIVORCED/SEPARATED/DECEASED",
                                                "3  MARRIED"="MARRIED"))

nispuf04$MARITAL <- revalue(nispuf04$MARITAL, c("1  WIDOWED/DIVORCED/SEPARATED/DECEASED"="NEVER MARRIED/WIDOWED/DIVORCED/SEPARATED/DECEASED",
                                                "2  NEVER MARRIED"="NEVER MARRIED/WIDOWED/DIVORCED/SEPARATED/DECEASED",
                                                "3  MARRIED"="MARRIED"))
##
nispuf03$MOBIL <- revalue(nispuf03$MOBIL, c("1  MOVED FROM DIFFERENT STATE"="MOVED FROM DIFFERENT STATE",
                                            "2  DID NOT MOVE FROM DIFFERENT STATE"="DID NOT MOVE FROM DIFFERENT STATE"))

nispuf04$MOBIL <- revalue(nispuf04$MOBIL, c("1  MOVED FROM DIFFERENT STATE"="MOVED FROM DIFFERENT STATE",
                                            "2  DID NOT MOVE FROM DIFFERENT STATE"="DID NOT MOVE FROM DIFFERENT STATE"))

##
nispuf03$M_AGEGRP <- revalue(nispuf03$M_AGEGRP, c("1  <=19"="<= 19 YEARS",
                                                  "2  20 - 29"="20 - 29 YEARS",
                                                  "3  30+"=">= 30 YEARS"))

nispuf04$M_AGEGRP <- revalue(nispuf04$M_AGEGRP, c("1  <=19"="<= 19 YEARS",
                                                  "2  20 - 29"="20 - 29 YEARS",
                                                  "3  30+"=">= 30 YEARS"))
##
nispuf03$SEX <- revalue(nispuf03$SEX, c("1  MALE"="MALE",
                                        "2  FEMALE"="FEMALE"))

nispuf04$SEX <- revalue(nispuf04$SEX, c("1  MALE"="MALE",
                                        "2  FEMALE"="FEMALE"))
##
nispuf03$STATE <- revalue(nispuf03$STATE, c("1  ALABAMA"="ALABAMA",
                                            "2  ALASKA"="ALASKA",
                                            "4  ARIZONA"="ARIZONA",
                                            "5  ARKANSAS"="ARKANSAS",
                                            "6  CALIFORNIA"="CALIFORNIA",
                                            "8  COLORADO"="COLORADO",
                                            "9  CONNECTICUT"="CONNECTICUT",
                                            "10  DELAWARE"="DELAWARE",
                                            "11  DIST. OF COLUMBIA"="DISTRICT OF COLUMBIA",
                                            "12  FLORIDA"="FLORIDA",
                                            "13  GEORGIA"="GEORGIA",
                                            "15  HAWAII"="HAWAII",
                                            "16  IDAHO"="IDAHO",
                                            "17  ILLINOIS"="ILLINOIS",
                                            "18  INDIANA"="INDIANA",
                                            "19  IOWA"="IOWA",
                                            "20  KANSAS"="KANSAS",
                                            "21  KENTUCKY"="KENTUCKY",
                                            "22  LOUISIANA"="LOUISIANA",
                                            "23  MAINE"="MAINE",
                                            "24  MARYLAND"="MARYLAND",
                                            "25  MASSACHUSETTS"="MASSACHUSETTS",
                                            "26  MICHIGAN"="MICHIGAN",
                                            "27  MINNESOTA"="MINNESOTA",
                                            "28  MISSISSIPPI"="MISSISSIPPI",
                                            "29  MISSOURI"="MISSOURI",
                                            "30  MONTANA"="MONTANA",
                                            "31  NEBRASKA"="NEBRASKA",
                                            "32  NEVADA"="NEVADA",
                                            "33  NEW HAMPSHIRE"="NEW HAMPSHIRE",
                                            "34  NEW JERSEY"="NEW JERSEY",
                                            "35  NEW MEXICO"="NEW MEXICO",
                                            "36  NEW YORK"="NEW YORK",
                                            "37  NORTH CAROLINA"="NORTH CAROLINA",
                                            "38  NORTH DAKOTA"="NORTH DAKOTA",
                                            "39  OHIO"="OHIO",
                                            "40  OKLAHOMA"="OKLAHOMA",
                                            "41  OREGON"="OREGON",
                                            "42  PENNSYLVANIA"="PENNSYLVANIA",
                                            "44  RHODE ISLAND"="RHODE ISLAND",
                                            "45  SOUTH CAROLINA"="SOUTH CAROLINA",
                                            "46  SOUTH DAKOTA"="SOUTH DAKOTA",
                                            "47  TENNESSEE"="TENNESSEE",
                                            "48  TEXAS"="TEXAS",
                                            "49  UTAH"="UTAH",
                                            "50  VERMONT"="VERMONT",
                                            "51  VIRGINIA"="VIRGINIA",
                                            "53  WASHINGTON"="WASHINGTON",
                                            "54  WEST VIRGINIA"="WEST VIRGINIA",
                                            "55  WISCONSIN"="WISCONSIN",
                                            "56  WYOMING"="WYOMING",
                                            "55  WISCONSIN"="WISCONSIN"))

nispuf04$STATE <- revalue(nispuf04$STATE, c("1  ALABAMA"="ALABAMA",
                                            "2  ALASKA"="ALASKA",
                                            "4  ARIZONA"="ARIZONA",
                                            "5  ARKANSAS"="ARKANSAS",
                                            "6  CALIFORNIA"="CALIFORNIA",
                                            "8  COLORADO"="COLORADO",
                                            "9  CONNECTICUT"="CONNECTICUT",
                                            "10  DELAWARE"="DELAWARE",
                                            "11  DIST. OF COLUMBIA"="DISTRICT OF COLUMBIA",
                                            "12  FLORIDA"="FLORIDA",
                                            "13  GEORGIA"="GEORGIA",
                                            "15  HAWAII"="HAWAII",
                                            "16  IDAHO"="IDAHO",
                                            "17  ILLINOIS"="ILLINOIS",
                                            "18  INDIANA"="INDIANA",
                                            "19  IOWA"="IOWA",
                                            "20  KANSAS"="KANSAS",
                                            "21  KENTUCKY"="KENTUCKY",
                                            "22  LOUISIANA"="LOUISIANA",
                                            "23  MAINE"="MAINE",
                                            "24  MARYLAND"="MARYLAND",
                                            "25  MASSACHUSETTS"="MASSACHUSETTS",
                                            "26  MICHIGAN"="MICHIGAN",
                                            "27  MINNESOTA"="MINNESOTA",
                                            "28  MISSISSIPPI"="MISSISSIPPI",
                                            "29  MISSOURI"="MISSOURI",
                                            "30  MONTANA"="MONTANA",
                                            "31  NEBRASKA"="NEBRASKA",
                                            "32  NEVADA"="NEVADA",
                                            "33  NEW HAMPSHIRE"="NEW HAMPSHIRE",
                                            "34  NEW JERSEY"="NEW JERSEY",
                                            "35  NEW MEXICO"="NEW MEXICO",
                                            "36  NEW YORK"="NEW YORK",
                                            "37  NORTH CAROLINA"="NORTH CAROLINA",
                                            "38  NORTH DAKOTA"="NORTH DAKOTA",
                                            "39  OHIO"="OHIO",
                                            "40  OKLAHOMA"="OKLAHOMA",
                                            "41  OREGON"="OREGON",
                                            "42  PENNSYLVANIA"="PENNSYLVANIA",
                                            "44  RHODE ISLAND"="RHODE ISLAND",
                                            "45  SOUTH CAROLINA"="SOUTH CAROLINA",
                                            "46  SOUTH DAKOTA"="SOUTH DAKOTA",
                                            "47  TENNESSEE"="TENNESSEE",
                                            "48  TEXAS"="TEXAS",
                                            "49  UTAH"="UTAH",
                                            "50  VERMONT"="VERMONT",
                                            "51  VIRGINIA"="VIRGINIA",
                                            "53  WASHINGTON"="WASHINGTON",
                                            "54  WEST VIRGINIA"="WEST VIRGINIA",
                                            "55  WISCONSIN"="WISCONSIN",
                                            "56  WYOMING"="WYOMING",
                                            "55  WISCONSIN"="WISCONSIN"))

##
##
nispuf03$RACE_K <- revalue(nispuf03$RACE_K, c("1  WHITE ONLY"="WHITE ONLY",
                                              "2  BLACK ONLY"="BLACK ONLY",
                                              "3  OTHER + MULTIPLE RACE"="OTHER + MULTIPLE RACE"
))


nispuf04$RACE_K <- revalue(nispuf04$RACE_K, c("1  WHITE ONLY"="WHITE ONLY",
                                              "2  BLACK ONLY"="BLACK ONLY",
                                              "3  OTHER + MULTIPLE RACE"="OTHER + MULTIPLE RACE"
))
##
nispuf03$I_HISP_K <- revalue(nispuf03$I_HISP_K, c("1  HISPANIC"="HISPANIC",
                                                  "2  NOT HISPANIC"="NON-HISPANIC"))

nispuf04$I_HISP_K <- revalue(nispuf04$I_HISP_K, c("1  HISPANIC"="HISPANIC",
                                                  "2  NOT HISPANIC"="NON-HISPANIC"))

##
nispuf03$RACEETHK <- revalue(nispuf03$RACEETHK, c("1  HISPANIC"="HISPANIC",
                                                  "2  NON-HISPANIC WHITE ONLY"="NON-HISPANIC WHITE ONLY",
                                                  "3  NON-HISPANIC BLACK ONLY"="NON-HISPANIC BLACK ONLY",
                                                  "4  NON-HISPANIC OTHER + MULTIPLE"="NON-HISPANIC OTHER + MULTIPLE"))

nispuf04$RACEETHK <- revalue(nispuf04$RACEETHK, c("1  HISPANIC"="HISPANIC",
                                                  "2  NON-HISPANIC WHITE ONLY"="NON-HISPANIC WHITE ONLY",
                                                  "3  NON-HISPANIC BLACK ONLY"="NON-HISPANIC BLACK ONLY",
                                                  "4  NON-HISPANIC OTHER + MULTIPLE"="NON-HISPANIC OTHER + MULTIPLE"))



nispuf03[ nispuf03 == "MISSING" ] = NA
nispuf04[ nispuf04 == "MISSING" ] = NA


nis3to10 <- bind_rows(nispuf03, nispuf04, nispuf05, nispuf06, 
                      nispuf07, nispuf08, nispuf09, nispuf10)
nis3to10$INCPORAR <- revalue(nis3to10$INCPORAR, c(">0"="0.00"))
nis3to10$INCPORAR <- as.numeric(nis3to10$INCPORAR)

#fucked up INS_6 somewhere, ignoring
nis3to10[, "MARITAL2"] <- NULL 
colnames(nis3to10)[which(colnames(nis3to10) == 'MARITAL')] <- 'MARITAL2'
nisALL[, "INS_6"] <- NULL 
nis3to10[, "MOBIL_I"] <- NULL 


nisdf <- bind_rows(nis3to10, nisALL)
##################################################################
##################################################################
##################################################################
##################################################################

##################################################################
##################################################################
##################################################################
##################################################################

nisdf$YEAR <- as.factor(nisdf$YEAR)
nisdf$CBF_01 <- as.factor(nisdf$CBF_01)
nisdf$CWIC_01 <- as.factor(nisdf$CWIC_01)
nisdf$CWIC_02 <- as.factor(nisdf$CWIC_02)
nisdf$C1R <- as.factor(nisdf$C1R)
nisdf$C5R <- as.factor(nisdf$C5R)
nisdf$CEN_REG <- as.factor(nisdf$CEN_REG)
nisdf$CHILDNM <- as.factor(nisdf$CHILDNM)
nisdf$EDUC1 <- as.factor(nisdf$EDUC1)
nisdf$FRSTBRN <- as.factor(nisdf$FRSTBRN)
nisdf$INCPOV1 <- as.factor(nisdf$INCPOV1)
nisdf$LANGUAGE <- as.factor(nisdf$LANGUAGE)
nisdf$I_HISP_K <- as.factor(nisdf$I_HISP_K)
nisdf$MARITAL2 <- as.factor(nisdf$MARITAL2)
nisdf$MOBIL <- as.factor(nisdf$MOBIL)
nisdf$M_AGEGRP <- as.factor(nisdf$M_AGEGRP)
nisdf$RACEETHK <- as.factor(nisdf$RACEETHK)
nisdf$SEX <- as.factor(nisdf$SEX)
nisdf$STATE <- as.factor(nisdf$STATE)
nisdf$INS_1 <- as.factor(nisdf$INS_1)
nisdf$INS_2 <- as.factor(nisdf$INS_2)
nisdf$INS_3 <- as.factor(nisdf$INS_3)
nisdf$INS_3A <- as.factor(nisdf$INS_3A)
nisdf$INS_4_5 <- as.factor(nisdf$INS_4_5)
nisdf$INS_11 <- as.factor(nisdf$INS_11)


nisdf$CBF_01 <- revalue(nisdf$CBF_01, c(".  MISSING"="MISSING",
                                        "6  DON'T KNOW"="DON'T KNOW",
                                        "7  REFUSED"="REFUSED"))

nisdf$CWIC_01 <- revalue(nisdf$CWIC_01, c(".  MISSING"="MISSING",
                                          "6  DON'T KNOW"="DON'T KNOW",
                                          "7  REFUSED"="REFUSED"))

nisdf$CWIC_02 <- revalue(nisdf$CWIC_02, c(".  MISSING"="MISSING",
                                          "6  DON'T KNOW"="DON'T KNOW",
                                          "7  REFUSED"="REFUSED"))

nisdf$C5R <- revalue(nisdf$C5R, c(".  MISSING"="MISSING"))
nisdf$MARITAL2 <- revalue(nisdf$MARITAL2, c("NEVER MARRIED"="NEVER MARRIED/WIDOWED/DIVORCED/SEPARATED/DECEASED"))
nisdf$MARITAL2 <- revalue(nisdf$MARITAL2, c("WIDOWED/DIVORCED/SEPARATED/DECEASED"="NEVER MARRIED/WIDOWED/DIVORCED/SEPARATED/DECEASED"))
nisdf$MOBIL <- revalue(nisdf$MOBIL, c("3  UNKNOWN"="UNKNOWN"))
nisdf$INCQ298A <- revalue(nisdf$INCQ298A, c("$75001+"="$7501 - $10000"))
nisdf$EDUC1 <- revalue(nisdf$EDUC1, c("COLLEGE GRAD"="COLLEGE GRADUATE"))

#create table 1
t1 <- c("YEAR", "AGEGRP", "CBF_01", "CWIC_01", "CWIC_02", 
        "C1R", "C5R", "CEN_REG", "CHILDNM","EDUC1", "FRSTBRN",  
        "INCPOV1", "I_HISP_K", "LANGUAGE", "MARITAL2", "MOBIL", 
        "M_AGEGRP", "RACE_K", "RACEETHK", "SEX", "STATE", "INS_1", "INS_2",
        "INS_3", "INS_3A", "INS_4_5", "INS_11", "INCQ298A")
t2 <- CreateCatTable(vars = t1, data = nisdf)

print(t2)

summary(nisdf$BF_ENDR06)
summary(nisdf$BF_EXCLR06)
summary(as.numeric(nisdf$BF_FORMR08))

