# IMPACT OF MASS SHOOTINGS ON MENTAL HEALTH POLICY


## Installing and loading packages
library("readxl")
Replication_dataset <- read_excel("~/Replication dataset.xlsx")
install.packages(c("plm", "pglm", "pgmm", "stargazer", "ggplot2"))
library("plm")
library("pglm")
library("pgmm")
library("stargazer")

# Converting dataset to a panel dataset with State (denoted by "FIPS") and Year as indices
Replication_dataset_panel = pdata.frame(Replication_dataset, index =c("FIPS","Year"), drop.index = FALSE)
# Creating indicator Variable for mass shootings
Replication_dataset_panel$Mass.shooting.indicator = ifelse(Replication_dataset_panel$mass_shootings, 1, 0)

# Baseline OLS estimates for total number of bills and laws
OLSEst1 <- lm(Total.number.of.Bills ~ lag(Mass.shooting.indicator,1), data =Replication_dataset_panel)            
OLSEst2 = plm(Total.number.of.Bills ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, model = "pooling")
OLSEst3 = plm(Total.number.of.Bills ~ lag(Mas.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, model = "within", effect ="twoways")

OLSEst4 = lm(Total.number.of.laws ~ lag(Mass.shooting.indicator,1), data =Replication_dataset_panel)
OLSEst5 = plm(Total.number.of.laws ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, model = "pooling")
OLSEst6 = plm(Total.number.of.laws ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, model = "within", effect ="twoways")

# Poisson specification for the count nature of the data
library("pglm")
PoissonEst2 = pglm(Total.number.of.Bills ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "poisson", model = "pooling")
PoissonEst6 = pglm(Total.number.of.Bills ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "poisson", model = "within", effect ="twoways")
PoissonEst5 = pglm(Total.number.of.laws ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "poisson", model = "pooling")
PoissonEst6 = pglm(Total.number.of.laws ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "poisson", model = "within", effect ="twoways")

# Estimate of Insurance related bills and laws 
## Intensive Margin
InsuranceEst3 = pglm(Insurance.Bills ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "poisson", model = "within", effect ="twoways")
InsuranceEst3 = pglm(Insurance.Laws ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "poisson", model = "within", effect ="twoways")
InsuranceEst3 = pglm(Insurance.Bills_dummy ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "logit", model = "within", effect ="twoways")
## Extensive Margin
InsuranceEst3 = pglm(Insurance.Laws_dummy ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "logit", model = "within", effect ="twoways")

# Estimate for School related bills and laws
## Intensive margin
SchoolEst3 = pglm(School.Bills ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "poisson", model = "within", effect ="twoways")
SchoolEst3 = pglm(School.Laws ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "poisson", model = "within", effect ="twoways")
SchoolEst3 = pglm(School.Bills_dummy ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "logit", model = "within", effect ="twoways")
## Extensive Margin
SchoolEst3 = pglm(School.Laws_dummy ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "logit", model = "within", effect ="twoways")


#Estimate for Community mental healthcare related bills and laws
## Intensive Margin
CommunityEst3 = pglm(Community.Bills ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "poisson", model = "within", effect ="twoways")
CommunityEst3 = pglm(Community.Laws ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "poisson", model = "within", effect ="twoways")
CommunityEst3 = pglm(Community.Bills_dummy ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "logit", model = "within", effect ="twoways")
## Extensive Margin
CommunityEst3 = pglm(Community.Laws_dummy ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "logit", model = "within", effect ="twoways")


# Estimate for firearm related bills and laws
## Intensive Margin
FirearmEst3 = pglm(Gun.Bills ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "poisson", model = "within", effect ="twoways")
FirearmEst3 = pglm(Gun.Laws ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "poisson", model = "within", effect ="twoways")
FirearmEst3 = pglm(Gun.Bills_dummy ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "logit", model = "within", effect ="twoways")
## Extensive Margin
FirearmEst3 = pglm(Gun.Laws_dummy ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "logit", model = "within", effect ="twoways")


# Hurdle Model with Fixed Effects (likelihood function did not converge for most specifications except for the Total count of bills and laws)
HurdleEst1 = hurdle(Total.number.of.Bills ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session + as.factor(FIPS) + as.factor(Year), data = Replication_dataset_panel, dist = c("poisson"), link = c("logit"))
HurdleEst2 = hurdle(Insurance.Bills ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session + as.factor(FIPS) + as.factor(Year), data = Replication_dataset_panel, dist = c("poisson"), link = c("logit"))
HurdleEst3 = hurdle(School.number.of.Bills ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session + as.factor(FIPS) + as.factor(Year), data = Replication_dataset_panel, dist = c("poisson"), link = c("logit"))
HurdleEst4 = hurdle(Community.Bills ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session + as.factor(FIPS) + as.factor(Year), data = Replication_dataset_panel, dist = c("poisson"), link = c("logit"))
HurdleEst5 = hurdle(Gun.Bills ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session + as.factor(FIPS) + as.factor(Year), data = Replication_dataset_panel, dist = c("poisson"), link = c("logit"))

# Zero Inflated Poisson Model with Fixed Effects (likelihood function did not converge for most specifications except for the Total count of bills)
ZeroInflEst1 = zeroinfl(Total.number.of.Bills ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session + as.factor(FIPS) + as.factor(Year), data = Replication_dataset_panel, dist = c("poisson"), link = c("logit"))
ZeroInflEst2 = zeroinfl(Insurance.Bills ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session + as.factor(FIPS) + as.factor(Year), data = Replication_dataset_panel, dist = c("poisson"), link = c("logit"))
ZeroInflEst3 = zeroinfl(School.number.of.Bills ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session + as.factor(FIPS) + as.factor(Year), data = Replication_dataset_panel, dist = c("poisson"), link = c("logit"))
ZeroInflEst4 = zeroinfl(Community.Bills ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session + as.factor(FIPS) + as.factor(Year), data = Replication_dataset_panel, dist = c("poisson"), link = c("logit"))
ZeroInflEst5 = zeroinfl(Gun.Bills ~ lag(Mass.shooting.indicator,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session + as.factor(FIPS) + as.factor(Year), data = Replication_dataset_panel, dist = c("poisson"), link = c("logit"))










#Heterogenous Impact of Media Coverage

## All news networks except FOX news
MediaEstTotal = pglm(Total.number.of.Bills ~ lag(Mass.shooting.indicator,1) + lag(Coverage.in.minutes,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "poisson", model = "within", effect ="twoways")
MediaEstInsurance = pglm(Insurance.Bills ~ lag(Mass.shooting.indicator,1) + lag(Coverage.in.minutes,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "poisson", model = "within", effect ="twoways")
MediaEstSchool = pglm(School.Bills ~ lag(Mass.shooting.indicator,1) + lag(Coverage.in.minutes,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "poisson", model = "within", effect ="twoways")
MediaEstCommunity = pglm(Community.Bills ~ lag(Mass.shooting.indicator,1) + lag(Coverage.in.minutes,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "poisson", model = "within", effect ="twoways")
MediaEstGun = pglm(Gun.Bills ~ lag(Mass.shooting.indicator,1) + lag(Coverage.in.minutes,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "poisson", model = "within", effect ="twoways")

## All news networks including FOX news
MediaEstTotal = pglm(Total.number.of.laws ~ lag(Mass.shooting.indicator,1) + lag(Coverage.in.minutes,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = subset(Replication_dataset_panel, Year>2010), family = "poisson", model = "within", effect ="twoways")
MediaEstInsurance = pglm(Insurance.Laws ~ lag(Mass.shooting.indicator,1) + lag(Coverage.in.minutes,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = subset(Replication_dataset_panel, Year > 2010), family = "poisson", model = "within", effect ="twoways")
MediaEstSchool = pglm(School.Laws ~ lag(Mass.shooting.indicator,1) + lag(Coverage.in.minutes,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = subset(Replication_dataset_panel), family = "poisson", model = "within", effect ="twoways")
MediaEstCommunity = pglm(Community.Laws ~ lag(Mass.shooting.indicator,1) + lag(Coverage.in.minutes,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = subset(Replication_dataset_panel, Year > 2010), family = "poisson", model = "within", effect ="twoways")
MediaEstGun = pglm(Gun.Laws ~ lag(Mass.shooting.indicator,1) + lag(Coverage.in.minutes,1) + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = subset(Replication_dataset_panel, Year > 2010), family = "poisson", model = "within", effect ="twoways")


# Estimates for partisanship
PartisanEstTotal = pglm(Total.number.of.laws ~ lag(Mass.shooting.indicator,1)  + Democrat.legislature*lag(Mass.shooting.indicator,1) + lag(Mass.shooting.indicator,1)*Split.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "poisson", model = "within", effect ="twoways")
PartisanEstInsurance = pglm(Insurance.Laws ~ lag(Mass.shooting.indicator,1)  + Democrat.legislature*lag(Mass.shooting.indicator,1) + lag(Mass.shooting.indicator,1)*Split.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "poisson", model = "within", effect ="twoways")
PartisanEstSchool = pglm(School.Laws ~ lag(Mass.shooting.indicator,1)  + Democrat.legislature*lag(Mas.shooting.indicator,1) + lag(Mass.shooting.indicator,1)*Split.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "poisson", model = "within", effect ="twoways")
PartisanEstCommunity = pglm(Community.Laws ~ lag(Mass.shooting.indicator,1)  + Democrat.legislature*lag(Mass.shooting.indicator,1) + lag(Mass.shooting.indicator,1)*Split.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "poisson", model = "within", effect ="twoways")
PartisanEstFirearm = pglm(Firearm.Laws ~ lag(Mass.shooting.indicator,1)  + Democrat.legislature*lag(Mass.shooting.indicator,1) + lag(Mass.shooting.indicator,1)*Split.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "poisson", model = "within", effect ="twoways")

# Estimates for Race and mass shootings
RaceEstTotal = pglm(Total.number.of.laws ~ lag(Mass.shooting.indicator,1) + lag(Mass.shooting.indicator,1)*as.factor(White_Shooter) + lag(Mas.shooting.indicator,1)*White_Victim+ Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "poisson", model = "within", effect ="twoways")
RaceEstInsurance = pglm(Insurance.Laws ~ lag(Mass.shooting.indicator,1) + lag(Mass.shooting.indicator,1)*as.factor(White_Shooter) + lag(Mas.shooting.indicator,1)*White_Victim + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "poisson", model = "within", effect ="twoways")
RaceEstSchool = pglm(School.Laws ~ lag(Mass.shooting.indicator,1) + lag(Mass.shooting.indicator,1)*as.factor(White_Shooter) + lag(Mas.shooting.indicator,1)*White_Victim + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "poisson", model = "within", effect ="twoways")
RaceEstCommunity = pglm(Community.Laws ~ lag(Mass.shooting.indicator,1) + lag(Mass.shooting.indicator,1)*as.factor(White_Shooter) + lag(Mas.shooting.indicator,1)*White_Victim + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "poisson", model = "within", effect ="twoways")
RaceEstGun = pglm(Gun.Laws ~ lag(Mass.shooting.indicator,1) + lag(Mass.shooting.indicator,1)*as.factor(White_Shooter) + lag(Mas.shooting.indicator,1)*White_Victim + Democrat.legislature + Republican.legislature + Share.of.female.legislators + lag(Unemployment.rate,1) + elderly.share + lag(suicide.rate,1) + lag(Share.of.mental.health.workers,1) + First.year.of.biennum + Regular.session, data = Replication_dataset_panel, family = "poisson", model = "within", effect ="twoways")



