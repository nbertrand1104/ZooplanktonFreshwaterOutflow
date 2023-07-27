#first attempts at writing a script to execute the "Hennessy and Burris model"

# install.packages("devtools")
#devtools::install_github("InteragencyEcologicalProgram/zooper")


## Example code for plotting zooper data from Sam Bashevkin/IEP's github page -- modified as notedmin
library(zooper)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(here)

set.seed(16)


# Set working directory and THEN load here package
setwd("C:/Users/nbertrand/Desktop/Bertrand/GitHub/ZooplanktonFreshwaterOutflow")


# Size classes and critters
## Macro: 500-505 um; amphipods and mysids (prior to 1974, EMP macrozoops were sampled using a 930 um mesh net)
## Meso: 150-160 um; copepods, cladocera
## Micro: 43-50 um; copepods, rotifers
##


# MESO ZOOPS
#############
#############

############################
# Download and Export Data #
############################

# Create a dataframe with data from zooper; for this appendix
## FMWT and STN data start in 2005; EMP starts in 1990; 20mm starts in 1995
MyZoops <- Zoopsynther(Data_type = "Taxa",                        # Taxa or Community
                       Sources = c("EMP","FMWT","STN","DOP", "20mm"),     # EMP, FMWT, STN, DOP, 20 mm
                       Size_class = "Meso",
                       Date_range = c("1988-01-01", "2022-12-31"))   # Pseudo show up in EMP in 1988
#                       Date_range = c("2017-08-01", "2021-11-30"))     # for all surveys, summer-fall

# If using multiple data sources (surveys) Check max and min dates for each source
Zoop_Summy <- MyZoops %>%
  group_by(Source) %>%
  summarize(first=min(as.Date(Date)),last=max(as.Date(Date)))

# Export file for use later
## Change file name if different data source(s) and/or time period
#saveRDS(MyZoops, file = "Zoop_Review_DataRaw/EMP_Zooper_1988_2021.rds")
#saveRDS(MyZoops, file = "Zoop_Review_DataRaw/AllSumFall_Zooper_2017_2021")
#saveRDS(MyZoops, file = "AllSurveys_Zooper_1988_2021.rds")

###################
# Manipulate Data #
###################

# Import data file
#MyZoops <- readRDS(file = "Zoop_Review_DataRaw/EMP_Zooper_1988_2021.rds")
#MyZoops <- readRDS(file = "Zoop_Review_DataRaw/AllSumFall_Zooper_2017_2021.rds")

# See if juveniles are identified to P. forbesi...they are not; however, very few adult P. marinus likely
## means very few juvenile P. marinus
#Pforbesi_Summy <- MyZoops %>%
#  filter(Genus == "Pseudodiaptomus") %>%
#  group_by(Lifestage, Taxname) %>%
#  summarize(first=min(as.Date(Date)),last=max(as.Date(Date)))


# Filter for adult P. forbesi and remove Taxatype = "Summed group" rows
Pforbesi_ad <- MyZoops %>%
  filter(Taxname == "Pseudodiaptomus forbesi", Lifestage == "Adult") %>%
  filter(Undersampled == "FALSE") %>%
  filter(!Taxatype == "Summed group")

# Filter for Pseudodiaptomus juveniles and remove Taxatype = "Summed group" rows
Pforbesi_juv <- MyZoops%>%
  filter(Genus == "Pseudodiaptomus", Lifestage == "Juvenile") %>%
  filter(Undersampled == "FALSE") %>%
  filter(!Taxatype == "Summed group")


view(Pforbesi_ad)

view(Pforbesi_juv)

allpf<- rbind.data.frame(Pforbesi_ad,Pforbesi_juv)
view(allpf)
##########################
##########

plt1<- ggplot(allpf,aes(x = as.Date(Date), y = CPUE))+
  geom_point()+
  facet_wrap(year(as.Date(Date)))
plt1

as.POSIXlt()


##############
#########################

library(readr)
negbinmodel_daily_dataset <- read_csv("Data/negbinmodel_daily_dataset.csv")
View(negbinmodel_daily_dataset)

outflow <- negbinmodel_daily_dataset %>%  select(date, delta_outflow)

view(outflow)


pforbesi_ad_flow <-left_join(outflow, Pforbesi_ad, by = join_by(date == Date))
view(pforbesi_ad_flow)

months <- c(6,7,8,9)

pforbesi_ad_flow <- pforbesi_ad_flow %>% filter(month(date) %in% months)
view(pforbesi_ad_flow)
######################################

cdfw <- 





##########################################
annualmeans <- pforbesi_ad_flow %>% 
  group_by(year(date)) %>% 
  summarise(CPUE_mean = mean(CPUE, na.rm = TRUE), 
            delta_outflow_mean = mean(delta_outflow))


view(annualmeans)


ggplot(annualmeans, aes(CPUE_mean)) +
  geom_histogram()

ggplot(annualmeans, aes(x=delta_outflow_mean, y=CPUE_mean)) +
  geom_point(size=2, shape=23)+
  geom_smooth()

#####################################
#attempting to build a linear regression model

library(broom)
library(ggpubr)
class(annualmeans$delta_outflow_mean)
summary(annualmeans)

cor(annualmeans$CPUE_mean,annualmeans$delta_outflow_mean)
#-0.0763818

hist(annualmeans$CPUE_mean)

plot(CPUE_mean ~delta_outflow_mean, data = annualmeans)


model.lm<- lm(CPUE_mean ~delta_outflow_mean, data = annualmeans)
summary(model.lm)
