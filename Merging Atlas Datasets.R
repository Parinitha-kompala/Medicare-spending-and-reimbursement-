# Step 1: Download the datasets
# This is the dataset for Medicare Reimbursements (county) I downloaded: "https://data.dartmouthatlas.org/downloads/research_files/county_hedis_6575ffs.csv.zip"
# This is the dataset for Selected Primary Care Access and Quality (county) I downloaded: https://data.dartmouthatlas.org/downloads/research_files/county_hedis_6575ffs.csv.zip 
library(tidyverse)

county_medicare_reimburs <- read.csv(file="county_stdprices_ffs.csv") # This is Longitudinal from 2011-2018
county_care_access <- read.csv(file="county_hedis_6575ffs.csv") # Longitudinal from 2008-2018

county_medicare_reimburs <- tibble(county_medicare_reimburs) %>% mutate("Id" = paste0(Geo_code, ":", Year))
county_care_access <- tibble(county_care_access)  %>% mutate("Id" = paste0(Geo_code, ":", Year))

county_care_access <- county_care_access %>% separate(Geo_Name, c("State", "County"), sep = "-")
write.table(county_care_access , file = "PrimaryCareAcesswithStateANDCounty.csv", row.names = FALSE)

county_medicare_reimburs <- county_medicare_reimburs %>% separate(Geo_Name, c("State", "County"), sep = "-")
write.table(county_medicare_reimburs , file = "MedReimburswithStateANDCounty.csv", row.names = FALSE)

AllData <- inner_join(county_medicare_reimburs, county_care_access, by="Id")

write.table(AllData , file = "CombinedCountyData.csv")


state_medicare_reimburs <- read.csv(file="state_stdprices_ffs.csv")
state_care_access <- read.csv(file="state_hedis_6575ffs.csv")
