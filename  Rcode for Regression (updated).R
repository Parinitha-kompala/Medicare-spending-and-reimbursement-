
library(tidyverse)
library(readxl)
library(lme4)

dataset <- read_excel(file.choose())

# Examining the Data 
head(dataset)
dim(dataset)
colnames(dataset)
unique(dataset$Event_label)

#Regression
# Is the adjusted rate adjusted for population?? 
spread_table <- dataset %>% select(Geo_Name, Adjusted_Rate, Year, Event_label, Population) %>% spread(key="Event_label", value="Adjusted_Rate")
 ## Check to see if this is right..... 
test_lmer <- lmer(`Part B Spending per Decedent during the Last Six Months of Life` ~ Geo_Name + (1 | Year), data = spread_table)
summary(test_lmer)
qqnorm(spread_table$`Part B Spending per Decedent during the Last Six Months of Life`)
qqline(spread_table$`Part B Spending per Decedent during the Last Six Months of Life`, col="red")

#outcome : modified_table$`Part B Spending per Decedent during the Last Six Months of Life`


# Covariates
ICU_Days <- 'ICU Days per Decedent During the Hospitalization In Which Death Occurred'
Phy_Visits <- 'Physician Visits per Decedent during the Last Six Months of Life'
Dif_Phys <- 'Number of Different Physicians Seen per Decedent during the Last Six Months of Life'
ICU_Beds <- 'High-Intensity ICU Bed Inputs per 1,000 Decedents during the Last Six Months of Life'
HHA_Visits <- 'Home Health Agency Visits per Decedent during the Last Six Months of Life'
Perc_Hospice <- 'Percent of Decedents Enrolled In Hospice during the Last Six Months of Life'

colnames(spread_table) <- str_replace(colnames(spread_table), 'ICU Days per Decedent During the Hospitalization In Which Death Occurred', 'ICU_Days')
colnames(spread_table) <- str_replace(colnames(spread_table), 'Physician Visits per Decedent during the Last Six Months of Life', 'Phy_Visits')
colnames(spread_table) <- str_replace(colnames(spread_table), 'Number of Different Physicians Seen per Decedent during the Last Six Months of Life', 'Dif_Phys')
colnames(spread_table) <- str_replace(colnames(spread_table), 'High-Intensity ICU Bed Inputs per 1,000 Decedents during the Last Six Months of Life', 'ICU_Beds')
colnames(spread_table) <- str_replace(colnames(spread_table), 'Home Health Agency Visits per Decedent during the Last Six Months of Life', 'HHA_Visits')
colnames(spread_table) <- str_replace(colnames(spread_table), 'Percent of Decedents Enrolled In Hospice during the Last Six Months of Life', 'Perc_Hospice')

# No Cluster by State
test_lmer <- lmer(`Part B Spending per Decedent during the Last Six Months of Life` ~ Geo_Name + ICU_Days + Phy_Visits + Dif_Phys + ICU_Beds + HHA_Visits + Perc_Hospice + (1 | Year), data = spread_table)
summary(test_lmer)

# Medicare spending as outomce
test_lmer <- lmer(`Total Medicare Reimbursements per Decedent during the Last Six Months of Life` ~ Geo_Name + ICU_Days + Phy_Visits + Dif_Phys + ICU_Beds + HHA_Visits + Perc_Hospice + (1 | Year), data = spread_table)
summary(test_lmer)

# Cluster by State
test_lmer2 <- lmer(`Part B Spending per Decedent during the Last Six Months of Life` ~ ICU_Days + Phy_Visits + Dif_Phys + ICU_Beds + HHA_Visits + Perc_Hospice  + (1 | Year) + (1 | Geo_Name), data = spread_table)
summary(test_lmer2)




# Linear Regression (time not a random effect)
test_lm <- lm(`Part B Spending per Decedent during the Last Six Months of Life` ~ Geo_Name +Year, data = spread_table)

# Step Function
steplm2 <- step(test_lm, direction = c("backward"))
summary(steplm2)

library(StepReg)









# Other
ex <- dataset %>% filter(Event_label == "Ambulance spending per Decedent during the last two years of life", Geo_Name == "Utah")

# Make Movie of data over time
library(magick)
library(magrittr)

faster <- 4
slow <- 1
list.files(path='/Users/elizabethanderson/OneDrive/Elizabeth\'s Documents/PhD/Biomedical Informatics/GIFMaker', pattern = '*.png', full.names = TRUE) %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=slow) %>% # animates, can opt for number of loops
  image_write("FileName.gif") # write to current dir
