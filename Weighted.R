library(tidyverse)
library(survey)
#Reading in the cost supplement file
supp <- read.csv("cspuf2021.csv")

#Adjusting for the weighting process
mcbs <- svrepdesign(
  weights = ~CSPUFWGT,
  repweights = "CSPUF[001-100]+",
  type = "Fay",
  rho = 0.3,
  data = supp,
  combined.weights = TRUE
)

#Grabbing just 65 and older
mcbs_all = subset(mcbs, CSP_AGE != 1)

#The All Category

#Computing the mean expenditures
dental_mean = mean(svymean(~PAMTDU, design=mcbs_all))
vision_mean = mean(svymean(~PAMTVU, design=mcbs_all))
hearing_mean = mean(svymean(~PAMTHU, design=mcbs_all))
home_health_mean = mean(svymean(~PAMTHH, design=mcbs_all))
inpatient_mean = mean(svymean(~PAMTIP, design=mcbs_all))
medical_provider_mean = mean(svymean(~PAMTMP, design=mcbs_all))
outpatient_mean = mean(svymean(~PAMTOP, design=mcbs_all))
drugs_mean = mean(svymean(~PAMTPM, design=mcbs_all))

exp_means <- c(
  dental_mean,
  vision_mean,
  hearing_mean,
  home_health_mean,
  inpatient_mean,
  medical_provider_mean,
  outpatient_mean,
  drugs_mean
  )

total_exp = sum(exp_means)

#Finding the percentage of expenditures spent by type
exp_percents <- round(exp_means/total_exp*100,1)

#Computing the mean payer amounts

medicare_mean = mean(svymean(~PAMTCARE, design=mcbs_all))
medicaid_mean = mean(svymean(~PAMTCAID, design=mcbs_all))
medicare_mco_hmo_mean = mean(svymean(~PAMTMADV, design=mcbs_all))
private_insurance_mean = mean(svymean(~PAMTALPR, design=mcbs_all))
out_of_pocket_mean = mean(svymean(~PAMTOOP, design=mcbs_all))
uncollected_liability_mean = mean(svymean(~PAMTDISC, design=mcbs_all))
other_mean = mean(svymean(~PAMTOTH, design=mcbs_all))

payer_means <- c(
  medicare_mean,
  medicaid_mean,
  medicare_mco_hmo_mean,
  private_insurance_mean,
  out_of_pocket_mean,
  uncollected_liability_mean,
  other_mean
)

total_mean = mean(svymean(~PAMTTOT, design=mcbs_all))

#Find the percentage payed by payer
payer_percents <- round(payer_means/total_mean*100,1)

#The Men Category
mcbs_men <- subset(mcbs, CSP_SEX == 1 & CSP_AGE != 1)

#Computing the mean expenditures
dental_mean_men = mean(svymean(~PAMTDU, design=mcbs_men))
vision_mean_men = mean(svymean(~PAMTVU, design=mcbs_men))
hearing_mean_men = mean(svymean(~PAMTHU, design=mcbs_men))
home_health_mean_men = mean(svymean(~PAMTHH, design=mcbs_men))
inpatient_mean_men = mean(svymean(~PAMTIP, design=mcbs_men))
medical_provider_mean_men = mean(svymean(~PAMTMP, design=mcbs_men))
outpatient_mean_men = mean(svymean(~PAMTOP, design=mcbs_men))
drugs_mean_men = mean(svymean(~PAMTPM, design=mcbs_men))

exp_means_men <- c(
  dental_mean_men,
  vision_mean_men,
  hearing_mean_men,
  home_health_mean_men,
  inpatient_mean_men,
  medical_provider_mean_men,
  outpatient_mean_men,
  drugs_mean_men
)

total_exp_men = sum(exp_means_men)

#Finding the percentage of expenditures spent by type
exp_percents_men<- round(exp_means_men/total_exp_men*100,1)

#Computing the mean payer amounts

medicare_mean_men = mean(svymean(~PAMTCARE, design=mcbs_men))
medicaid_mean_men = mean(svymean(~PAMTCAID, design=mcbs_men))
medicare_mco_hmo_mean_men = mean(svymean(~PAMTMADV, design=mcbs_men))
private_insurance_mean_men = mean(svymean(~PAMTALPR, design=mcbs_men))
out_of_pocket_mean_men = mean(svymean(~PAMTOOP, design=mcbs_men))
uncollected_liability_mean_men = mean(svymean(~PAMTDISC, design=mcbs_men))
other_mean_men = mean(svymean(~PAMTOTH, design=mcbs_men))

payer_means_men <- c(
  medicare_mean_men,
  medicaid_mean_men,
  medicare_mco_hmo_mean_men,
  private_insurance_mean_men,
  out_of_pocket_mean_men,
  uncollected_liability_mean_men,
  other_mean_men
)

total_mean_men = mean(svymean(~PAMTTOT, design=mcbs_men))

#Find the percentage payed by payer
payer_percents_men <- round(payer_means_men/total_mean_men*100,1)

#The Women Category
mcbs_women <- subset(mcbs, CSP_SEX == 2  & CSP_AGE != 1)

#Computing the mean expenditures
dental_mean_women = mean(svymean(~PAMTDU, design=mcbs_women))
vision_mean_women = mean(svymean(~PAMTVU, design=mcbs_women))
hearing_mean_women = mean(svymean(~PAMTHU, design=mcbs_women))
home_health_mean_women = mean(svymean(~PAMTHH, design=mcbs_women))
inpatient_mean_women = mean(svymean(~PAMTIP, design=mcbs_women))
medical_provider_mean_women = mean(svymean(~PAMTMP, design=mcbs_women))
outpatient_mean_women = mean(svymean(~PAMTOP, design=mcbs_women))
drugs_mean_women = mean(svymean(~PAMTPM, design=mcbs_women))

exp_means_women <- c(
  dental_mean_women,
  vision_mean_women,
  hearing_mean_women,
  home_health_mean_women,
  inpatient_mean_women,
  medical_provider_mean_women,
  outpatient_mean_women,
  drugs_mean_women
)

total_exp_women = sum(exp_means_women)

#Finding the percentage of expenditures spent by type
exp_percents_women<- round(exp_means_women/total_exp_women*100,1)

#Computing the mean payer amounts

medicare_mean_women = mean(svymean(~PAMTCARE, design=mcbs_women))
medicaid_mean_women = mean(svymean(~PAMTCAID, design=mcbs_women))
medicare_mco_hmo_mean_women = mean(svymean(~PAMTMADV, design=mcbs_women))
private_insurance_mean_women = mean(svymean(~PAMTALPR, design=mcbs_women))
out_of_pocket_mean_women = mean(svymean(~PAMTOOP, design=mcbs_women))
uncollected_liability_mean_women = mean(svymean(~PAMTDISC, design=mcbs_women))
other_mean_women = mean(svymean(~PAMTOTH, design=mcbs_women))

payer_means_women <- c(
  medicare_mean_women,
  medicaid_mean_women,
  medicare_mco_hmo_mean_women,
  private_insurance_mean_women,
  out_of_pocket_mean_women,
  uncollected_liability_mean_women,
  other_mean_women
)

total_mean_women = mean(svymean(~PAMTTOT, design=mcbs_women))

#Find the percentage payed by payer
payer_percents_women <- round(payer_means_women/total_mean_women*100,1)

#Creating the tables

expenditure_types <- c(
  "dental", 
  "vision", 
  "hearing", 
  "home health", 
  "inpatient", 
  "medical provider", 
  "outpatient", 
  "prescription drugs"
)

expenditures <- tibble(
  Expenditure_Type = expenditure_types,
  all = exp_percents,
  men = exp_percents_men,
  women = exp_percents_women
)

payer_types <- c(
  "Medicare",
  "Medicaid",
  "Medicare MCO/HMO",
  "Private Insurance",
  "Out of Pocket",
  "Uncollected Liability",
  "Other"
)

payers <- tibble(
  Payer = payer_types,
  all = payer_percents,
  men = payer_percents_men,
  women = payer_percents_women
)
