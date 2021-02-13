# Does medicaid expansion affect live birth weights?
# This program leverages the fact that several states opted to expand medicaid years after healtcare quality
# data began being published in 2014 to construct a difference-in-differences analysis of the effects of 
# the medicare expansion on live birth weights
# Conor Hennessy
# February, 2021

rm(list=ls())

years_with_data <- c(2014, 2015, 2016, 2017, 2018, 2019)

#first, get child health birth weight data from GitHub
#data originally from https://www.medicaid.gov/medicaid/quality-of-care/performance-measurement/adult-and-child-health-care-quality-measures/childrens-health-care-quality-measures/index.html#AnnualReporting

for (i in 1:length(years_with_data)) {
  link=paste0("https://github.com/cfhenn/healthcare/raw/master/Data/", toString(years_with_data[i]), "_Child_Health_Care_Quality_Measures.csv")
  lbw_df<- as.data.frame(read.csv(file = url(link)))
  lbw_df <- lbw_df[(lbw_df$Measure.Abbreviation == "LBW-CH" 
                                    & lbw_df$Population == "Medicaid only"),] 
  lbw_df <- lbw_df[,c("State", "State.Rate")]
  lbw_df$year <- years_with_data[i]
  assign(paste0("lbw", years_with_data[i]), lbw_df)
}

#combine all years  of birth data 
lbw_df <- rbind(lbw2014, lbw2015, lbw2016, lbw2017, lbw2018, lbw2019)

remove(lbw2014, lbw2015, lbw2016, lbw2017, lbw2018, lbw2019)

#next, merge in data on when expansions took effect in states that expanded medicaid
link="https://github.com/cfhenn/healthcare/raw/master/Data/medicaid_expansion.csv"
expansion_df <- as.data.frame(read.csv(file = url(link)))
names(expansion_df) <- c("State", "medicaid_expansion_year")
lbw_df <- merge(lbw_df, expansion_df, by="State")
remove(expansion_df)


#next, merge in data for control variables
#race and income have been shown to be correlated with live birth weight
link="https://github.com/cfhenn/healthcare/blob/master/Data/pums_smallfile.csv?raw=true"
pums_df <- as.data.frame(read.csv(file = url(link)))
pums_df$State <- pums_df$state
pums_df <- pums_df[(pums_df$hinscaid == "Has insurance through Medicaid"),]
pums_df$pct_white <- (pums_df$race == "White")*pums_df$perwt
pums_df$pct_black <- (pums_df$race == "Black/African American/Negro")*pums_df$perwt
pums_df$pct_asian <- (pums_df$race == "Other Asian or Pacific Islander" | pums_df$race == "Chinese" | pums_df$race == "Japanese" )*pums_df$perwt

pums_df$inctot[(pums_df$inctot == 9999999)] <- NA
pums_df$inctot <- pums_df$inctot*pums_df$perwt

names(pums_df)[names(pums_df) == "statefip"] <- "state"

pums_df <- aggregate(cbind(pct_white, pct_black, pct_asian, inctot, perwt) ~ State + year, pums_df, sum)

controlvars_list <- c("pct_white", "pct_black", "pct_asian", "inctot")
for (i in 1:length(controlvars_list)){
  pums_df[controlvars_list[i]] <- pums_df[controlvars_list[i]]/pums_df$perwt
}

lbw_df <- merge(lbw_df, pums_df, by = c("State", "year"))
remove(pums_df)

#finally, conduct a differnce-in-differences regression
#medicaid expansion considered as the treatment
#regression will be run once with control variables and once without
#control variables likely to be important, as states with medicaid expansions will have higher-income medicaid patients
#and income has been shown to correlate with live birth weight

lbw_df$medicaid_expansion_year[is.na(lbw_df$medicaid_expansion_year)] <- -9999
lbw_df$State.Rate <- as.integer(lbw_df$State.Rate)
lbw_df <- subset(lbw_df, (lbw_df$medicaid_expansion != 2014) 
       & (lbw_df$medicaid_expansion != 2019) 
       & (lbw_df$year != 2015) 
       & (lbw_df$year != 2016) )

lbw_df$time    <-  as.integer(lbw_df$year > 2016)
lbw_df$treated <- as.integer(lbw_df$medicaid_expansion_year > 0)
lbw_df$d_i_d   <- lbw_df$time*lbw_df$treated

did_reg <- lm(State.Rate ~ time + treated + d_i_d, data = lbw_df)
summary(did_reg)

did_reg_w_control <- lm(State.Rate ~ time + treated + d_i_d + pct_white + pct_black + pct_asian + inctot, data = lbw_df)
summary(did_reg_w_control)

		
#this regression does not show that the medicaid expansion had any affect on 
#live birth weights; however, the number of states in the treatment and control 
#groups are small. Also, the parallel trends assumption may not hold, and the 
#sample size is too small for propensity-score matching, thus these 
#results are do not comprehensively disprove that any such effect exists

