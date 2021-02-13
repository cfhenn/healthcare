# Does medicaid expansion affect live birth weights?
# This program leverages the fact that several states opted to expand medicaid years after healtcare quality
# data began being published in 2014 to construct a difference-in-differences analysis of the effects of 
# the medicare expansion on live birth weights
# Conor Hennessy
# February, 2021

rm(list=ls())

years_with_data <- c(2014, 2015, 2016, 2017, 2018, 2019)

for (i in 1:length(years_with_data)) {
  link=paste0("https://github.com/cfhenn/healthcare/raw/master/Data/child_healthcare_data/", toString(years_with_data[i]), "_Child_Health_Care_Quality_Measures.csv")
  lbw_df<- as.data.frame(read.csv(file = url(link)))
  lbw_df <- lbw_df[(lbw_df$Measure.Abbreviation == "LBW-CH" 
                                    & lbw_df$Population == "Medicaid only"),] 
  lbw_df <- lbw_df[,c("State", "State.Rate")]
  lbw_df$year <- years_with_data[i]
  assign(paste0("lbw", years_with_data[i]), lbw_df)
}

lbw_df <- rbind(lbw2014, lbw2015, lbw2016, lbw2017, lbw2018, lbw2019)

remove(lbw2014, lbw2015, lbw2016, lbw2017, lbw2018, lbw2019)

link="https://github.com/cfhenn/healthcare/raw/master/Data/medicaid_expansion.csv"
expansion_df <- as.data.frame(read.csv(file = url(link)))
names(expansion_df) <- c("State", "medicaid_expansion_year")
lbw_df <- merge(lbw_df, expansion_df, by="State")
remove(expansion_df)

######################################
# NEW
######################################
link="https://github.com/cfhenn/healthcare/raw/master/Data/medicaid_expansion.csv"
pums_df <- as.data.frame(read.csv(file = url(link)))
pums_df <- pums_df[(pums_df$hincaid == 2)]
keep if hinscaid == 2 // only keep people on medicaid
gen pct_white = (race == 1)*perwt
gen pct_black = (race == 2)*perwt
gen pct_asian = (race >= 4 & race >= 6)*perwt
gen pct_othrc = (race == 3 | race >  6)*perwt
replace inctot = inctot*perwt
decode statefip, gen(state)

collapse (sum) pct_white pct_black pct_asian pct_othrc inctot perwt, by(state year)

local control_vars pct_white pct_black pct_asian pct_othrc inctot
foreach cv of local control_vars{
  replace `cv' = `cv'/perwt
}

merge 1:1 state year using medicaid_outcome_merged.dta, keep(3) nogen

######################################


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
		
