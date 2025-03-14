#loading necessary library
library(psych)
library(dplyr)
library(stringr)
library(ggplot2)
library(plm)
library(rcompanion)
library(vcd)
library(DT)
library(fixest)
library(lmtest)




#Loading real estate appraisal data from 2015 to 2022
data2022 <- read.csv("uad_2022.csv")
data2021 <- read.csv("uad_2021.csv")
data2020 <- read.csv("uad_2020.csv")
data2019 <- read.csv("uad_2019.csv")
data2018 <- read.csv("uad_2018.csv")
data2017 <- read.csv("uad_2017.csv")
data2016 <- read.csv("uad_2016.csv")
data2015 <- read.csv("uad_2015.csv")

#removing state, county and tract data for merging 2022 data
data2022 <- data2022 %>% 
  dplyr::select(-state_fips_2010, -county_fips_2010, -tract_fips_2010)

#renaming column name fips_2020 to fips_2010 to merge into main file for year 2022
names(data2022)[names(data2022) == "state_fips_2020"] <- "state_fips_2010"
names(data2022)[names(data2022) == "county_fips_2020"] <- "county_fips_2010"
names(data2022)[names(data2022) == "tract_fips_2020"] <- "tract_fips_2010"

# Define the number of data frames 
data_names <- paste0("data", 2015:2021)

for( df_name in data_names) {
  # Get the actual data frame by name
  df <- get(df_name)
  
  df <- df %>% 
    dplyr::select(-state_fips_2020, -county_fips_2020, -tract_fips_2020)
  
  # Assign the updated data frame back to its original name
  assign(df_name, df)
}

#merging data from 2015 to 2022
merged_data <- rbind( data2022, data2021 ,data2020,
                      data2019 ,data2018, data2017,
                      data2016,data2015)

#filter only home purchase --> Number : 635662
merged_data <- merged_data %>%
              filter(purpose == 1)

#removing missing values (9)
merged_data <-  merged_data %>% filter(built_up != 9)
merged_data <-  merged_data %>% filter(owner_occupied != 9)
merged_data <-  merged_data %>% filter(growth_rate != 9)
merged_data <-  merged_data %>% filter(property_value_trend != 9)
merged_data <-  merged_data %>% filter(demand_supply != 9)
merged_data <-  merged_data %>% filter(marketing_time != 9)
merged_data <-  merged_data %>% filter(lot_size != 9)
merged_data <-  merged_data %>% filter(quality != 9)
merged_data <-  merged_data %>% filter(condition != 9)
merged_data <-  merged_data %>% filter(bathrooms != 9)
merged_data <-  merged_data %>% filter(bedrooms != 9)
merged_data <-  merged_data %>% filter(gross_living_area != 9)
merged_data <-  merged_data %>% filter(dts_rural != 9)
merged_data <-  merged_data %>% filter(updated_last_15_years != 9)

#summary
summary(merged_data)

#replacing categories for each column
merged_data <- merged_data %>% 
  mutate(
    lot_size  = case_when(
      merged_data$lot_size  == 1 ~ "Less than 1/8 acre",
      merged_data$lot_size  == 2 ~ "1/8 up to 1/4 acre",
      merged_data$lot_size  == 3 ~ "1/4 up to 1/2 acre",
      merged_data$lot_size  == 4 ~ "1/2 up to 1 acre",
      merged_data$lot_size  == 5 ~ "1+ acre"
    )
  )

merged_data <- merged_data %>% 
  mutate(
    bathrooms = case_when(
      merged_data$bathrooms == 1 ~ "1 Bathrooms",
      merged_data$bathrooms == 2 ~ "2 Bathrooms",
      merged_data$bathrooms == 3 ~ "3 Bathrooms",
      merged_data$bathrooms == 4 ~ "4+ Bathrooms"
    )
  )

merged_data <- merged_data %>% 
  mutate(
    bedrooms = case_when(
      merged_data$bedrooms == 1 ~ "0-2 bedrooms",
      merged_data$bedrooms == 2 ~ "3 bedrooms",
      merged_data$bedrooms == 3 ~ "4+ bedrooms"
    )
  )

merged_data <- merged_data %>% 
  mutate(
    built_up = case_when(
      merged_data$built_up == 1 ~ "Over 75%",
      merged_data$built_up == 2 ~ "25% to 75%",
      merged_data$built_up == 3 ~ "Under 25%"
    )
  )

merged_data <- merged_data %>% 
  mutate(
    dts_rural = case_when(
      merged_data$dts_rural == 1 ~ "Yes",
      merged_data$dts_rural == 2 ~ "No"
    )
  )

merged_data <- merged_data %>% 
  mutate(
    gross_living_area = case_when(
      merged_data$gross_living_area == 1 ~ "Less than 1,250 sq. ft.",
      merged_data$gross_living_area == 2 ~ "1,250 to 1,499 sq. ft.",
      merged_data$gross_living_area == 3 ~ "1,500 to 1,749 sq. ft.",
      merged_data$gross_living_area == 4 ~ "1,750 to 1,999 sq. ft.",
      merged_data$gross_living_area == 5 ~ "2,000 to 2,249 sq. ft.",
      merged_data$gross_living_area == 6 ~ "2,250 to 2,499 sq. ft.",
      merged_data$gross_living_area == 7 ~ "2,500 to 2,999 sq. ft.",
      merged_data$gross_living_area == 8 ~ "3,000 or more sq. ft."
    )
  )

merged_data <- merged_data %>% 
  mutate(
    condition = case_when(
      merged_data$condition == 1 ~ "C1",
      merged_data$condition == 2 ~ "C2",
      merged_data$condition == 3 ~ "C3",
      merged_data$condition == 4 ~ "C4",
      merged_data$condition == 5 ~ "C5 and C6"
    )
  )

merged_data <- merged_data %>% 
  mutate(
    quality = case_when(
      merged_data$quality == 1 ~ "Q1",
      merged_data$quality == 2 ~ "Q2",
      merged_data$quality == 3 ~ "Q3",
      merged_data$quality == 4 ~ "Q4",
      merged_data$quality == 5 ~ "Q5 and Q6"
    )
  )

merged_data <- merged_data %>% 
  mutate(
    owner_occupied = case_when(
      merged_data$owner_occupied == 1 ~ "Yes",
      merged_data$owner_occupied == 2 ~ "No"
    )
  )

merged_data <- merged_data %>% 
  mutate(
    updated_last_15_years = case_when(
      merged_data$updated_last_15_years == 1 ~ "Yes",
      merged_data$updated_last_15_years == 2 ~ "No"
    )
  )

merged_data <- merged_data %>% 
  mutate(
    growth_rate = case_when(
      merged_data$growth_rate == 1 ~ "Rapid",
      merged_data$growth_rate == 2 ~ "Stable",
      merged_data$growth_rate == 3 ~ "Slow"
    )
  )

merged_data <- merged_data %>% 
  mutate(
    property_value_trend = case_when(
      merged_data$property_value_trend == 1 ~ "Increasing",
      merged_data$property_value_trend == 2 ~ "Stable",
      merged_data$property_value_trend == 3 ~ "Declining"
    )
  )

merged_data <- merged_data %>% 
  mutate(
    demand_supply = case_when(
      merged_data$demand_supply == 1 ~ "Shortage",
      merged_data$demand_supply == 2 ~ "In Balance",
      merged_data$demand_supply == 3 ~ "Over supply"
    )
  )

merged_data <- merged_data %>% 
  mutate(
    marketing_time = case_when(
      merged_data$marketing_time == 1 ~ "Under 3 Months",
      merged_data$marketing_time == 2 ~ "3 to 6 Months",
      merged_data$marketing_time == 3 ~ "Over 6 Months"
    )
  )

#looking at summary of the variables
summary(merged_data)

#variable selection for the analaysis
data_for_analysis <- merged_data[, c( "year","state_fips_2010","county_fips_2010" ,"tract_fips_2010",
                                                          "growth_rate", "owner_occupied", 
                                                          "property_value_trend", 
                                                          "built_up", "demand_supply", "marketing_time", 
                                                          "lot_size", "quality", "condition", 
                                                          "bathrooms","bedrooms", "updated_last_15_years", "gross_living_area", 
                                                          "dts_rural","appraisal_to_contract","appraised_value",
                                                          "contract_price")]

summary(data_for_analysis)





#renaming for acronym
data_for_analysis <- data_for_analysis %>%
  rename(   
    Yr = year,
    SFips = state_fips_2010,
    CFips = county_fips_2010,
    TFips = tract_fips_2010,
    GR = growth_rate,
    OO = owner_occupied,
    Pvt = property_value_trend,
    Bup = built_up,
    DS = demand_supply,
    LS = lot_size,
    Bath = bathrooms,
    Bed = bedrooms,
    Q = quality,                  
    Con = condition,             
    Upd = updated_last_15_years,  
    DR = dts_rural,
    GLA = gross_living_area,
    MT = marketing_time,
    ATC = appraisal_to_contract,
    AV = appraised_value,
    CP = contract_price
  )

summary(data_for_analysis)

#removing NAs
data_for_analysis <- na.omit(data_for_analysis)

#calculating contract to appraisal ratio
data_for_analysis$CTA <- round((data_for_analysis$CP/data_for_analysis$AV)*100, 1)


# counting the number of rows for each category
category_counts <- as.data.frame(table(data_for_analysis$MT))

# renaming Var1 to "Marketing Time"
category_counts <- category_counts %>% rename(`Marketing Time` = Var1)

# create a bar plot for marketing time
ggplot(category_counts, aes(x = `Marketing Time`, y = Freq, fill = `Marketing Time`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::comma(Freq)), vjust = -0.5, size = 4) +  # Adding labels above bars
  labs(title = "Distribution of Marketing Time", x = "Marketing Time", y = "Count") +
  theme_minimal()

#getting number of variables in Marketing Time
table(data_for_analysis$MT)

#converting "over 6 months" and "3 to 6 months" to "Over 3 months"
data_for_analysis <- data_for_analysis %>%
  mutate(MT = case_when(
    data_for_analysis$MT == "3 to 6 Months" ~ "Over 3 Months",
    data_for_analysis$MT == "Over 6 Months" ~ "Over 3 Months", 
    data_for_analysis$MT == "Under 3 Months" ~ "Under 3 Months", 
  ))

#rechecking categories of variables in Marketing Time
table(data_for_analysis$MT)


# Categorize ATC values
data_for_analysis <- data_for_analysis %>%
  mutate(CTA_Category = case_when(
    CTA > 100  ~ "Over 100",
    CTA < 100  ~ "Less than 100",
    CTA == 100 ~ "Equal to 100",
    TRUE ~ "Unknown"
  ))

#checking categories of variables in CTA
table(data_for_analysis$CTA)

ggplot(data_for_analysis, aes(x = CP, y = AV)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot of Appraised Value vs. Contract Price",
       x = "Contract Price",
       y = "Appraised Value") +
  theme_minimal()

# Count the number of rows in each category
category_counts <- data_for_analysis %>%
  count(CTA_Category)

# Create a bar plot with comma-formatted labels
ggplot(category_counts, aes(x = CTA_Category, y = n, fill = CTA_Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::comma(n)), vjust = -0.5, size = 4) +  # Add commas to data labels
  labs(title = "Distribution of Contract Price to Appraisal Value", 
       x = "Contract Price to Appraisal Value", 
       y = "Count") +
  theme_minimal()

data_premium <- data_for_analysis%>%
                                    filter(CTA_Category == "Over 100")

# grouping 'Year' and 'CTA' columns
cta_summary <- data_premium %>%
              group_by(Yr) %>%
              count()

# Plotting count of CTA over the years
ggplot(cta_summary, aes(x = as.factor(Yr), y = n)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +  # Bar chart
  geom_text(aes(label = scales::comma(n)), vjust = -0.5, size = 4) +  # Data labels
  labs(title = "Count of CTA Observations Over the Years",
       x = "Year",
       y = "Count of CTA") +
  theme_minimal()


# grouping 'Year' and 'CTA' columns
cta_summary <- data_premium %>%
  group_by(MT) %>%
  count()

# Plotting count of CTA over the years
ggplot(cta_summary, aes(x = as.factor(MT), y = n)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +  # Bar chart
  geom_text(aes(label = scales::comma(n)), vjust = -0.5, size = 4) +  # Data labels
  labs(title = "Count of CTA Observations Over the Years",
       x = "Marketing Time",
       y = "Count of CTA") +
  theme_minimal()


# grouping 'Year' and 'CTA' columns
cta_summary <- data_premium %>%
  group_by(MT) %>%
  count()

# Plotting count of CTA over the years
ggplot(cta_summary, aes(x = as.factor(MT), y = n)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +  # Bar chart
  geom_text(aes(label = scales::comma(n)), vjust = -0.5, size = 4) +  # Data labels
  labs(title = "Count of CTA Observations Over the Years",
       x = "Marketing Time",
       y = "Count of CTA") +
  theme_minimal()

# create a bar plot for marketing time
ggplot(cta_summary, aes(x = as.factor(MT), y = n, fill = MT)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::comma(n)), vjust = -0.5, size = 4) +  # Adding labels above bars
  labs(title = "Count of Premium Pricing by Marketing Time", x = "Marketing Time", y = "Count") +
  theme_minimal()

# grouping 'OO' and 'CTA' columns
cta_summary_oo <- data_premium %>%
  group_by(OO) %>%
  count()


# create a bar plot for Owner Occupied
ggplot(cta_summary_oo, aes(x = as.factor(OO), y = n, fill = OO)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::comma(n)), vjust = -0.5, size = 4) +  # Adding labels above bars
  labs(title = "Count of Premium Pricing by Owner Occupied", x = "Owner Occupied", y = "Count") +
  theme_minimal()


# grouping 'upd' and 'CTA' columns
cta_summary_upd <- data_premium %>%
  group_by(Upd) %>%
  count()


# create a bar plot for Remodeled house
ggplot(cta_summary_upd, aes(x = as.factor(Upd), y = n, fill = Upd)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::comma(n)), vjust = -0.5, size = 4) +  # Adding labels above bars
  labs(title = "Count of Premium Pricing by Remodeled property", x = "Remodeled property", y = "Count") +
  theme_minimal()

# grouping 'Rural' and 'CTA' columns
cta_summary_dr <- data_premium %>%
  group_by(DR) %>%
  count()


# create a bar plot for Rural and non-rural
ggplot(cta_summary_dr, aes(x = as.factor(DR), y = n, fill = DR)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::comma(n)), vjust = -0.5, size = 4) +  # Adding labels above bars
  labs(title = "Count of Premium Pricing by Rural Area", x = "Rural", y = "Count") +
  theme_minimal()



#only selecting categorical variables
df_categorical <- data_for_analysis [, c( "Yr","GR", "OC",
                                          "DS", "MT", 
                                          "LS", "Q", "Con", 
                                          "Bath","Bed", "Upd",
                                          "DR")]

df_numerical <- data_for_analysis [, c( "CP","AV",
                                        "CTA", "ATC")]

psych::describe(df_numerical)

#---------------------------------------------------------------------------------------------------------------------------------------------

# Question 2: What variables effect the marketing time

#changing into binary variables
data_for_analysis <- data_for_analysis %>% 
  mutate(
    MT = case_when(
      data_for_analysis$MT == "Over 3 Months" ~ 0, 
      data_for_analysis$MT == "Under 3 Months" ~ 1, 
    )
  )

table(data_for_analysis$GR)

# Make sure 'GR' is a factor and reorder levels so 'Stable' is the reference level
data_for_analysis$GR <- factor(data_for_analysis$GR, levels = c("Stable", "Slow", "Rapid"))

# Question 2: What variables effect the marketing time

# Fit the model with fixed effects - Year and County
model_question_hypothesis4 <- feglm(MT ~ CTA + DR + Upd + GR | Yr + CFips, 
                                 family = binomial(link = "logit"), 
                                 data = data_for_analysis)

# Summary of the model
summary(model_question_hypothesis4)

# Fit the null model (intercept-only model)
model_null <- feglm(MT ~ 1 | Yr + CFips, 
                    family = binomial(link = "logit"), 
                    data = data_for_analysis)

# Extract the log-likelihoods
logLik_null <- logLik(model_null)
logLik_full <- logLik(model_question_2_clogit)

# Likelihood ratio statistic: 2 * (log-likelihood of full model - log-likelihood of null model)
lrt_statistic <- 2 * (logLik_full - logLik_null)

# Degrees of freedom: difference in the number of parameters
df <- length(coef(model_question_2_clogit)) - length(coef(model_null))

# p-value for the likelihood ratio test (chi-squared distribution)
p_value <- round(1 - pchisq(lrt_statistic, df), 10000)

pchisq(lrt_statistic, df)

lrt_statistic
df
p_value

#odds ratio
exp(cbind(Odd_Ratio = coef(model_question_2_clogit)))

#---------------------------------------------------------------------------------------------------------------------------------------------

# Question 1: What variables effect the CTA

#regression model with Fixed effects - for hypothesis 1
#Upd, Bath, Bed and Quality as independent variable
model_question_CTA_hypothesis1<- plm(CTA ~  Upd + Bath + Bed + Q,
                                data = data_for_analysis,
                                index = c("Yr", "CFips"),
                                model = "within")

#showing model summary for hypothesis 1
summary(model_question_CTA_hypothesis1)


#regression model with Fixed effects - for hypothesis 2
#GR, Bath, Bed and Quality as independent variable
model_question_CTA_hypothesis2 <- plm(CTA ~  GR + DS + DR + Bed + Bath,
                                  data = data_for_analysis,
                                  index = c("Yr", "CFips"),
                                  model = "within")
#showing model summary for hypothesis 2
summary(model_question_CTA_hypothesis2)


#regression model with Fixed effects - for hypothesis 3
#DR, Lot Size, Bath, Bed and Quality as independent variable
model_question_CTA_hypothesis3 <- plm(CTA ~  DR + LS + Bath + Bed + Q,
                                  data = data_for_analysis,
                                  index = c("Yr", "CFips"),
                                  model = "within")
#showing model summary for hypothesis 3
summary(model_question_CTA_hypothesis3)


# levels in DR to show effects of non-rural area
data_for_analysis$DR <- factor(data_for_analysis$DR, levels = c("Yes", "No"))

model_question_CTA_OO <- plm(CTA ~  OO + Bath + Bed + Q,
                                data = data_for_analysis,
                                index = c("Yr", "CFips"),
                                model = "within")
#showing summary of data
summary(model_question_CTA_OO)

# levels in DR to show effects of non-rural area
data_for_analysis$DR <- factor(data_for_analysis$DR, levels = c("Yes", "No"))

#for logistic fixed effects - https://cran.r-project.org/web/packages/bife/vignettes/howto.html 
# https://lrberge.github.io/fixest/index.html  - Fixest
# https://cran.r-project.org/web/packages/fixest/readme/README.html - Fixest
