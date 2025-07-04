---
  title: "CCV_Lüder et al., 2025_demo"
---

# Clear environment
rm(list = ls())
# Install and load packages 
packages <- c("readxl", "dplyr", "psych")
installed <- packages %in% rownames(installed.packages()) if (any(!installed)) install.packages(packages[!installed])
lapply(packages, library, character.only = TRUE)

# Load dataset
library(readxl)
data_CCV <- read_excel("...")

# Extract the German and the Arabic subsample based on inclusion criteria
data_CCV_ge <- subset(data_CCV, group == "German" & residence == "Germany" & origin == "Germany")
data_CCV_ar <- subset(data_CCV, group == "Arabic" & residence == "Germany")
# Determine the sample sizes of the German and Arabic subsamples
n_german <- nrow(data_CCV_ge)
n_arabic <- nrow(data_CCV_ar)
# Combine both subsamples to a total sample
data_CCV <- rbind(data_CCV_ge, data_CCV_ar)
# Determine the total sample size 
n_total <- nrow(data_CCV)

# Descriptive statistics: Mean and standard deviation of age in the total sample, Arabic subsample, German subsample
mean(data_CCV$DE01_age, na.rm = TRUE) 
sd(data_CCV$DE01_age, na.rm = TRUE)
mean(data_CCV_ar$DE01_age, na.rm = TRUE)
sd(data_CCV_ar$DE01_age, na.rm = TRUE)
mean(data_CCV_ge$DE01_age, na.rm = TRUE)
sd(data_CCV_ge$DE01_age, na.rm = TRUE)
# Inferential statistics: Independent samples t-test to assess group differences in age
t.test(DE01_age ~ group, data = data_CCV, na.action = na.omit, var.equal = FALSE)

# Descriptive statistics:Frequency distribution of gender (0 = male, 1 = female, 2 = prefer not to say) in the total sample, Arabic subsample, German subsample
print(table(data_CCV$DE02))
print(table(data_CCV_ar$DE02))
print(table(data_CCV_ge$DE02))
# Inferential statistics: Group comparison excluding "2 = prefer not to say"
gender_table <- table(group = rep(c("AR", "GE"), times = c(sum(data_CCV_ar$DE02 %in% 0:1), sum(data_CCV_ge$DE02 %in% 0:1))), gender = c( data_CCV_ar$DE02[data_CCV_ar$DE02 %in% 0:1], data_CCV_ge$DE02[data_CCV_ge$DE02 %in% 0:1]))
print(gender_table)
print(chisq.test(gender_table))

# Descriptive statistics: Frequency distribution of country of origin in the total sample, Arabic subsample, German subsample
origin_a <- table(data_CCV_ar$DE03)
print(origin_a)
origin_g <- table(data_CCV_ge$DE03)
print(origin_g)
origin <- table(data_CCV$DE03)
print(origin)

# Descriptive statistics: Frequency distribution of migration status in the total sample, Arabic subsample, German subsample
# self-reported migration status: 1 = forcely displaced, 2 = immigrated, 99 = no response
migration_status <- table(data_CCV$DE12)
print(migration_status)
# Descriptive statistics: Reasons for displacement or migration (multiple response items)
# DE13_01 to DE13_07 represent predefined reasons:
reasondata <- c("DE13_01", "DE13_02", "DE13_03", "DE13_04", "DE13_05", "DE13_06", "DE13_07")
reasonname <- c("War", "Climate change", "Natural disasters", 
                "Persecution in home country", "Human rights violations", 
                "Education", "Other")
# Frequency distribution of reason for displacement or migration
# Data frame with variable codes, descriptions, and count of participants selecting each reason
counts <- sapply(reasondata, function(v) sum(data_CCV[[v]] == "1", na.rm = TRUE))
result_df <- data.frame(Variable = reasondata,Description = reasonname, Endorsed_Count = counts)
print(result_df)
# Exploration of open-text responses for "other reasons for displacement or migration"
other_reasons <- table(data_CCV$DE13_07a)
show(other_reasons)

# Descriptive statistics: Frequency distribution of current psychological distress (self-reported) in the total sample, Arabic subsample, German subsample
# DE18 captures responses to the question: "Are you currently experiencing psychological distress?" (0 = No, 1 = Yes) reflecting participants' subjective mental health status, not constituting clinical diagnosis
psychdis <- table(data_CCV$DE18)
print(psychdis)
psychdis_ar <- table(data_CCV_ar$DE18)
print(psychdis_ar)
psychdis_ge <- table(data_CCV_ge$DE18)
print(psychdis_ge)

# Descriptive statistics: Frequency distribution of current psychiatric or psychotherapeutic treatment in the total sample, Arabic subsample, German subsample  
# Variable DE16: 1 = currently receiving treatment; 0 = not receiving treatment
groups <- list(AR = data_CCV_ar,GE = data_CCV_ge, total = data_CCV)
therapy_table <- sapply(groups, function(df) { c(Therapy = sum(df$DE16 == 1, na.rm = TRUE), NoTherapy = sum(df$DE16 == 0, na.rm = TRUE))})
print(therapy_table)
# Inferential statistics: Group differences in treatment status
groups <- list(AR = data_CCV_ar,GE = data_CCV_ge)
therapy_table <- sapply(groups, function(df) {
  c(Therapy = sum(df$DE16 == 1, na.rm = TRUE),
    NoTherapy = sum(df$DE16 == 0, na.rm = TRUE))})
print(therapy_table)
# Pearson's Chi-squared test of independence
chisq.test(therapy_table)

# Descriptive statistics: Frequency distribution of traumatic events in the total sample
# Subset of participants from the total sample who reported exposure to at least one traumatic event according to the Life Events Checklist for DSM-5 (LEC-5), as direct experience or as a witness
# The same syntax can be applied to the Arabic (data_CCV_traumaar) and German subsamples (data_CCV_traumage)
data_CCV_trauma <- subset(data_CCV, (LE01_01_1 == 1 | LE01_01_2 == 1 |
                                       LE01_02_1 == 1 | LE01_02_2 == 1 |
                                       LE01_03_1 == 1 | LE01_03_2 == 1 |
                                       LE01_04_1 == 1 | LE01_04_2 == 1 |
                                       LE01_05_1 == 1 | LE01_05_2 == 1 |
                                       LE01_06_1 == 1 | LE01_06_2 == 1 |
                                       LE01_07_1 == 1 | LE01_07_2 == 1 |
                                       LE01_08_1 == 1 | LE01_08_2 == 1 |
                                       LE01_09_1 == 1 | LE01_09_2 == 1 |
                                       LE01_10_1 == 1 | LE01_10_2 == 1 |
                                       LE01_11_1 == 1 | LE01_11_2 == 1 |
                                       LE01_12_1 == 1 | LE01_12_2 == 1 |
                                       LE01_13_1 == 1 | LE01_13_2 == 1 |
                                       LE01_14_1 == 1 | LE01_14_2 == 1 |
                                       LE01_15_1 == 1 | LE01_15_2 == 1 |
                                       LE01_16_1 == 1 | LE01_16_2 == 1 |
                                       LE01_17_1 == 1 | LE01_17_2 == 1 ))
View(data_CCV_trauma)
nrow(data_CCV_trauma)
# Identification of experienced events according to the LEC-5
# Definition of the LEC-5 items
lec_items <- c(
  "Natural Disaster", 
  "Fire or Explosion", 
  "Transportation Accident", 
  "Serious Accident at Work, Home, or During Recreational Activity", 
  "Exposure to Toxic Substance", 
  "Physical Assault", 
  "Assault with a Weapon", 
  "Sexual Assault", 
  "Other Unwanted or Uncomfortable Sexual Experience", 
  "Combat or Exposure to a War Zone", 
  "Captivity (e.g., being kidnapped, abducted, held hostage, prisoner of war)", 
  "Life-Threatening Illness or Injury", 
  "Severe Human Suffering", 
  "Sudden Violent Death (e.g., homicide, suicide)", 
  "Sudden Accidental Death", 
  "Serious Injury, Harm, or Death You Caused to Someone Else", 
  "Any Other Very Stressful Event or Experience"
)
library(dplyr)
row.Sums1 <- function(data) {
  results <- data.frame(Item = lec_items)
  for (i in 01:17) {
    count <- data %>%
      filter(data[[paste0("LE01_", sprintf("%02d", i), "_1")]] == 1) %>%
      nrow()
    percentage <- (count / nrow(data)) * 100
    results[i, "Count"] <- count
    results[i, "Percentage"] <- percentage
  }
  return(results)
}
row.Sums2 <- function(data) {
  results <- data.frame(Item = lec_items)
  for (i in 01:17) {
    count <- data %>%
      filter(data[[paste0("LE01_", sprintf("%02d", i), "_2")]] == 1) %>%
      nrow()
    percentage <- (count / nrow(data)) * 100
    results[i, "Count"] <- count
    results[i, "Percentage"] <- percentage
  }
  return(results)
}
results_total <- cbind(row.Sums1(data_CCV_trauma), row.Sums2(data_CCV_trauma))

# Descriptive statistics: Average number of traumatic events (direct exposed (_1) and witnessed (_2)) per participant (including participants with events not classified as traumatic by the LEC-5 but reported as subjectively distressing)
data_CCV$count_events <- rowSums(
  data_CCV[, c(
    paste0("LE01_", sprintf("%02d", 1:17), "_1"),
    paste0("LE01_", sprintf("%02d", 1:17), "_2")
  )],
  na.rm = TRUE
)
total_sum <- sum(data_CCV$count_events, na.rm = TRUE)
mean(data_CCV$count_events)     
sd(data_CCV$count_events)        

data_CCV_ar$count_events <- rowSums(
  data_CCV_ar[, c(
    paste0("LE01_", sprintf("%02d", 1:17), "_1"),
    paste0("LE01_", sprintf("%02d", 1:17), "_2")
  )],
  na.rm = TRUE
)
total_sum <- sum(data_CCV_ar$count_events, na.rm = TRUE)
mean(data_CCV_ar$count_events)   
sd(data_CCV_ar$count_events)    

data_CCV_ge$count_events <- rowSums(
  data_CCV_ge[, c(
    paste0("LE01_", sprintf("%02d", 1:17), "_1"),
    paste0("LE01_", sprintf("%02d", 1:17), "_2")
  )],
  na.rm = TRUE
)
total_sum <- sum(data_CCV_ge$count_events, na.rm = TRUE)
mean(data_CCV_ge$count_events)   
sd(data_CCV_ge$count_events)    
# Inferential statistics: Comparison of average number of traumatic/ distressing events by group (Arabic vs. German)
t.test(count_events ~ group, data = data_CCV)

# Build PCL-5 sum score across the 20 PCL-5 items adding it as a new variable (PCLsum) in the total sample
# Run the same syntax for the Arabic and the German subsamples
data_CCV$PCLsum <- rowSums(data_CCV[, c("PCL_01", "PCL_02", "PCL_03", "PCL_04", "PCL_05", "PCL_06", "PCL_07", "PCL_08", "PCL_09", "PCL_10", "PCL_11", "PCL_12", "PCL_13", "PCL_14", "PCL_15", "PCL_16", "PCL_17", "PCL_18", "PCL_19", "PCL_20")], na.rm = FALSE)

# Quantitative assessment of normality: Shapiro–Wilk test (a left-skewed distribution is assumed; thus, normality is not expected)
shapiro.test(data_CCV$PCLsum)

# Qualitative assessment of normality: Visual comparison of the empirical distribution of PCL-5 sum scores (in red) with a theoretical normal distribution (in grey) for the total sample
# The same syntax can be applied to the Arabic and German subsamples
hist(data_CCV$PCLsum, freq = FALSE, main = "Distribution of PCL-5 Scores")
lines(density(na.omit(data_CCV$PCLsum)), col = "red", lwd = 2)
curve_x <- seq(min(data_CCV$PCLsum, na.rm = TRUE), max(data_CCV$PCLsum, na.rm = TRUE), length.out = 100)
curve_y <- dnorm(curve_x, mean = mean(data_CCV$PCLsum, na.rm = TRUE), sd = sd(data_CCV$PCLsum, na.rm = TRUE))
lines(curve_x, curve_y, col = "grey", lwd = 2)

# Descriptive statistics: Mean and standard deviation of PCL-5 sum scores in the total sample, Arabic subsample, German subsample
mean(data_CCV$PCLsum, na.rm = TRUE)
sd(data_CCV$PCLsum, na.rm = TRUE)
mean(data_CCV_ge$PCLsum, na.rm = TRUE)
sd(data_CCV_ge$PCLsum, na.rm = TRUE)
mean(data_CCV_ar$PCLsum, na.rm = TRUE)
sd(data_CCV_ar$PCLsum, na.rm = TRUE)
# Inferential statistics: Group comparison of PCL-5 sum scores between Arabic and German subsamples using the Wilcoxon rank-sum test (equivalent to the Mann–Whitney U test)
wilcox.test(PCLsum ~ group, data = data_CCV)


### Internal consistency of the PCL-5 in the total sample
## The same syntax can be applied to the Arabic and German subsamples
# Extract a dataset containing only the PCL-5 items and delete missings listwise
PCL<-na.omit(data_CCV[c(itemlist)])
PCL
nrow(PCL)
# Assess the internal consistency of the PCL-5 items using Cronbach's alpha
library(psych)
Cronbachs_alpha <- alpha(PCL)
Cronbachs_alpha

