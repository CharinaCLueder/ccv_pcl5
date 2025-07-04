---
title: "CCV_Lüder et al., 2025_CFA_MGCFA"
---

# Clear environment
  rm(list = ls())
# Install and load packages 
required <- c('haven', 'xlsx', 'semTools', 'semPlot', 'lavaan', 'dplyr', 
             'qgraph', 'readxl', 'writexl')
installed <- required %in% rownames(installed.packages())
if (any(!installed)) install.packages(required[!installed])
lapply(required, library, character.only = TRUE)

# Load and show dataset
library(readxl)
data_CCV <- read_excel("~/...")
data_CCV[data_CCV == -9] <- NA
View(data_CCV)

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

# Build PCL-5 sum score across the 20 PCL-5 items adding it as a new variable (PCLsum) to data_CCV
data_CCV$PCLsum <- rowSums(data_CCV[, c("PCL_01", "PCL_02", "PCL_03", "PCL_04", "PCL_05", "PCL_06", "PCL_07", "PCL_08", "PCL_09", "PCL_10", "PCL_11", "PCL_12", "PCL_13", "PCL_14", "PCL_15", "PCL_16", "PCL_17", "PCL_18", "PCL_19", "PCL_20")], na.rm = FALSE)


### Conduct Confirmatory Factor Analysis (CFA)
### The following CFA syntax was primarily run on the total sample (data_CCV)
### The same syntax was applied to the two subsample datasets: data_CCV_ar and data_CCV_ge

# Define model variables and fit indices for CFA
groupvar   <-'group'
itemlist <-c('PCL_01', 
             'PCL_02',  
             'PCL_03',  
             'PCL_04',  
             'PCL_05',  
             'PCL_06',  
             'PCL_07',  
             'PCL_08',  
             'PCL_09',  
             'PCL_10',  
             'PCL_11',  
             'PCL_12',  
             'PCL_13',  
             'PCL_14',  
             'PCL_15',  
             'PCL_16',  
             'PCL_17',  
             'PCL_18',  
             'PCL_19',  
             'PCL_20')
fitCoeffs<-c(
  'n',              # not included in fitMeasures
  'npar',
  'chisq',
  'df',
  'chisq_pdf',      # not included in fitMeasures
  'pvalue',
  'chisq.scaling.factor',
  'chisq.scaled',
  'df.scaled',
  'chisq_pdfs',     # not included in fitMeasures
  'pvalue.scaled',
  'cfi.scaled',
  'srmr',
  'tli.scaled',
  'rmsea.scaled',
  'rmsea.ci.lower.scaled',
  'rmsea.ci.upper.scaled')

# Prepare dataset for CFA: select variables, delete missings listwise
CFA_variables <- data_CCV[c("case", groupvar, itemlist, "PCLsum")]
PCL <- na.omit(CFA_variables)
# Determine the sample size after listwise deletion
nrow(PCL)

## Conduct CFA to test the DSM-5 Model (Model1)
# Specify DSM-5 model
dsm5 <- 'reexp=~ PCL_01 + PCL_02 + PCL_03 + PCL_04 + PCL_05
        avoid=~ PCL_06 + PCL_07
        nacm=~ PCL_08 + PCL_09 + PCL_10 + PCL_11 + PCL_12 + PCL_13 + PCL_14
        alarre=~ PCL_15 + PCL_16 + PCL_17 + PCL_18 + PCL_19 + PCL_20'
modname  <- paste0('cfa.DSM5')
# Load semTools package for generating measurement invariance syntax
library(semTools)
# Specify the CFA model using theta parameterization and WLSMV, recommended for ordinal data
lavsyn <- measEq.syntax(
  configural.model = dsm5,
  data             = PCL,            
  ordered          = itemlist,           
  meanstructure    = TRUE,                
  ID.cat           = 'Wu.2016',           
  parameterization = 'theta',            
  estimator        = 'WLSMV',            
  return.fit       = FALSE)       
lavsyn
# Detach semTools package to avoid conflicts with lavaan (comments on packages will not be repeated)
detach(package:semTools)
# Convert the model syntax to a string and save it to a text file
lavsyn <-as.character(lavsyn)     
cat(lavsyn)
filename <- paste0('cfa.DSM5.','lavsyn.',modname,'.txt')
filename
write.table(lavsyn, filename)
# Load the lavaan package for structural equation modeling (comments on packages will not be repeated)
library(lavaan)
# Estimate the CFA model and run CFA, treating all variables in 'PCL' as ordered categorical (ordinal) data
lavmod <- cfa(model   = lavsyn,         
              data    = PCL,        
              ordered = names(PCL))  
# Save the CFA results 
filename <- paste0('cfa.DSM5.','lavmod.',modname,'.rds')
filename
saveRDS(lavmod,filename)
# Reload the CFA results
lavmod <- readRDS(filename) 
# Check for local independence of residuals in the CFA model by iteratively adjusting the threshold criterion, with 0.05 as a primary cutoff (rmax).
# overall, residual correlations below |0.1| are considered indicative of local independence - thresholds  inspired by Cohen's guidelines for interpreting correlation magnitude (r > |0.1| small, |0.3| medium, |0.5| large)
library(lavaan)
# Extract Bentler standardized residual correlations
rescorr     <- residuals(lavmod, type='cor.bentler')$cov
rescorr
# Display only residual correlations above rmax, here set to 0.09, starting with the primary cutoff of 0.05.
rmax        <- 0.09
tmp<-abs(rescorr)>rmax                  
tmp[tmp==FALSE]<-NA                    
tmp[upper.tri(tmp, diag = TRUE)] <-NA   
tmp<-tmp*rescorr                       
print.table(tmp,na.print="")           
detach(package:lavaan)
# Largest residual correlation: r(PCL_10, PCL_13) = -0.0954 --> Indicates that local independence of residuals is still given.

# Load required packages
library(lavaan)
library(semTools)
# Extract model fit indices from the CFA  
fitMeasures(lavmod) 
# Create a summary data frame of fit indices
fitMeasures  <- data.frame(value=fitMeasures(lavmod))
# Calculate additional values and fit indices not provided by fitMeasures(), such as sample size and (scaled) chi-square/df ratios  
nobs    <- unlist(lavmod@Data@nobs)     
nobs
chisq      <- fitMeasures['chisq',]
df         <- fitMeasures['df',]       
chisq_pdf  <- chisq/df 
chisqs     <- fitMeasures['chisq.scaled',]
dfs        <- fitMeasures['df.scaled',]       
chisq_pdfs <- chisqs/dfs
# Add the additionally calculated values and fit indices to fitMeasures 
fitMeasures   <- rbind(fitMeasures, 
                       n          = nobs,
                       chisq_pdf  = chisq_pdf,
                       chisq_pdfs = chisq_pdfs) 
fitMeasures
# Extract the selected fit indices, format them into a table, and assign the result to "Model1"
fittable <- data.frame(value=fitMeasures[fitCoeffs,]) 
rownames(fittable) <- fitCoeffs
colnames(fittable) <- modname 
fittable           <- t(fittable)
fittable
Model1 <- fittable



## Conduct CFA to test the Dysphoria Model (Model2)
# Specify the Dysphoria Model
dysph <- 'reexp=~ PCL_01 + PCL_02 + PCL_03 + PCL_04 + PCL_05
          avoid=~ PCL_06 + PCL_07
          dysar=~ PCL_08 + PCL_09 + PCL_10 + PCL_11 + PCL_12 + PCL_13 + PCL_14 + PCL_15 + PCL_19  + PCL_20
          alarre=~ PCL_16 + PCL_17 + PCL_18'
modname  <- paste0('cfa.Dysph')

library(semTools)
# Specify the CFA model using theta parameterization and WLSMV, recommended for ordinal data   
lavsyn <- measEq.syntax(
  configural.model = dysph,
  data             = PCL,            
  ordered          = itemlist,           
  meanstructure    = TRUE,                
  ID.cat           = 'Wu.2016',           
  parameterization = 'theta',            
  estimator        = 'WLSMV',            
  return.fit       = FALSE)       
detach(package:semTools)

# Convert the model syntax to a string and save it to a text file
lavsyn <-as.character(lavsyn)     
cat(lavsyn)
filename <- paste0('cfa.Dysph.','lavsyn.',modname,'.txt')
filename
write.table(lavsyn, filename)

library(lavaan)
# Estimate the CFA model and run CFA, treating all variables in 'PCL' as ordered categorical (ordinal) data
lavmod <- cfa(model   = lavsyn,         
              data    = PCL,        
              ordered = names(PCL))  
# Save the CFA results 
filename <- paste0('cfa.Dysph.','lavmod.',modname,'.rds')
filename
saveRDS(lavmod,filename)
# Reload the CFA results
lavmod <- readRDS(filename) 
# Check for local independence of residuals in the CFA model 
library(lavaan)
rescorr     <- residuals(lavmod, type='cor.bentler')$cov
rescorr
rmax        <- 0.1
tmp<-abs(rescorr)>rmax                 
tmp[tmp==FALSE]<-NA                    
tmp[upper.tri(tmp, diag = TRUE)] <-NA   
tmp<-tmp*rescorr                      
print.table(tmp,na.print="")           
detach(package:lavaan)

library(lavaan)
library(semTools)
# Extract model fit indices from the CFA  
fitMeasures(lavmod) 
# Create a summary data frame of fit indices
fitMeasures  <- data.frame(value=fitMeasures(lavmod))
# Calculate additional values and fit indices not provided by fitMeasures(), such as sample size and (scaled) chi-square/df ratios  
nobs    <- unlist(lavmod@Data@nobs)     
nobs
chisq      <- fitMeasures['chisq',]
df         <- fitMeasures['df',]       
chisq_pdf  <- chisq/df 
chisqs     <- fitMeasures['chisq.scaled',]
dfs        <- fitMeasures['df.scaled',]       
chisq_pdfs <- chisqs/dfs
# Add the additionally calculated values and fit indices to fitMeasures 
fitMeasures   <- rbind(fitMeasures, 
                       n          = nobs,
                       chisq_pdf  = chisq_pdf,
                       chisq_pdfs = chisq_pdfs) 
fitMeasures
# Extract the selected fit indices, format them into a table, and assign the result to "Model2"
fittable <- data.frame(value=fitMeasures[fitCoeffs,]) 
rownames(fittable) <- fitCoeffs
colnames(fittable) <- modname 
fittable           <- t(fittable)
fittable
Model2 <- fittable


## Conduct CFA to test the Dysphoric Arousal Model (Model3)
# Specify the Dysphoric Arousal Model
dysphar  <- 'reexp=~ PCL_01 + PCL_02 + PCL_03 + PCL_04 + PCL_05
        avoid=~ PCL_06 + PCL_07
        nacm=~ PCL_08 + PCL_09 + PCL_10 + PCL_11 + PCL_12 + PCL_13 + PCL_14
        dysar=~ PCL_15 + PCL_16 + PCL_19 + PCL_20
        anxar=~ PCL_17 + PCL_18'
modname  <- paste0('cfa.Dysphar')

library(semTools)
# Specify the CFA model using theta parameterization and WLSMV, recommended for ordinal data   
lavsyn <- measEq.syntax(
  configural.model = dysphar,
  data             = PCL,            
  ordered          = itemlist,           
  meanstructure    = TRUE,                
  ID.cat           = 'Wu.2016',           
  parameterization = 'theta',            
  estimator        = 'WLSMV',            
  return.fit       = FALSE)       
lavsyn
detach(package:semTools)
# Convert the model syntax to a string and save it to a text file
lavsyn <-as.character(lavsyn)     
cat(lavsyn)
filename <- paste0('cfa.Dysphar.','lavsyn.',modname,'.txt')
filename
write.table(lavsyn, filename)

library(lavaan)
# Estimate the CFA model and run CFA, treating all variables in 'PCL' as ordered categorical (ordinal) data
lavmod <- cfa(model   = lavsyn,         
              data    = PCL,        
              ordered = names(PCL))
# Save the CFA results 
filename <- paste0('cfa.Dysphar.','lavmod.',modname,'.rds')
filename
saveRDS(lavmod,filename)
# Reload the CFA results
lavmod <- readRDS(filename) 
# Check for local independence of residuals in the CFA model 
library(lavaan)
rescorr     <- residuals(lavmod, type='cor.bentler')$cov
rescorr
rmax        <- 0.09
tmp<-abs(rescorr)>rmax                 
tmp[tmp==FALSE]<-NA                    
tmp[upper.tri(tmp, diag = TRUE)] <-NA   
tmp<-tmp*rescorr                      
print.table(tmp,na.print="")           
detach(package:lavaan)

library(lavaan)
library(semTools)
# Extract model fit indices from the CFA  
fitMeasures(lavmod) 
# Create a summary data frame of fit indices
fitMeasures  <- data.frame(value=fitMeasures(lavmod))
# Calculate additional values and fit indices not provided by fitMeasures(), such as sample size and (scaled) chi-square/df ratios  
nobs    <- unlist(lavmod@Data@nobs)     
nobs
chisq      <- fitMeasures['chisq',]
df         <- fitMeasures['df',]       
chisq_pdf  <- chisq/df 
chisqs     <- fitMeasures['chisq.scaled',]
dfs        <- fitMeasures['df.scaled',]       
chisq_pdfs <- chisqs/dfs
# Add the additionally calculated values and fit indices to fitMeasures 
fitMeasures   <- rbind(fitMeasures, 
                       n          = nobs,
                       chisq_pdf  = chisq_pdf,
                       chisq_pdfs = chisq_pdfs) 
fitMeasures
# Extract the selected fit indices, format them into a table, and assign the result to "Model3"
fittable <- data.frame(value=fitMeasures[fitCoeffs,]) 
rownames(fittable) <- fitCoeffs
colnames(fittable) <- modname 
fittable           <- t(fittable)
fittable
Model3 <- fittable



## Conduct CFA to test the Anhedonia Model (Model4)
# Specify the Anhedonia Model
anhed <- 'reexp=~ PCL_01 + PCL_02 + PCL_03 + PCL_04 + PCL_05
        avoid=~ PCL_06 + PCL_07
        nacm=~ PCL_08 + PCL_09 + PCL_10 + PCL_11
        anhed=~ PCL_12 + PCL_13 + PCL_14
        dysar=~ PCL_15 + PCL_16 + PCL_19 + PCL_20
        anxar=~ PCL_17 + PCL_18'
modname  <- paste0('cfa.Anhed')

library(semTools)
# Specify the CFA model using theta parameterization and WLSMV, recommended for ordinal data   
lavsyn.4 <- measEq.syntax(
  configural.model = anhed,
  data             = PCL,            
  ordered          = itemlist,           
  meanstructure    = TRUE,                
  ID.cat           = 'Wu.2016',           
  parameterization = 'theta',            
  estimator        = 'WLSMV',            
  return.fit       = FALSE)       
lavsyn.4
detach(package:semTools)
# Convert the model syntax to a string and save it to a text file
lavsyn.4 <-as.character(lavsyn.4)     
cat(lavsyn.4)
filename <- paste0('cfa.Anhed.','lavsyn.4.',modname,'.txt')
filename
write.table(lavsyn.4, filename)

library(lavaan)
# Estimate the CFA model and run CFA, treating all variables in 'PCL' as ordered categorical (ordinal) data
lavmod.4 <- cfa(model   = lavsyn.4,         
                data    = PCL,        
                ordered = names(PCL))  
# Save the CFA results 
filename <- paste0('cfa.Anhed.','lavmod.4.',modname,'.rds')
filename
saveRDS(lavmod.4,filename)
# Reload the CFA results
lavmod.4 <- readRDS(filename) 
# Check for local independence of residuals in the CFA model 
library(lavaan)
rescorr     <- residuals(lavmod.4, type='cor.bentler')$cov
rescorr
rmax        <- 0.09
tmp<-abs(rescorr)>rmax                 
tmp[tmp==FALSE]<-NA                   
tmp[upper.tri(tmp, diag = TRUE)] <-NA   
tmp<-tmp*rescorr                      
print.table(tmp,na.print="")           
detach(package:lavaan)

library(lavaan)
library(semTools)
# Extract model fit indices from the CFA  
fitMeasures(lavmod.4) 
# Create a summary data frame of fit indices
fitMeasures  <- data.frame(value=fitMeasures(lavmod.4))
# Calculate additional values and fit indices not provided by fitMeasures(), such as sample size and (scaled) chi-square/df ratios  
nobs    <- unlist(lavmod.4@Data@nobs)    
nobs
chisq      <- fitMeasures['chisq',]
df         <- fitMeasures['df',]       
chisq_pdf  <- chisq/df 
chisqs     <- fitMeasures['chisq.scaled',]
dfs        <- fitMeasures['df.scaled',]       
chisq_pdfs <- chisqs/dfs
# Add the additionally calculated values and fit indices to fitMeasures 
fitMeasures   <- rbind(fitMeasures, 
                       n          = nobs,
                       chisq_pdf  = chisq_pdf,
                       chisq_pdfs = chisq_pdfs) 
fitMeasures
# Extract the selected fit indices, format them into a table, and assign the result to "Model4"
fittable <- data.frame(value=fitMeasures[fitCoeffs,]) 
rownames(fittable) <- fitCoeffs
colnames(fittable) <- modname 
fittable           <- t(fittable)
fittable
Model4 <- fittable



## Conduct CFA to test the Externalizing Behaviour Model (Model5)
# Specify the Externalizing Behaviour Model
extbe <- 'reexp=~ PCL_01 + PCL_02 + PCL_03 + PCL_04 + PCL_05
        avoid=~ PCL_06 + PCL_07
        nacm=~ PCL_08 + PCL_09 + PCL_10 + PCL_11 + PCL_12 + PCL_13 + PCL_14
        extbe=~ PCL_15 + PCL_16
        dysar=~ PCL_19 + PCL_20
        anxar=~ PCL_17 + PCL_18'
modname  <- paste0('cfa.Extbe')

library(semTools)
# Specify the CFA model using theta parameterization and WLSMV, recommended for ordinal data   
lavsyn <- measEq.syntax(
  configural.model = extbe,
  data             = PCL,            
  ordered          = itemlist,           
  meanstructure    = TRUE,                
  ID.cat           = 'Wu.2016',           
  parameterization = 'theta',            
  estimator        = 'WLSMV',            
  return.fit       = FALSE)       
lavsyn
detach(package:semTools)
# Convert the model syntax to a string and save it to a text file
lavsyn <-as.character(lavsyn)     
cat(lavsyn)
filename <- paste0('cfa.Extbe.','lavsyn.',modname,'.txt')
filename
write.table(lavsyn, filename)

library(lavaan)
# Estimate the CFA model and run CFA, treating all variables in 'PCL' as ordered categorical (ordinal) data
lavmod <- cfa(model   = lavsyn,         
              data    = PCL,        
              ordered = names(PCL)) 
# Save the CFA results 
filename <- paste0('cfa.Extbe.','lavmod.',modname,'.rds')
filename
saveRDS(lavmod,filename)
# Reload the CFA results
lavmod <- readRDS(filename) 
# Check for local independence of residuals in the CFA model 
library(lavaan)
rescorr     <- residuals(lavmod, type='cor.bentler')$cov
rescorr
rmax        <- 0.09
tmp<-abs(rescorr)>rmax                
tmp[tmp==FALSE]<-NA                    
tmp[upper.tri(tmp, diag = TRUE)] <-NA   
tmp<-tmp*rescorr                      
print.table(tmp,na.print="")          
detach(package:lavaan)

library(lavaan)
library(semTools)
# Extract model fit indices from the CFA  
fitMeasures(lavmod) 
# Create a summary data frame of fit indices
fitMeasures  <- data.frame(value=fitMeasures(lavmod))
# Calculate additional values and fit indices not provided by fitMeasures(), such as sample size and (scaled) chi-square/df ratios  
nobs    <- unlist(lavmod.4@Data@nobs)    
nobs
chisq      <- fitMeasures['chisq',]
df         <- fitMeasures['df',]       
chisq_pdf  <- chisq/df 
chisqs     <- fitMeasures['chisq.scaled',]
dfs        <- fitMeasures['df.scaled',]       
chisq_pdfs <- chisqs/dfs
# Add the additionally calculated values and fit indices to fitMeasures 
fitMeasures   <- rbind(fitMeasures, 
                       n          = nobs,
                       chisq_pdf  = chisq_pdf,
                       chisq_pdfs = chisq_pdfs) 
fitMeasures
# Extract the selected fit indices, format them into a table, and assign the result to "Model5"
fittable <- data.frame(value=fitMeasures[fitCoeffs,]) 
rownames(fittable) <- fitCoeffs
colnames(fittable) <- modname 
fittable           <- t(fittable)
fittable
Model5 <- fittable


## Conduct CFA to test the Hybrid Model(Model6)
# Specify the Hybrid Model
hybrid <- 'reexp=~ PCL_01 + PCL_02 + PCL_03 + PCL_04 + PCL_05
        avoid=~ PCL_06 + PCL_07
        negaf=~ PCL_08 + PCL_09 + PCL_10 + PCL_11
        anhed=~ PCL_12 + PCL_13 + PCL_14
        extbe=~ PCL_15 + PCL_16
        dysar=~ PCL_19 + PCL_20
        anxar=~ PCL_17 + PCL_18'
modname  <- paste0('cfa.Hybrid')

library(semTools)
# Specify the CFA model using theta parameterization and WLSMV, recommended for ordinal data   
lavsyn.6 <- measEq.syntax(
  configural.model = hybrid,
  data             = PCL,            
  ordered          = itemlist,           
  meanstructure    = TRUE,                
  ID.cat           = 'Wu.2016',           
  parameterization = 'theta',            
  estimator        = 'WLSMV',            
  return.fit       = FALSE)       
lavsyn.6
detach(package:semTools)
# Convert the model syntax to a string and save it to a text file
lavsyn.6 <-as.character(lavsyn.6)     
cat(lavsyn.6)
filename <- paste0('cfa.Hybrid.','lavsyn.6.',modname,'.txt')
filename
write.table(lavsyn.6, filename)

library(lavaan)
# Estimate the CFA model and run CFA, treating all variables in 'PCL' as ordered categorical (ordinal) data
lavmod.6 <- cfa(model   = lavsyn.6,         
                data    = PCL,        
                ordered = names(PCL))
# Save the CFA results 
filename <- paste0('cfa.Hybrid.','lavmod.6.',modname,'.rds')
filename
saveRDS(lavmod.6,filename)
# Reload the CFA results
lavmod.6 <- readRDS(filename) 
# Check for local independence of residuals in the CFA model 
library(lavaan)
rescorr     <- residuals(lavmod.6, type='cor.bentler')$cov
rescorr
rmax        <- 0.09
tmp<-abs(rescorr)>rmax                 
tmp[tmp==FALSE]<-NA                   
tmp[upper.tri(tmp, diag = TRUE)] <-NA   
tmp<-tmp*rescorr                       
print.table(tmp,na.print="")           
detach(package:lavaan)

library(lavaan)
library(semTools)
# Extract model fit indices from the CFA  
fitMeasures(lavmod.6) 
# Create a summary data frame of fit indices
fitMeasures  <- data.frame(value=fitMeasures(lavmod.6))
# Calculate additional values and fit indices not provided by fitMeasures(), such as sample size and (scaled) chi-square/df ratios  
nobs    <- unlist(lavmod.6@Data@nobs)     
nobs
chisq      <- fitMeasures['chisq',]
df         <- fitMeasures['df',]       
chisq_pdf  <- chisq/df 
chisqs     <- fitMeasures['chisq.scaled',]
dfs        <- fitMeasures['df.scaled',]       
chisq_pdfs <- chisqs/dfs
# Add the additionally calculated values and fit indices to fitMeasures 
fitMeasures   <- rbind(fitMeasures, 
                       n          = nobs,
                       chisq_pdf  = chisq_pdf,
                       chisq_pdfs = chisq_pdfs) 
fitMeasures
# Extract the selected fit indices, format them into a table, and assign the result to "Model6"
fittable <- data.frame(value=fitMeasures[fitCoeffs,]) 
rownames(fittable) <- fitCoeffs
colnames(fittable) <- modname 
fittable           <- t(fittable)
fittable
Model6 <- fittable



### Model Comparison within CFA
### Comparing alternative factor structures based on the fit indices

## Exporting fit statistics from all CFA models
library(haven)
library(dplyr)
library(writexl)
library(openxlsx)
# Combine fit indices from all specified CFA models into a single data frame
fittable<-rbind(Model1, Model2, Model3, Model4, Model5, Model6)
fittable
outputdata <- fittable
sheetname  <- paste0('fitTable')
filename   <- paste0('CFA_results.', sheetname,'.xlsx')
filename

write.xlsx(
  x = data.frame(outputdata), 
  file = filename, 
  append = FALSE,   
  quote = TRUE,     
  sep = "x",       
  eol = "\r",       
  na = "NA",        
  dec = ".",       
  rowNames = TRUE,  
  colNames = TRUE, 
  sheetName = sheetname,
  qmethod = ("escape")
)
detach(package:haven)
detach(package:dplyr)
detach(package:writexl)
detach(package:openxlsx)

## Exporting fit indices for the best-fitting models (Anhedonia model (Model4) and Hybrid model (Model6))
library(haven)
library(dplyr)
library(writexl)
library(openxlsx)
# Combine fit indices from Model4 and Model6 into one single data frame for comparative analysis
fittable <- rbind(Model4, Model6)
fittable
outputdata <- fittable
sheetname  <- paste0('fitTable')
filename   <- paste0('CFA_compare.', sheetname,'.xlsx')
filename
write.xlsx(
  x = data.frame(outputdata), 
  file = filename, 
  append = FALSE,   
  quote = TRUE,     
  sep = "x",        
  eol = "\r",       
  na = "NA",        
  dec = ".",        
  rowNames = TRUE,  
  colNames = TRUE,  
  sheetName = sheetname,
  qmethod = ("escape")
)
detach(package:haven)
detach(package:dplyr)
detach(package:writexl)
detach(package:openxlsx)
# Load the previously exported fit table
library(readxl)
CFA_compare_fitTable <- read_excel("CFA_compare.fitTable.xlsx")
View(CFA_compare_fitTable)
# Perform Satorra-Bentler scaled chi-square difference test between Model4 and Model6
library(lavaan)
satorra_bentler_test <- lavTestLRT(lavmod.4, lavmod.6)
satorra_bentler_test
# Perform Delta CFI
# Extract CFI values from the fit table (cfi_4 = CFI for Anhedonia Model (Model4); cfi_6 = CFI for Hybrid Model (Model6))
cfi <- CFA_compare_fitTable$cfi.scaled
cfi_4 <- cfi[1]
cfi_4
cfi_6 <- cfi[2]
cfi_6
# Calculate the change in CFI between the two models (ΔCFI < 0.01 → trivial change; 0.01 ≤ ΔCFI < 0.02 → moderate change; ΔCFI ≥ 0.02 → substantial change)
delta_cfi <- cfi_4 - cfi_6
delta_cfi

## Plot the best fitting models
# Plot the Anhedonia Model (Model4)

library(semPlot)
library(qgraph)
semPaths(lavmod.4, 
         intercepts = FALSE, 
         residuals = TRUE, 
         whatLabels = "std",
         layout = "tree",      
         sizeMan = 10,         
         sizeLat = 15,         
         curvePivot = TRUE,    
         edge.label.cex = 1.5, 
         node.label.cex = 1.5, 
         sep.manifests = 2     
)
# Plot the Hybrid Model (Model6)

library(semPlot)
library(qgraph)
semPaths(lavmod.6, 
         intercepts = FALSE, 
         residuals = TRUE, 
         whatLabels = "std",
         layout = "tree",      
         sizeMan = 10,         
         sizeLat = 15,         
         curvePivot = TRUE,    
         edge.label.cex = 1.5, 
         node.label.cex = 1.5, 
         sep.manifests = 2     
)



### Conduct Multiple-Group Confirmatory Factor Analysis (MGCFA) for the best fitting models
### The following MGCFA syntax is primarily applied to the Anhedonia model
### The same syntax was subsequently applied to the Hybrid model by replacing the input model accordingly

# Determine the number of groups in the dataset
ngroups<-length(unique(PCL$group))
ngroups

### MGCFA Anhedonia model 

## Set model name for configural invariance (baseline model): M0
modname   <- paste0('M0.con')
# Specify the configural invariance model based on the Anhedonia model
library(semTools)
lavsyn <- measEq.syntax(
  configural.model = anhed,               
  data             = dataset,             
  ordered          = itemlist,            
  meanstructure    = TRUE,                
  ID.cat           = 'Wu.2016',           
  parameterization = 'theta',             
  estimator        = 'WLSMV',             
  group.equal      = c('configural'),    
  sample.nobs      = rep(1,ngroups),    
  group            = groupvar,           
  return.fit       = FALSE) 
# Convert the model syntax to a string and save it to a text file
lavsyn <- as.character(lavsyn)     
cat(lavsyn)
filename <- paste0('M0_Anhed_configural_result.','lavsyn.',modname,'.txt')
filename
write.table(lavsyn, filename)
# Estimate and save the configural invariance model based on the Anhedonia model
library(lavaan)
lavmod <- cfa(model   = lavsyn,          
              data    = dataset,         
              ordered = itemlist,        
              group  = 'group') 
lavmod
filename <- paste0('M0_Anhed_configural.','lavmod.',modname,'.rds')
filename
saveRDS(lavmod,filename)
# Reload the results
lavmod <- readRDS(filename)
# Extract model fit indices and create a summary data frame containing all fit indices
fitMeasures <- data.frame(value=fitMeasures(lavmod))
fitMeasures
# Calculate additional values and fit indices not provided by fitMeasures()
n    <- nrow(dataset)  
n
chisq      <- fitMeasures['chisq',]
df         <- fitMeasures['df',]       
chisq_pdf  <- chisq/df 
chisqs     <- fitMeasures['chisq.scaled',]
dfs        <- fitMeasures['df.scaled',]       
chisq_pdfs <- chisqs/dfs
# Add the additionally calculated values and fit indices to fitMeasures 
fitMeasures   <- rbind(fitMeasures, 
                       n = n,
                       chisq_pdf  = chisq_pdf,
                       chisq_pdfs = chisq_pdfs) 
fitMeasures
# Extract the selected fit indices, format them into a table, and assign the result to "M0_configural"
fittable <- data.frame(value=fitMeasures[fitCoeffs,]) 
rownames(fittable) <- fitCoeffs
colnames(fittable) <- modname 
fittable           <- t(fittable)
fittable
M0_configural <- fittable

## Set model name for threshold invariance: M1
modname   <- paste0('M1.thresh')
# Specify the threshold invariance model based on the Anhedonia model
library(semTools)
lavsyn <- measEq.syntax(
  configural.model = anhed,               
  data             = dataset,             
  ordered          = itemlist,            
  meanstructure    = TRUE,                
  ID.cat           = 'Wu.2016',           
  parameterization = 'theta',             
  estimator        = 'WLSMV',             
  sample.nobs      = rep(1,ngroups),      
  group            = groupvar,             
  group.equal      = c('thresholds'),     
  return.fit       = FALSE) 
# Convert the model syntax to a string and save it to a text file
lavsyn <- as.character(lavsyn)     
cat(lavsyn)
filename <- paste0('M1_Anhed_thresholds_result.','lavsyn.',modname,'.txt')
filename
write.table(lavsyn, filename)
# Estimate and save the threshold invariance model based on the Anhedonia model
library(lavaan)
lavmod.th <- cfa(model   = lavsyn,          
                 data    = dataset,         
                 ordered = itemlist,  
                 group  = 'group') 
lavmod.th
filename <- paste0('M1_Anhed_thresholds.','lavmod.',modname,'.rds')
filename
saveRDS(lavmod.th,filename)
# Reload the results
lavmod.th <- readRDS(filename)
# Extract model fit indices and create a summary data frame containing all fit indices
fitMeasures <- data.frame(value=fitMeasures(lavmod.th))
fitMeasures
# Calculate additional values and fit indices not provided by fitMeasures()
n    <- nrow(dataset)  
n
chisq      <- fitMeasures['chisq',]
df         <- fitMeasures['df',]       
chisq_pdf  <- chisq/df 
chisqs     <- fitMeasures['chisq.scaled',]
dfs        <- fitMeasures['df.scaled',]       
chisq_pdfs <- chisqs/dfs
# Add the additionally calculated values and fit indices to fitMeasures 
fitMeasures   <- rbind(fitMeasures, 
                       n = n,
                       chisq_pdf  = chisq_pdf,
                       chisq_pdfs = chisq_pdfs) 
fitMeasures
# Extract the selected fit indices, format them into a table, and assign the result to "M1_thresholds"
fittable <- data.frame(value=fitMeasures[fitCoeffs,]) 
rownames(fittable) <- fitCoeffs
colnames(fittable) <- modname 
fittable           <- t(fittable)
fittable
M1_thresholds <- fittable

## Set model name for metric invariance: M2
modname   <- paste0('M2.metric')
# Specify the metric invariance model based on the Anhedonia model
library(semTools)
lavsyn <- measEq.syntax(
  configural.model = anhed,                 
  data             = dataset,              
  ordered          = itemlist,             
  meanstructure    = TRUE,                
  ID.cat           = 'Wu.2016',            
  parameterization = 'theta',             
  estimator        = 'WLSMV',             
  sample.nobs      = rep(1,ngroups),      
  group            = groupvar,             
  group.equal      = c('thresholds','loadings'), 
  return.fit       = FALSE) 
# Convert the model syntax to a string and save it to a text file
lavsyn <- as.character(lavsyn)     
cat(lavsyn)
filename <- paste0('M2_Anhed_metric_result.','lavsyn.',modname,'.txt')
filename
write.table(lavsyn, filename)
# Estimate and save the metric invariance model based on the Anhedonia model
library(lavaan)
lavmod.metric <- cfa(model   = lavsyn,          
                     data    = dataset,         
                     ordered = itemlist,  
                     group  = 'group') 
lavmod.metric
filename <- paste0('M2_Anhed_metric.','lavmod.',modname,'.rds')
filename
saveRDS(lavmod.metric,filename)
# Reload the results
lavmod.metric <- readRDS(filename)
# Extract model fit indices and create a summary data frame containing all fit indices
fitMeasures <- data.frame(value=fitMeasures(lavmod.metric))
fitMeasures
# Calculate additional values and fit indices not provided by fitMeasures()
n    <- nrow(dataset)  
n
chisq      <- fitMeasures['chisq',]
df         <- fitMeasures['df',]       
chisq_pdf  <- chisq/df 
chisqs     <- fitMeasures['chisq.scaled',]
dfs        <- fitMeasures['df.scaled',]       
chisq_pdfs <- chisqs/dfs
# Add the additionally calculated values and fit indices to fitMeasures 
fitMeasures   <- rbind(fitMeasures, 
                       n = n,
                       chisq_pdf  = chisq_pdf,
                       chisq_pdfs = chisq_pdfs)
fitMeasures
# Extract the selected fit indices, format them into a table, and assign the result to "M2_metric"
fittable <- data.frame(value=fitMeasures[fitCoeffs,]) 
rownames(fittable) <- fitCoeffs
colnames(fittable) <- modname 
fittable           <- t(fittable)
fittable
M2_metric <- fittable

## Set model name for scalar invariance: M3
modname   <- paste0('M3.scalar')
# Specify the scalar invariance model based on the Anhedonia model
library(semTools)
lavsyn <- measEq.syntax(
  configural.model = anhed,                 
  data             = dataset,              
  ordered          = itemlist,             
  meanstructure    = TRUE,                 
  ID.cat           = 'Wu.2016',            
  parameterization = 'theta',              
  estimator        = 'WLSMV',              
  sample.nobs      = rep(1,ngroups),       
  group            = groupvar,             
  group.equal      = c('thresholds','loadings','intercepts'),  
  return.fit       = FALSE) 
# Convert the model syntax to a string and save it to a text file
lavsyn <- as.character(lavsyn)
cat(lavsyn)
filename <- paste0('M3_Anhed_scalar_result.','lavsyn.',modname,'.txt')
filename
write.table(lavsyn, filename)
# Estimate and save the scalar invariance model based on the Anhedonia model
library(lavaan)
lavmod.scalar <- cfa(model   = lavsyn,           
                     data    = dataset,          
                     ordered = itemlist,   
                     group  = 'group') 
lavmod.scalar
filename <- paste0('M3_Anhed_scalar','lavmod.',modname,'.rds')
filename
saveRDS(lavmod.scalar,filename)
# Reload the results
lavmod.scalar <- readRDS(filename)
# Extract model fit indices and create a summary data frame containing all fit indices
fitMeasures <- data.frame(value=fitMeasures(lavmod.scalar))
fitMeasures
# Calculate additional values and fit indices not provided by fitMeasures()
n    <- nrow(dataset)  
n
chisq      <- fitMeasures['chisq',]
df         <- fitMeasures['df',]       
chisq_pdf  <- chisq/df 
chisqs     <- fitMeasures['chisq.scaled',]
dfs        <- fitMeasures['df.scaled',]       
chisq_pdfs <- chisqs/dfs
# Add the additionally calculated values and fit indices to fitMeasures 
fitMeasures   <- rbind(fitMeasures, 
                       n = n,
                       chisq_pdf  = chisq_pdf,
                       chisq_pdfs = chisq_pdfs)
fitMeasures
# Extract the selected fit indices, format them into a table, and assign the result to "M3_scalar"
fittable <- data.frame(value=fitMeasures[fitCoeffs,]) 
rownames(fittable) <- fitCoeffs
colnames(fittable) <- modname 
fittable           <- t(fittable)
fittable

M3_scalar <- fittable

## Set model name for full measurement invariance: M4
modname   <- paste0('M4.full')
# Specify the full measurement invariance model based on the Anhedonia model
library(semTools)
lavsyn <- measEq.syntax(
  configural.model = anhed,                 
  data             = dataset,              
  ordered          = itemlist,             
  meanstructure    = TRUE,                 
  ID.cat           = 'Wu.2016',            
  parameterization = 'theta',              
  estimator        = 'WLSMV',              
  sample.nobs      = rep(1,ngroups),       
  group            = groupvar,             
  group.equal      = c('thresholds','loadings','intercepts','residuals'),  
  return.fit       = FALSE) 
# Convert the model syntax to a string and save it to a text file
lavsyn <- as.character(lavsyn)     
cat(lavsyn)
filename <- paste0('M4_Anhed_full_result.','lavsyn.',modname,'.txt')
filename
write.table(lavsyn, filename)
# Estimate and save the full measurement invariance model based on the Anhedonia model
library(lavaan)
lavmod.full <- cfa(model   = lavsyn,           
                   data    = dataset,          
                   ordered = itemlist,         
                   group  = 'group') 
lavmod.full
filename <- paste0('M4_Anhed_full.','lavmod.',modname,'.rds')
filename
saveRDS(lavmod.full,filename)
# Reload the results
lavmod.full <- readRDS(filename)
# Extract model fit indices and create a summary data frame containing all fit indices
fitMeasures <- data.frame(value=fitMeasures(lavmod.full))
fitMeasures
# Calculate additional values and fit indices not provided by fitMeasures()
n    <- nrow(dataset)  
n
chisq      <- fitMeasures['chisq',]
df         <- fitMeasures['df',]       
chisq_pdf  <- chisq/df 
chisqs     <- fitMeasures['chisq.scaled',]
dfs        <- fitMeasures['df.scaled',]       
chisq_pdfs <- chisqs/dfs
# Add the additionally calculated values and fit indices to fitMeasures 
fitMeasures   <- rbind(fitMeasures, 
                       n = n,
                       chisq_pdf  = chisq_pdf,
                       chisq_pdfs = chisq_pdfs)
fitMeasures
# Extract the selected fit indices, format them into a table, and assign the result to "M4_full"
fittable <- data.frame(value=fitMeasures[fitCoeffs,]) 
rownames(fittable) <- fitCoeffs
colnames(fittable) <- modname 
fittable           <- t(fittable)
fittable

M4_full <- fittable

### Model Comparison MGCFA
##  Exporting fit statistics from all MGCFA models

library(haven)
library(dplyr)
library(writexl)
library(openxlsx)
# Combine fit indices from all specified MGCFA models into a single data frame
fittable<-rbind(M0_configural, M1_thresholds, M2_metric, M3_scalar, M4_full)
fittable
outputdata <- fittable
sheetname  <- paste0('fitTable')
filename   <- paste0('MGCFA_Anhedonia_results.', sheetname,'.xlsx')
filename

write.xlsx(
  x = data.frame(outputdata), 
  file = filename, 
  append = FALSE,   
  quote = TRUE,     
  sep = "x",        
  eol = "\r",       
  na = "NA",        
  dec = ".",        
  rowNames = TRUE,  
  colNames = TRUE,  
  sheetName = sheetname,
  qmethod = ("escape")
)  

detach(package:haven)
detach(package:dplyr)
detach(package:writexl)
detach(package:openxlsx)

# Load the previously exported fit table
library(readxl)
CFA_compare_fitTable <- read_excel("MGCFA_Anhedonia_results.fitTable.xlsx")
View(MGCFA_Anhedonia_results.fitTable.xlsx)
# Perform Satorra-Bentler scaled chi-square difference test between M2_metric and M3_scalar
library(lavaan)
satorra_bentler_test <- lavTestLRT(lavmod.metric, lavmod.scalar)
satorra_bentler_test
