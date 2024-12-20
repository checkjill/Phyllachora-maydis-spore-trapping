library(rJava)
library(glmulti)
library(dplyr)
library(readxl)

LRdat <- read.csv(file = '/Users/jilliancheck/Library/CloudStorage/OneDrive-MichiganStateUniversity/Documents/Work/Spore Trapping/Code/HPCC/LRdat2.csv')

# wrapper function for linear mixed-models

glmer.glmulti <- function(formula,data, random="",...){
  glmer(paste(deparse(formula),random), 
        family = binomial, 
        data=data, 
        control=glmerControl(optimizer="bobyqa"), ...)
}

# define formula

form_glmulti = as.formula(paste('thrs_0 ~ ', paste(colnames(LRdat[5:length(LRdat)]), collapse = ' + ')))

# multi selection for glmer

mfit <- glmulti(form_glmulti,random='+ (1 | SiteYear)', maxsize = 2,
                data = LRdat, method = "h", fitfunc = glmer.glmulti,
                crit = 'aic', intercept = TRUE, marginality = FALSE, level = 2)

mfit

# looking at all models

loadout <- weightable(mfit)

write.csv(loadout, file = '/Users/jilliancheck/Library/CloudStorage/OneDrive-MichiganStateUniversity/Documents/Work/Spore Trapping/Manuscript/Figures/Model loadout.csv')

# examine best model

summary(mfit@objects[[1]])

# variable performance 

plot(mfit, type = 's')