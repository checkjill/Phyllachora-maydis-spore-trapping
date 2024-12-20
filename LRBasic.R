library(rJava)
library(glmulti)
library(dplyr)
library(readxl)
library(grDevices)
library(lme4)

LRdat <- read.csv(file = '/mnt/home/checkjil/SporeTrap/Data/LRdat.csv')
LRdat <- LRdat %>% select('thrs_0', 'SiteYear', starts_with('MA1_')) %>% select(1:12) 
# removing MA other than 1 and some other predictors due to "Too many predictors" error

# wrapper function for linear mixed-models

glmer.glmulti <- function(formula,data, random="",...){
  glmer(paste(deparse(formula),random),
        family = binomial,
        data=data,
        control=glmerControl(optimizer="bobyqa"), ...)
}

# define formula

form_glmulti = as.formula(paste('thrs_0 ~ ', paste(colnames(LRdat[3:length(LRdat)]), collapse = ' + ')))
# multi selection for glmer

mfit <- glmulti(form_glmulti,random='+ (1 | SiteYear)', maxsize = 3,
                data = LRdat, method = "h", fitfunc = glmer.glmulti,
                crit = 'aic', intercept = TRUE, marginality = FALSE, level = 1)

mfit

# looking at all models

loadout <- weightable(mfit)

write.csv(loadout, file = '/mnt/home/checkjil/SporeTrap/Results/ModelLoadoutBasic.csv')

# variable performance 

par(mar=c(1,1,1,1))
pdf(file = "/mnt/home/checkjil/SporeTrap/Results/varimptBasic.pdf",
    # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 4) # The height of the plot in inches
plot(mfit, type = 's')
dev.off()
