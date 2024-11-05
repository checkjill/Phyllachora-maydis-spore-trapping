library(rJava)
library(glmulti)
library(dplyr)
library(readxl)
library(grDevices)
library(lme4)

LRdat <- read.csv(file = '/mnt/home/checkjil/SporeTrap/LRdat2.csv') %>% 
select(!starts_with(c('MA2_', 'MA3_', 'MA4_'))) %>%
select(1:15) # removing MA other than 1 and some other predictors due to "Too many predictors" error

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

write.csv(loadout, file = '/mnt/home/checkjil/SporeTrap/ModelLoadout.csv')

# variable performance 

par(mar=c(1,1,1,1))
pdf(file = "/mnt/home/checkjil/SporeTrap/varimpt.pdf",
    # The directory you want to save the file in
    width = 4, # The width of the plot in inches
    height = 4) # The height of the plot in inches
plot(mfit, type = 's')
dev.off()
