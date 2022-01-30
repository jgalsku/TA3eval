
#########################################################
#########################################################

# added code that creates data.frames to save data of loop 

#########################################################
#########################################################

library(dplyr)
require("gtools")
require("MASS")
require("foreach")
require("iterators")
require("doParallel")
require("randomGLM")
#require("glmnet")
require("msir")


load("C:/Users/yo/Dropbox/TA3/TA3loop_SSOC/TA3_Input_SSOC.Rdata")


# create matrix to store results
TA3_age_est <- matrix(NA, nrow=NROW(TA3_Input_SSOC), ncol=10)
varnames <- c("ID_catkey", "doc_age", "PredAge", "LB", "UB", "AnalDat_nrow", "warnings", 
              "total_feat", "MeanStdError", "Corr(Age and Pred Age)")
colnames(TA3_age_est) <- varnames


# create another matrix to store AnalCaseB data for each individual

AnalCaseB_bind <- matrix(NA, nrow=1, ncol=119)

# dput(names(AnalCaseB_bind))

varnames2 <-c("ID_catkey", "acet_end_lipping", "acet_post_margin", "acet_post_surf",
              "ant_inf_spine_exos", "C1_eburnation", "C1_lipping", "Calc_LW",
              "cerv_candlewax", "cerv_lipping", "clav_med_epiph_0_123", "clav_med_epiph_01_23",
              "clav_med_epiph_012_3", "clav_med_macro_0_12", "clav_med_macro_01_2",
              "DISH_C", "DISH_L", "DISH_T", "fem_head_surf_0_12", "fem_head_surf_01_2",
              "fovea_margin", "glenoid_lipping_0_12", "glenoid_lipping_01_2",
              "Hum_LW", "humerus_greater_tubercle", "iliac_crest_fusion_0_123",
              "iliac_crest_fusion_01_23", "iliac_crest_fusion_012_3", "ilium_tuberculum_0_12",
              "ilium_tuberculum_01_2", "inf_post_iliac_exos", "Inno_LW", "ischium_tub_bumps_0_12",
              "ischium_tub_bumps_01_2", "ischium_tub_margin_spur", "L1_inf_epiph_0_123",
              "L1_inf_epiph_01_23", "L1_inf_epiph_012_3", "L1_inf_surf_0_123",
              "L1_inf_surf_01_23", "L1_inf_surf_012_3", "L1_spinous_process_0_12",
              "L1_spinous_process_01_2", "L1_sup_epiph_0_123", "L1_sup_epiph_01_23",
              "L1_sup_epiph_012_3", "L1_sup_surf_0_123", "L1_sup_surf_01_23",
              "L1_sup_surf_012_3", "L5_inf_surf_0_123", "L5_inf_surf_01_23",
              "L5_inf_surf_012_3", "L5_sup_surf_0_123", "L5_sup_surf_01_23",
              "L5_sup_surf_012_3", "lesser_tubercle_bumps", "lesser_tubercle_margin_0_12",
              "lesser_tubercle_margin_01_2", "lumbar_candlewax", "lumbar_lipping",
              "occipital_condyle_0_12", "occipital_condyle_01_2", "parietal_depression",
              "post_exost_complete_0_12", "post_exost_complete_01_2", "R1_Fusion",
              "R2_rim_profile", "R2_shingle", "R310_rim_profile", "R310_shingle",
              "radius_tuberosity_ridge", "S12_fusion_0_12", "S12_fusion_01_2",
              "sacral_elbow", "SI_fusion_0_12", "SI_fusion_01_2", "spheno_occipital",
              "sternum_dorsal_ridge", "sup_post_iliac_exos", "symph_relief_0_12",
              "symph_relief_01_2", "thoracic_candlewax", "thoracic_lipping",
              "Tib_LW", "troch_fossa_exost", "troch_med_surf_exost", "ventral_margin_0_123",
              "ventral_margin_01_23", "ventral_margin_012_3", "dorsal_margin_0_123",
              "dorsal_margin_01_23", "dorsal_margin_012_3", "fibula_mercury_wings",
              "inferior_surf_porosity", "Ischium_medial_spur", "L5_inf_epiph_0_123",
              "L5_inf_epiph_01_23", "L5_inf_epiph_012_3", "L5_inf_margin_0_12",
              "L5_inf_margin_01_2", "L5_sup_epiph_0_123", "L5_sup_epiph_01_23",
              "L5_sup_epiph_012_3", "L5_sup_margin_0_12", "L5_sup_margin_01_2",
              "lat_epicondyle", "med_epicondyle", "pubic_collar", "S1_margin_0_12",
              "S1_margin_01_2", "sup_apex_0_12", "sup_apex_01_2", "trapezium_lipping",
              "troch_lat_surf_0_123", "troch_lat_surf_01_23", "troch_lat_surf_012_3",
              "ulna_exostoses", "clav_lat_macro", "clav_med_gravel")


colnames(AnalCaseB_bind) <- varnames2
AnalCaseB_bind<-as.data.frame(AnalCaseB_bind)













########################################################################################################
########################################################################################################

                                 ## ORGINAL CODE slightly MODIFIED ##

########################################################################################################
########################################################################################################


#rm(list = ls())


# ############# Helper Functions #############
# trim <- function (x) gsub("^\\s+|\\s+$", "", x)
# 
# 
# ############# Gathering of Arguments Passed In #############
# args = commandArgs(trailingOnly=TRUE)
# temp_dir<-trim(args[1])  # directory where input and output files will be
# scripts_dir<-trim(args[2])  # directory where R related files will be
# pkg_dir<-trim(args[3])  # directory where R install packages will be
# TA3ProgramVersion<-trim(args[4])  # version number of the application
# TA3RCodeVersion<-trim(args[5])  # version number of the R code
# TA3BUMVersion<-trim(args[6])  # version number of the TA3BUM file
# TA3OUMVersion<-trim(args[7])  # version number of the TA3OUM file


# Executable: C:\Users\Steve10\AppData\Local\Programs\ta3\resources\R-Portable\bin\RScript.exe
# Parameters:
# C:\\Users\\Steve10\\AppData\\Local\\Programs\\ta3\\resources\\scripts\\ta3.R
# 1 C:\\Users\\Steve10\\TA3\\analysis
# 2 C:\\Users\\Steve10\\AppData\\Local\\Programs\\ta3\\resources\\scripts
# 3 C:\\Users\\Steve10\\TA3\\packages
# 4 0.7.0

# SH <- FALSE
# if (SH) {
#         
#         temp_dir<-'C:\\Users\\Steve10\\TA3\\analysis' ; # trim(args[1])  # directory where input and output files will be
#         scripts_dir<-'C:\\Users\\Steve10\\AppData\\Local\\Programs\\ta3\\resources\\scripts'; # trim(args[2])  # directory where R related files will be
#         pkg_dir<-'C:\\Users\\Steve10\\TA3\\packages'; # trim(args[3])  # directory where R install packages will be
#         TA3ProgramVersion<-'0.7.1'  # version number of the application
#         
#         
# }



############# Update Environment #############
# #.libPaths(c(pkg_dir, .libPaths()))
# .libPaths(c(pkg_dir))
# setwd(scripts_dir)


############# Development Options #############
# development <- FALSE
# if (development) { scripts_dir <- file.path("c:", "rthings") }
# if (development) { temp_dir <- file.path("c:", "rthings") }
# 



############# Creating Local Variables for Saving/Usage #############

# rda_fileB <- file.path(scripts_dir, "TA3BUM.Rda")
# rda_fileO <- file.path(scripts_dir, "TA3OUM.Rda")
# case_tall_file <- file.path(scripts_dir, "TA3_Case_Scores.Rda")
# 
# input_file<-file.path(temp_dir, "TA3_Input.csv")
# 
# output_file<-file.path(temp_dir, "output.txt")
# output_image1<-file.path(temp_dir, "output1.png")
# #output_image2<-file.path(temp_dir, "output2.png")
# 



scripts_dir<-'C:/Users/yo/TA3/assets/analysis'; # trim(args[2])  # directory where R related files will be

rda_fileB <- file.path(scripts_dir, "TA3BUM.Rda")
rda_fileO <- file.path(scripts_dir, "TA3OUM.Rda")
# case_tall_file <- file.path(scripts_dir, "TA3_Case_Scores.Rda")


# read in reference data files if not already present
TA3BUM<-readRDS(rda_fileB) # binary traits
TA3OUM<-readRDS(rda_fileO) # ordinal traits


apply(TA3BUM, 2, table, exclude=NULL)
apply(TA3OUM, 2, table, exclude=NULL)



############# List of Required Packages #############
# require("gtools")
# require("MASS")
# require("foreach")
# require("iterators")
# require("doParallel")
# require("randomGLM")
# #require("glmnet")
# require("msir")



############# Debugging Information #############
# sessionInfo()
# .libPaths()
# getwd()
# 



############# TA3 Analysis (Stephen D. Ousley) #############


#  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ #
##################  copy below to %APPDATA% ###############

###############################################################################
######################### Start of R Processing ###############################
#
#         0.46.1 using OUM field list and length only
#         0.46 working with new data format, enhanced output
#         0.45 added binary vs. raw data
#         0.44 - fixed plots and sped up calculations
#         0.43 - fixed development colons; smarter output;
#         0.42 - fixed x y
#
###############################################################################
###############################################################################




#########################################################
#  modified into Loop (#JG = modified/added by J.Galimany)
#########################################################

for (k in 1:342) # 1:n total sample
{ 

TA3_Input <- as.data.frame(TA3_Input_SSOC[k,])
TA3_Input$cerv_candlewax <- NA #to compensate for TA3 software's error 

#JG store data on ID_catkey and documented age for that case in the "TA3_age_est" matrix
TA3_age_est[k,1] <- TA3_Input$ID_catkey
TA3_age_est[k,2] <- TA3_Input$doc_age


#########################################v

#JG eliminate doc_age column
TA3_Input <-  TA3_Input[,-c(1,2)]

TA3_Input <- TA3_Input %>%
        mutate(across(everything(), as.numeric))

################################################


# NEW / MODIFIED BELOW
# R code version
#      TA3RCodeVersion <- '0.46.1';

# software program version.  currently or soon 0.70
#if (development) { TA3ProgramVersion <- '0.71'};


# use binary or raw scores?
UseBinaryScores <- TRUE;

# set some vars to nothing so errors do not get repeated
PredAge <- 0;
AnalDat <- 0;
TA3_Case_Scores <- 0;


# # read in reference data files if not already present
# if (!(exists('TA3BUM'))) {TA3BUM<-readRDS(rda_fileB)}
# if (!(exists('TA3OUM'))) {TA3OUM<-readRDS(rda_fileO)}


TA3_Case_Scores <-  readRDS("C:/Users/yo/TA3/assets/analysis/TA3_Case_Scores.Rda")



# convert factors to character format in case scores
TA3_Case_Scores$TraitDBName <- as.character(TA3_Case_Scores$TraitDBName)
TA3_Case_Scores$TraitText <- as.character(TA3_Case_Scores$TraitText)

# process trait scores file, has ALL scores including NAs
# TA3_Input <- read.csv(input_file)

# update values in case file (tall) from scores file (wide)
for (i in (1:nrow(TA3_Case_Scores) ) ) {
        
        for (j in (1:ncol(TA3_Input) ) ) {
                
                if (TA3_Case_Scores$TraitDBName[i] == names(TA3_Input)[j]) {
                        
                        TA3_Case_Scores$TraitScore[i]  <- TA3_Input[1,j]
                }
                
        }
        
}

# Remove NAs in case tabular data (but there may be no NAs, so cant just use !which)
TA3_Case_Scores <- TA3_Case_Scores[which(complete.cases(TA3_Case_Scores$TraitScore)),]

# copy case scores to analysis data
AnalCase <- data.frame('parietal_depression' = 0); # create data frame();

for (i in (1:nrow(TA3_Case_Scores) ) ) {
        
        AnalCase[i] <- TA3_Case_Scores[i,'TraitScore']
        #names(AnalCase)[i] <- as.character(TA3_Case_Scores[i,'TraitDBName']);
        names(AnalCase)[i] <- TA3_Case_Scores[i,'TraitDBName'];
        #  NOTE: as.character() added because it failed after running successfully for many time. Why?
        #  BECAUSE they are factors now, need to convert.
        
}

### convert left and right Case values into unilateral
# get L and R fields
BiTraitList <- NULL

# AnalCase already no NAs (blanks)
for (i in (1:ncol(AnalCase))) {
        
        lside <- paste(substr(names(AnalCase[i]), 1, nchar(names(AnalCase[i]))-1 ), 'L', sep = '');
        rside <- paste(substr(names(AnalCase[i]), 1, nchar(names(AnalCase[i]))-1 ), 'R', sep = '');
        
        # OR     take care of DISH for now
        if  ( (lside %in% names(AnalCase)    ||  rside %in% names(AnalCase))
              # take care of DISH and max lengths (XL) for now
              & !(regexpr('DISH', lside) > 0) & !(regexpr('XL', lside) > 0)  ) {
                
                BiTraitList <- append(BiTraitList, names(AnalCase[i]) );   #paste(substr(lside,1, nchar(lside)-1), sep = '') )
                
        }
}

# check if there are indeed bilateral traits scored
if (length(BiTraitList) > 0) {
        # sweep through all Case fields, assign Left to neutral trait if not null;
        for (i in (1:length(BiTraitList))) {
                
                currfld <- BiTraitList[i];
                biside <- substr(currfld, 1, nchar(currfld)-1);
                
                AnalCase[biside] <- AnalCase[currfld];
                
                # set up lookuplist for later better formatting
                # TraitNames <-   append TraitNames
                
        }  #for
        
        # remove left or right fields
        AnalCase <- AnalCase[,-which(names(AnalCase) %in% BiTraitList)]
        
}  # if bitraits there, not present id midline



##########################################################
### Convert raw data into binary-only values
AnalCaseB <- AnalCase;

# Go through all column names in AnalCase (non-blank ones would be better)
# see which ones have ordinal values in reference data

# set up list for converted columns
OrdinalCols <- ''
i <- 1;

for (col in 1:ncol(AnalCaseB))
{
        colname <- names(AnalCaseB)[col]
        collen <- length(table(TA3OUM[colname]))
        
        if (collen > 2)
        {
                # print(colname);
                OrdinalCols[i] <- colname;
                i <- i + 1;
                if (collen == 3)
                {
                        AnalCaseB[paste(colname, '_0_12', sep = '')] <- NA
                        
                        AnalCaseB[paste(colname, '_01_2', sep = '')] <- NA
                        
                        
                        AnalCaseB[which(AnalCaseB[colname] == 0), paste(colname, '_0_12', sep = '')] <-  0
                        AnalCaseB[which(AnalCaseB[colname] == 0), paste(colname, '_01_2', sep = '')] <-  0
                        
                        AnalCaseB[which(AnalCaseB[colname] == 1), paste(colname, '_0_12', sep = '')] <-  0
                        AnalCaseB[which(AnalCaseB[colname] == 1), paste(colname, '_01_2', sep = '')] <-  1
                        
                        AnalCaseB[which(AnalCaseB[colname] == 2), paste(colname, '_0_12', sep = '')] <-  1
                        AnalCaseB[which(AnalCaseB[colname] == 2), paste(colname, '_01_2', sep = '')] <-  1
                        
                } else
                {
                        if (collen == 4)
                        {
                                AnalCaseB[paste(colname, '_0_123', sep = '')] <- NA
                                
                                AnalCaseB[paste(colname, '_01_23', sep = '')] <- NA
                                
                                AnalCaseB[paste(colname, '_012_3', sep = '')] <- NA
                                
                                
                                AnalCaseB[which(AnalCaseB[colname] == 0), paste(colname, '_0_123', sep = '')] <- 0
                                AnalCaseB[which(AnalCaseB[colname] == 0), paste(colname, '_01_23', sep = '')] <- 0
                                AnalCaseB[which(AnalCaseB[colname] == 0), paste(colname, '_012_3', sep = '')] <- 0
                                
                                AnalCaseB[which(AnalCaseB[colname] == 1), paste(colname, '_0_123', sep = '')] <-  1
                                AnalCaseB[which(AnalCaseB[colname] == 1), paste(colname, '_01_23', sep = '')] <-  0
                                AnalCaseB[which(AnalCaseB[colname] == 1), paste(colname, '_012_3', sep = '')] <-  0
                                
                                AnalCaseB[which(AnalCaseB[colname] == 2), paste(colname, '_0_123', sep = '')] <-   1
                                AnalCaseB[which(AnalCaseB[colname] == 2), paste(colname, '_01_23', sep = '')] <-   1
                                AnalCaseB[which(AnalCaseB[colname] == 2), paste(colname, '_012_3', sep = '')] <-   0
                                
                                AnalCaseB[which(AnalCaseB[colname] == 3), paste(colname, '_0_123', sep = '')] <-  1
                                AnalCaseB[which(AnalCaseB[colname] == 3), paste(colname, '_01_23', sep = '')] <-  1
                                AnalCaseB[which(AnalCaseB[colname] == 3), paste(colname, '_012_3', sep = '')] <-  1
                                
                                #table(AnalCaseB[c(colname, paste(colname,'_0_123', sep= ''), paste(colname,'_01_23', sep= ''), paste(colname,'_012_3', sep= ''))])
                                #table(AnalCaseB[colname])
                                
                                
                        } # if
                        
                } #else
                
        } #if collen > 2
        
} # for each col


# remove non-binary fields from newly binarized case table, remove fields by name
if (OrdinalCols[1] > '') {
        AnalCaseB <- AnalCaseB[,-which(names(AnalCaseB) %in% OrdinalCols)]
}


# AnalCase and AnalCaseB are set up;
# SEE NOTE 4 in TA3_Application Notes-RCran-Electron-Windows.txt


####### Extract Data; choice is for binary or ordinal, still uncertain in many cases  ###########################
#### Extract binary data for now
# get field names not blank (no NAs)

if (UseBinaryScores) # binary
{
        NBF <- names(AnalCaseB)
        #read from binary file
        AnalDat <- na.omit(TA3BUM[c('RandID','age',NBF)])
        
        
} else   # ordinal
{
        NBF <- names(AnalCase)
        #read from ordinal file
        AnalDat <- na.omit(TA3OUM[c('RandID','age',NBF)])
        
        
}



#####################################################
#JG
# save AnalDat data for each case (reference sample data for each case)
setwd("C:/Users/yo/Dropbox/TA3/TA3loop_SSOC/AnalDat_loop_SSOC")
file_name <- paste0("AnalDat_",TA3_age_est[k,1])
write.csv(AnalDat, file = paste0(file_name,".csv"))

####################################################v


# pre-process data
# separate RandIDs
RandIDs <- AnalDat$RandID
# remove column from analyzed data
AnalDat <- AnalDat[,-which(colnames(AnalDat) == 'RandID')]

## GLM needs numerical values anyway- so convert all

# separate ages?
ages <- AnalDat["age"]
# remove ages column from analyzed data?
#AnalDat <- AnalDat[,-which(colnames(AnalDat) == 'age')]


# y must be numeric (not integer)
agesn <- lapply(ages, as.numeric)

# strip name from vector
agesnu <- unlist(agesn, use.names = F)


# the predictors need to be numeric
#AnalDat <- lapply(AnalDat, as.numeric)

# need to convert back into a dataframe:
AnalDat <- as.data.frame(AnalDat);

#str(AnalDat)
#'data.frame':	199 obs. of  89 variables:
cat(nrow(AnalDat), 'records in reference data.');

ages <- AnalDat["age"]
AnalDat <- AnalDat[,-which(colnames(AnalDat) == 'age')]

agesn <- lapply(ages, as.numeric)

agesnu <- unlist(agesn, use.names = F)

AnalDat <- lapply(AnalDat, as.numeric)

AnalDat <- as.data.frame(AnalDat)

####################### Preprocessing Done ###################

####### Start of R Statistical Analysis (example: randomGLM) ############

#require(randomGLM)
#require(msir)
#require(glmnet)
# parallel
#nThr <- detectCores()


# TODO: can we show progress bar and estimate time remaining?
# nFeaturesinBag
# nBags usually 100, but 20 or 30 should be just fine

out <- capture.output(RGLM <- randomGLM(AnalDat, agesnu, classify = F, nBags = 100, nThreads=1)) #  , keepModels = T) (no need to keep models)
#RGLM <- randomGLM(AnalDat, agesnu, classify = F, nBags = 100, keepModels = T, maxInteractionOrder = 2, nFeaturesInBag = 100)

#, nThreads=nThr,
#replace = TRUE,
#sampleWeight=NULL,
# nObsInBag = if (replace) nrow(AnalDat) else as.integer(0.632 * nrow(AnalDat)),
#nFeaturesInBag = ceiling(ifelse(ncol(AnalDat)<=10, ncol(AnalDat),
#ifelse(ncol(AnalDat)<=300, (1.0276-0.00276*ncol(AnalDat))*ncol(AnalDat), ncol(AnalDat)/5))),
#minInBagObs = min( max( nrow(AnalDat)/2, 5), 2*nrow(AnalDat)/3))


#GLMN <- glmnet(AnalDat, agesnu, classify = F, nBags = 100, minInBagObs = 20)

# Calculates and plots a 1.96 * SD prediction band, that is,
# a 95% prediction band
DFP <- cbind(RGLM$predictedOOB, agesnu)
DFP <- as.data.frame(DFP)
names(DFP) <- c('PredAge','Age')

# larger span now makes MUCH better (default = 0.67, Weisberg (2005):112 )
lol <- loess.sd(DFP, nsigma = 1.96, span = 1)

# predicted age
# if (UseBinaryScores)
# { 
        PredAge <- predict(RGLM,AnalCaseB, type='response')

# }  else
# { PredAge <- predict(RGLM,AnalCase, type='response')
# }
PredAge


# produce basic output: MSE
MeanStdError <- sqrt(mean((DFP$PredAge - DFP$Age)^2))


# get prediction intervals for this individual
ADindex <- which.min(abs(lol$x  - PredAge))
LB <- lol$lower[ADindex]
UB <- lol$upper[ADindex]


# # Save plot to file (enhanced plot)  IF APP RUNNING
# if (!development) { png(filename=output_image1, width = 1400, height = 1200, res = 300, pointsize = 7)   }
# 
# # plot OOB estimated age and 95% CI for OOB individuals
# plot(DFP$PredAge, DFP$Age, ylim = c(15,110), xlim = c(15,110), , pch = 17, cex = 0.7, col = 'blue',
#      xlab = 'Predicted Age', ylab = 'Age', main = 'TA3 Age Estimation (Random GLM) 95% OOB PI')
# 
# lines(lol$x, lol$y, lw = 3, lty=3, col= 'purple')
# lines(lol$x, lol$upper, lty=2, lw = 1, col= 'purple')
# lines(lol$x, lol$lower, lty=2, lw = 1, col= 'purple')
# # line of perfect agreement
# abline(0,1, lw = 1)
# 
# if (!development) {  dev.off()  }
# 

#print(paste('The estimated age at death is', round(PredAge), 'years and the Standard Error is', round(MeanStdError,1),'\n', 'using a sample size of',nrow(AnalDat)))


############# End of TA3 Analysis #############




############# Save output to Application #############


# # if (UseBinaryScores) {scorestr <- 'Using binarized ordinal scores'} else {'Using binary and nominal scores'};
# scorestr <- 'Trait: score'
# 
# # write results to a file for reading
# progVersion<-paste('  Program Version ', TA3ProgramVersion, sep='')
# codeVersion<-paste('  R Code Version ', TA3RCodeVersion, sep='')
# if (UseBinaryScores) {scorebasis<-'Using binarized ordinal scores.'} else {scorebasis<-'Using raw scores.'};
# bumVersion<-paste('  TA3BUM Version ', TA3BUMVersion, sep='')
# oumVersion<-paste('  TA3OUM Version ', TA3OUMVersion, sep='')
# write(
#         paste(
#                 '---------------------------------------------',
#                 'TA3 Age Estimation',
#                 progVersion,
#                 codeVersion,
#                 scorebasis,
#                 bumVersion,
#                 oumVersion,
#                 '---------------------------------------------',
#                 ' ',
#                 sep='\n'
#         ),
#         file=output_file,
#         append=FALSE,
#         sep=''
# )
# 
# 
# write(paste(
#         scorestr,
#         '---------------------------------',
#         sep='\n'),
#       file=output_file,
#       append=TRUE,
#       sep=''
# )
# 
# 
# for (i in c(1:nrow(TA3_Case_Scores))) {
#         write(paste(TA3_Case_Scores$TraitText[i], ': ',TA3_Case_Scores$TraitScore[i],  sep=''), file=output_file, append=TRUE, sep='\n')
# }
# 
# write(
#         paste(
#                 ' ',
# paste('Sample size = ', round(nrow(AnalDat)), sep=''),
# ' ',
# 'Random GLM Analysis',
# paste('  Estimated age at death = ', round(PredAge,1), ' years', sep=''),
# paste('  Estimated lower 95% bound = ', round(LB,1), ' years', sep=''),
# paste('  Estimated upper 95% bound = ', round(UB,1), ' years', sep=''),
# paste('  '),
# paste('  Standard Error = ', round(MeanStdError,1), sep=''),
# paste('  Corr(Age and Pred Age) = ', round(cor(DFP$Age,DFP$PredAge),3), sep=''),
# sep='\n'
#         ),
#         file=output_file,
#         append=TRUE,
#         sep=''
# )
# 
# if (development) {  TA3_Case_Scores  }
# 
# if (development) {  read.delim(file.path(temp_dir, 'output.txt'))  }



##############################################

#JG to store data
TA3_age_est[k,3] <- round(PredAge,1)
TA3_age_est[k,4] <- round(unname(LB),1)
TA3_age_est[k,5] <- round(unname(UB),1)
TA3_age_est[k,6] <- paste(nrow(AnalDat), 'records in reference data.')
TA3_age_est[k,7] <- paste(out[1], out[2], sep = "") # record warning message 
TA3_age_est[k,8] <- ncol(AnalCaseB) # record number of features used
TA3_age_est[k,9] <- round(MeanStdError,2)
TA3_age_est[k,10] <- round(cor(DFP$Age,DFP$PredAge),3)


AnalCaseB$ID_catkey <- TA3_age_est[k,1] # add ID_catkey to AnalCaseB dataframe
AnalCaseB_bind <- bind_rows(AnalCaseB_bind, AnalCaseB) # bind current AnalCaseB to AnalCaseB_bind

}


#  convert matrix to dataframe and save it as csv
TA3_age_est <- data.frame(TA3_age_est)

setwd("C:/Users/yo/Dropbox/TA3/TA3loop_SSOC")
write.csv(TA3_age_est, file = "TA3loop_SSOC_results.csv")



#prepare AnalCaseB_bind dataframe 

# eliminate first NA row
AnalCaseB_bind <- AnalCaseB_bind[-1,]

# add total_feat variable woith count of non-NA columns
AnalCaseB_bind$total_feat <- rowSums(!is.na(AnalCaseB_bind[-which(names(AnalCaseB_bind)=="ID_catkey")]))

# reorder columns
# AnalCaseB_bind <- AnalCaseB_bind[,order(colnames(AnalCaseB_bind))]
AnalCaseB_bind <- AnalCaseB_bind %>% relocate(ID_catkey, total_feat)
write.csv(AnalCaseB_bind, file = "TA3loop_SSOC_AnalCaseB_bind.csv")

#############################################v
