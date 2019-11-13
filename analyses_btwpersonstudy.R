library("effsize")
library("metafor")
source("effect_size_d_paired_function.R")

# Anonimize data and store the analysis data file----
# # The code below is run on the raw non-anonymous data to generate the anonymous data file
# PANASdata <- read.csv(file('SESOI - btw person PA.csv'), stringsAsFactors = FALSE)
# 
# #Save one case by fixing a clear participant typo (double 4 instead of single 4)
# PANASdata[69,15] <- "ID removed for anonimity"
# 
# #mean PA for each participant
# PANASdata$mPA <- rowMeans(PANASdata[ , grep('PA_', colnames(PANASdata))], na.rm=TRUE)
# 
# #create new variable for each participant that extracts mean PA from their partner
# # based on StudentID (from other observations in the dataset) that matches the partnerID
# # of the target participant
# PANASdata$mPApartner <- PANASdata[match(PANASdata$PartnerID, PANASdata$StudentID, nomatch = NA_integer_, incomparables = NULL),
#                                   'mPA']
# 
# # Get all PANAS Data from partner (not just the mean)
# PANAS_partner <- PANASdata[match(PANASdata$PartnerID,
#                                  PANASdata$StudentID,
#                                  nomatch = NA_integer_,
#                                  incomparables = NULL),
#                            c(5:14)]
# # Change column names
# colnames(PANAS_partner)[1:10] <- paste(colnames(PANAS_partner)[1:10], "partner", sep = "_")
# # Combine the 2 dataframes
# PANASdata <- cbind(PANASdata,PANAS_partner)
# 
# # Below is a check of we get the same means - and we do
# PANASdata$mPA2 <- rowMeans(PANASdata[ ,19:28], na.rm=TRUE)
# 
# # We can no delete columns to make the data anonymous
# PANASdata <- subset(PANASdata, select = -c(StudentID, PartnerID, mPA2))
# 
# # Add random StudentID as participant identifier
# PANASdata$StudentID <- c(1:nrow(PANASdata))
# 
# #Then write the csv file
# write.csv2(PANASdata, file = "PANAS_study2_data_anon.csv", row.names = FALSE)

# Read in data----
PANASdata <- read.csv2(file('PANAS_study2_data_anon.csv'), stringsAsFactors = FALSE)

#remove any observations with NA values
PANASdata <- PANASdata[complete.cases(PANASdata), ]

#calculate the difference in mean PA between the pairs, 
PANASdata$PA_diff <- PANASdata$mPA - PANASdata$mPApartner

#Calculate all statistics for each global anchor question group.----

sum_data<-data.frame(t = numeric(0), 
                     df = numeric(0), 
                     p = numeric(0), 
                     ci_lower_mean = numeric(0), 
                     ci_upper_mean = numeric(0), 
                     mean_1 = numeric(0), 
                     mean_2 = numeric(0),
                     m_diff = numeric(0),
                     ci_l_m_diff = numeric(0),
                     ci_u_m_diff = numeric(0),
                     d_av = numeric(0),
                     d_av_unb = numeric(0),
                     s_av = numeric(0),
                     s_diff = numeric(0),
                     ci_l_d_av = numeric(0),
                     ci_u_d_av = numeric(0),
                     d_z = numeric(0),
                     d_z_unb = numeric(0),
                     ci_l_d_z = numeric(0),
                     ci_u_d_z = numeric(0),
                     N = numeric(0),
                     m1 = numeric(0),
                     m2 = numeric(0),
                     sd1 = numeric(0),
                     sd2 = numeric(0),
                     cor = numeric(0),
                     vi = numeric(0))
for(i in 1:5){
  temp <- t.test(PANASdata$PA_diff[PANASdata$CompareQ == i])
  sum_data[i,1] <- temp$statistic
  sum_data[i,2] <- temp$parameter
  sum_data[i,3] <- temp$p.value
  sum_data[i,4] <- temp$conf.int[1]
  sum_data[i,5] <- temp$conf.int[2]
  sum_data[i,6] <- temp$estimate[1]
  sum_data[i,7] <- temp$estimate[2]
  temp <- effect_size_d_paired(PANASdata$mPA[PANASdata$CompareQ == i], PANASdata$mPApartner[PANASdata$CompareQ == i])
  sum_data[i,8] <- temp$m_diff
  sum_data[i,9] <- temp$ci_l_m_diff
  sum_data[i,10] <- temp$ci_u_m_diff
  sum_data[i,11] <- temp$d_av
  sum_data[i,12] <- temp$d_av_unb
  sum_data[i,13] <- temp$s_av
  sum_data[i,14] <- temp$s_diff
  sum_data[i,15] <- temp$ci_l_d_av
  sum_data[i,16] <- temp$ci_u_d_av
  sum_data[i,17] <- temp$d_z
  sum_data[i,18] <- temp$d_z_unb
  sum_data[i,19] <- temp$ci_l_d_z
  sum_data[i,20] <- temp$ci_u_d_z
  sum_data[i,21] <- temp$N
  sum_data[i,22] <- temp$m1
  sum_data[i,23] <- temp$m2
  sum_data[i,24] <- temp$sd1
  sum_data[i,25] <- temp$sd2
  sum_data[i,26] <- temp$cor
  sum_data[i,27] <- 1/sum_data$N[i] + sum_data$d_z[i]^2 / (2*sum_data$N[i]) #this is the formula for the variance based on the metafor package. It leads to CI slightly different from dz_l and dz_u because these are based on non-central t (using MBESS package).
}

# We can perform a meta-analysis based on this data.
# We can use the escalc function - which reports a bias corrected version of dz (so the estimates are slightly lower)
g <- escalc(ni = sum_data$N, m1i = sum_data$m1, m2i = sum_data$m2, sd1i = sum_data$sd1, sd2i = sum_data$sd2, ri = sum_data$cor, measure = "SMCC")
# We reverse the direction of the effect for individuals in group 2 by using abs(yi)
result <- rma(abs(yi), vi, data=g[c(2,4),])
result
forest(result)

#Or do it based on the data computed by the paired effects function (same results)
#select only the rows and columns for little change, dz and variance
g <- sum_data[c(2,4),c(18,27)]
#perform the meta-analysis
result <- rma(yi = abs(d_z_unb), vi = vi, data=g)
result
forest(result)

#Note that the CI for d_z differ slightly from the meta-analytic effect sizes (due to the use of non-central t in MBESS and bias correction)
sum_data[c(2,4),c(18:20)]

#OLD ANALYSES BEFORE META_ANALYSIS----
#reverse scoring those who said they felt a little less so we can get
# the mean for the subset of participants who felt either a little less or a little
# more PA
PANASdata$PA_diff_rec <- ifelse(PANASdata$CompareQ == 2, PANASdata$PA_diff*-1, 
                            PANASdata$PA_diff*1)

#mean of difference in PA between self and partner for all those who said they
# felt either a little less or a little more positive compared to partner
mean(PANASdata$PA_diff_rec[PANASdata$CompareQ == 2 | PANASdata$CompareQ == 4], na.rm=TRUE)
sd(PANASdata$PA_diff_rec[PANASdata$CompareQ == 2 | PANASdata$CompareQ == 4], na.rm=TRUE)

#Farid, should be paired = TRUE?
effsize::cohen.d(PANASdata$mPApartner[PANASdata$CompareQ == 2],
        PANASdata$mPA[PANASdata$CompareQ == 2],
        pooled=TRUE,paired=FALSE, 
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95)

#Farid, should be paired = TRUE?
effsize::cohen.d(PANASdata$mPApartner[PANASdata$CompareQ == 4],
        PANASdata$mPA[PANASdata$CompareQ == 4],
        pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95)


#together, but this is cohen's dz
mPA = mean(PANASdata$PA_diff[PANASdata$CompareQ == 2 | PANASdata$CompareQ == 4], na.rm=TRUE)
sPA = sd(PANASdata$PA_diff[PANASdata$CompareQ == 2 | PANASdata$CompareQ == 4], na.rm=TRUE)
mPA
sPA
cohenszPA = mPA/sPA
cohenszPA

# number of participants in the "little changed" groups
sum(PANASdata$CompareQ == 2 | PANASdata$CompareQ == 4) 
# n = 53 (when there are only complete cases)

sum(PANASdata$CompareQ == 1)
sum(PANASdata$CompareQ == 3)
sum(PANASdata$CompareQ == 5)




#########
# exploratory analyses
#########

#comparing those who felt about the same with those who felt a little different
PAsame <- PANASdata$PA_diff[PANASdata$CompareQ == 3]
PAlittle <- PANASdata$PA_diff_rec[PANASdata$CompareQ == 2 | PANASdata$CompareQ == 4]
t.test(PAsame, PAlittle)

cohen.d(PAsame,
        PAlittle,
        pooled=TRUE,paired=FALSE,
        na.rm=TRUE, hedges.correction=FALSE,
        conf.level=0.95)


#####
#testing difference between those who were a little less positive and those who 
# were a little more positive
PAless <- PANASdata$PA_diff_rec[PANASdata$CompareQ == 2]
PAmore <- PANASdata$PA_diff_rec[PANASdata$CompareQ == 4]
t.test(PAless, PAmore)
#difference is nonsig

#testing them against those who were the same
t.test(PAless, PAsame)
#p = .0503
t.test(PAmore, PAsame)
# p = .3737


#looking at individual variability
sum(PANASdata$PA_diff_rec[PANASdata$CompareQ == 2 | PANASdata$CompareQ == 4] >= 0.26)
# 26/53 in the little-different group showed differences at MID level or higher

sum(PANASdata$PA_diff_rec[PANASdata$CompareQ == 2 | PANASdata$CompareQ == 4] >= 0)
# 36/53 showed differences greater than or equal to zero scale points 
# in the direction they said they changed on the global Qs

sum(PANASdata$PA_diff_rec[PANASdata$CompareQ == 2 | PANASdata$CompareQ == 4] < 0)
# 17/53 showed differences in the opposite direction to what they said 
# (e.g., said they felt a little more positive emotions but PA scores were lower)

sum(PANASdata$PA_diff_rec[PANASdata$CompareQ == 2] < 0) 
#5/21 of those who felt less positive showed change in opposite direction

sum(PANASdata$PA_diff_rec[PANASdata$CompareQ == 4] < 0)
#12/32 of those who felt less positive showed change in opposite direction



###########
# plotting
###########


library('ggplot2')
#need to make it long format
library('reshape2') 

PANASdata_long = PANASdata
PANASdata_long$PAself = PANASdata_long$mPA #new variable with more descriptive name
PANASdata_long$PApartner = PANASdata_long$mPApartner #new variable with more descriptive name

#WHEN WE MAKE IT LONG FORMAT and we only want those in the little changed PA group first
PANASdata_long = melt(PANASdata_long[PANASdata_long$CompareQ==2 | PANASdata_long$CompareQ == 4,], 
              measure.vars = c("PAself", "PApartner") )
head(PANASdata_long)
#looking at the dataframe created, can see that each participant now has a
# T1_PA, and T2_PA entry (i.e., in long format)

PANASdata_long$CompareQ = as.factor(PANASdata_long$CompareQ)
levels(PANASdata_long$CompareQ) <- c("a little less positive", "a little more positive") #renaming the levels

ggplot(data = PANASdata_long, aes(x = variable, y = value)) + 
    geom_point(size = 2, aes(color = CompareQ)) + #colour points by globalPA
    geom_path(aes(group = StudentID, color = CompareQ)) + #spaghetti plot
    ylab("Positive Affect (mean)") +
    xlab("Self or Partner") +
    theme_bw() +
    facet_grid(~ CompareQ) + theme(panel.grid.major = element_line(colour = "gray86", 
        size = 0.6), panel.grid.minor = element_line(size = 0.2), 
    strip.background = element_rect(colour = "gray96"), 
    legend.background = element_rect(fill = "gray94"), 
    legend.position = c(0.5, 0.5))

#### want to look at how well people's compareQ matches to their partner's compareQ
#create new variable for each participant that extracts CompareQ from their partner 
# based on StudentID (from other observations in the dataset) that matches the PartnerID
# of the target participant 
PANASdata$CompareQP <- PANASdata[match(PANASdata$PartnerID, PANASdata$StudentID, nomatch = NA_integer_, incomparables = NULL),
                                  'CompareQ']

#examining when participants' response on CompareQ matches their partner's response
# for those who felt a little less positive
PANASdata$CompareQP[PANASdata$CompareQ == 2] == PANASdata$CompareQ[PANASdata$CompareQ == 2]
sum(PANASdata$CompareQP[PANASdata$CompareQ == 2] == PANASdata$CompareQ[PANASdata$CompareQ == 2])
#7

#and a little more positive
PANASdata$CompareQP[PANASdata$CompareQ == 4] == PANASdata$CompareQ[PANASdata$CompareQ == 4]
sum(PANASdata$CompareQP[PANASdata$CompareQ == 4] == PANASdata$CompareQ[PANASdata$CompareQ == 4])
#8



#to reverse the scores back to normal
PANASdata$PA_diff <- ifelse(PANASdata$CompareQ == 2, PANASdata$PA_diff*-1, 
                            PANASdata$PA_diff*1)

PANASdata$PA_diff[PANASdata$CompareQ == 4]
PANASdata$PA_diff[PANASdata$CompareQ == 2]
