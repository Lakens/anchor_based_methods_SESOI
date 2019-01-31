library('plyr')
library("effsize")
library('ggplot2')
library('dplyr')
library('metafor')
#Added a dedicated function to perform all these calculations - DL ----
source("effect_size_d_paired_function.R")

#READ IN RAW DATA----

#to read in data for T1
T1PANAS <- read.csv(file('PANAS_study1_T1_data_anon.csv'), stringsAsFactors = FALSE)

#to see and remove duplicate entries, keeping only the first entry for each
# unique Student ID
duplicated(T1PANAS$StudentID)
sum(duplicated(T1PANAS$StudentID)) #4 duplicates in T1 data
T1PANAS <- T1PANAS[!duplicated(T1PANAS$StudentID),]
sum(duplicated(T1PANAS$StudentID)) #0 duplicates in T1 data after removing duplicates

#to read in data for T2
T2PANAS <- read.csv(file('PANAS_study1_T2_data_anon.csv'), stringsAsFactors = FALSE)

#checking for duplicates from T2 data
duplicated(T2PANAS$StudentID)
sum(duplicated(T2PANAS$StudentID)) #0 duplicates in T2 data

#renaming the PANAS variables so they reflect they are from T1
names(T1PANAS)[2:21] <- c("T1_PAattentive", "T1_PAinterested", "T1_PAalert", "T1_PAexcited", 
                           "T1_PAenthusiastic", "T1_PAinspired", "T1_PAproud", "T1_PAdetermined", 
                           "T1_PAstrong", "T1_PAactive", "T1_NAdistressed", "T1_NAupset", 
                           "T1_NAhostile", "T1_NAirritable", "T1_NAscared", "T1_NAafraid", 
                           "T1_NAashamed", "T1_NAguilty", "T1_NAnervous", "T1_NAjittery")
head(T1PANAS)

#renaming variables from T2 to reflect they are from T2
names(T2PANAS)[2:23] <- c( "T2_PAattentive", "T2_PAinterested", "T2_PAalert", "T2_PAexcited", 
                            "T2_PAenthusiastic", "T2_PAinspired", "T2_PAproud", "T2_PAdetermined", 
                            "T2_PAstrong", "T2_PAactive", "T2_NAdistressed", "T2_NAupset", 
                            "T2_NAhostile", "T2_NAirritable", "T2_NAscared", "T2_NAafraid", 
                            "T2_NAashamed", "T2_NAguilty", "T2_NAnervous", "T2_NAjittery",
                            "globalPA", "globalNA")
head(T2PANAS)

#to merge the data sets, matching T1 to T2 responses, by student ID,
# and matching only the first response for those with duplicate Student IDs using plyr package
PANASdata <- join(T1PANAS, T2PANAS, by = c("StudentID"), type="full")

#remove those with NA values (e.g., where there are T1 response but no T2) to keep only participants with complete data
PANASdata <- PANASdata[complete.cases(PANASdata), ]


#RUN THE PRE-REGISTERED SCRIPT----

#using subsetting to calculate mean of T1 and T2 PA and NA for each participant
PANASdata$T1_PA <- rowMeans(PANASdata[ , grep('T1_PA', colnames(PANASdata))], na.rm=TRUE)
PANASdata$T1_NA <- rowMeans(PANASdata[ , grep('T1_NA', colnames(PANASdata))], na.rm=TRUE)
PANASdata$T2_PA <- rowMeans(PANASdata[ , grep('T2_PA', colnames(PANASdata))], na.rm=TRUE)
PANASdata$T2_NA <- rowMeans(PANASdata[ , grep('T2_NA', colnames(PANASdata))], na.rm=TRUE)

# for subset of participants who were a little less positive/negative on PA & NA, 
# respectively, (i.e., those who selected option 2 on the global Qs)
# we reverse the sign so that change scores will be comparable in the same
# direction as for those who said they were a little more positive/negative 
# i.e., those who selected option 4 on the global Qs

#changed from original code Farid (as to not overwrite the original T1_PA etc variables)
#also recode much changed group (PANASdata$globalPA == 1)

PANASdata$T1_PA_rec <- ifelse(PANASdata$globalPA == 2 | PANASdata$globalPA == 1,
                              PANASdata$T1_PA*-1,
                              PANASdata$T1_PA*1)

PANASdata$T2_PA_rec <- ifelse(PANASdata$globalPA == 2 | PANASdata$globalPA == 1,
                              PANASdata$T2_PA*-1,
                              PANASdata$T2_PA*1)

PANASdata$T1_NA_rec <- ifelse(PANASdata$globalNA == 2 | PANASdata$globalNA == 1,
                              PANASdata$T1_NA*-1,
                              PANASdata$T1_NA*1)

PANASdata$T2_NA_rec <- ifelse(PANASdata$globalNA == 2 | PANASdata$globalNA == 1,
                              PANASdata$T2_NA*-1,
                              PANASdata$T2_NA*1)



#calculate difference score, from T1 to T2, for PA and NA for each participant
PANASdata$PA_change <- PANASdata$T2_PA_rec - PANASdata$T1_PA_rec
PANASdata$NA_change <- PANASdata$T2_NA_rec - PANASdata$T1_NA_rec

# We are interested in the minimal change people indicate as a little better or worse (T2-T1).
# These changes can be in the positive or negative diferection - so just taking the mean will cancel them out.
# Person 1: T1 score 4, T2 score 3, reports feeling a little worse, change score = 3-4 = -1
# Person 2: T1 score 4, T2 score 3, reports feeling a little better, change score = 3-4 = -1
# Person 3: T1 score 3, T2 score 4, reports feeling a little better, change score = 1
# Person 4: T1 score 3, T2 score 4, reports feeling a little worse, change score = 1
# Since we are not interested in the direction, we want only positive change scores. 
#  For person 1 and 3 this is simple - we flip the score of person 1.
# What do we do with the people (2 and 4) who responded in the opposite direction? 
# They associate feeling a little better or worse with a change in the opposite direction. So these should get negative scores. 
# So we want the 4 scores to be: 1, -1, 1, -1. 

# Farid originally worked with recoding the T1 and T2 scores. 
# Scores for people who feel worse are multiplied by -1. This makes all their scores negative. 
# If T2 scores are lower than T1 (e.g., 2 vs 3) and they report feeling worse, their change score is now a 1. 

#I think it is easier (and the same) to just flip change scores.

PANASdata$PA_change_rec <- PANASdata$T2_PA - PANASdata$T1_PA
PANASdata$NA_change_rec <- PANASdata$T2_NA - PANASdata$T1_NA

PANASdata$PA_change_rec <- ifelse(PANASdata$globalPA == 2 | PANASdata$globalPA == 1,
                                   PANASdata$PA_change_rec * -1,
                                   PANASdata$PA_change_rec * 1)

PANASdata$NA_change_rec <- ifelse(PANASdata$globalNA == 2 | PANASdata$globalNA == 1,
                                  PANASdata$NA_change_rec * -1,
                                  PANASdata$NA_change_rec * 1)

#Total mean
mean(PANASdata$PA_change_rec)
sd(PANASdata$PA_change_rec)

#Mean only for little change group
mean(PANASdata$PA_change_rec[PANASdata$globalPA == 2 | PANASdata$globalPA == 4])
sd(PANASdata$PA_change_rec[PANASdata$globalPA == 2 | PANASdata$globalPA == 4])

ggplot(PANASdata, aes(PA_change_rec))  + 
  geom_histogram(colour = "black", fill = "grey") +
  xlab("Positive Affect Time 1") + ylab("Count") + theme_bw(base_size = 16)

# We calculate numbers using only the little changed group mostly. 
# Let's create 2 dataframes containing only those people
# This is especially more useful for plotting in ggplot

PANASdata_little_change_PA <- filter(PANASdata, PANASdata$globalPA == 2 | PANASdata$globalPA == 4)
PANASdata_little_change_NA <- filter(PANASdata, PANASdata$globalNA == 2 | PANASdata$globalNA == 4)

#Also for no change and much change groups.
PANASdata_no_change_PA <- filter(PANASdata, PANASdata$globalPA == 3)
PANASdata_no_change_NA <- filter(PANASdata, PANASdata$globalNA == 3)
PANASdata_much_change_PA <- filter(PANASdata, PANASdata$globalPA == 1 | PANASdata$globalPA == 5)
PANASdata_much_change_NA <- filter(PANASdata, PANASdata$globalNA == 1 | PANASdata$globalNA == 5)

#for all those in the little changed group (i.e., 2 or 4 on global Qs), for PA and NA separately,
# calculate the mean change in PA and NA (i.e., mean of PA_change and NA_change)
mPA = mean(PANASdata_little_change_PA$PA_change)
sPA = sd(PANASdata_little_change_PA$PA_change)
mPA #[1] 0.2719626
sPA #[1] 0.5390058

mNA = mean(PANASdata_little_change_NA$NA_change)
sNA = sd(PANASdata_little_change_NA$NA_change)
mNA #[1] 0.2887755
sNA #[1] 0.4592425

#calculate Cohen's dz, which accounts for the correlation between T1 and T2 measures.
# cohen's dz is actually the same as standardised response mean in Walters & Brazier (2003)
cohenszPA = mPA/sPA
cohenszPA #[1] 0.5045634

cohenszNA = mNA/sNA
cohenszNA #[1] 0.6288084

#can also use effsize package to get CIs

cohen.d(PANASdata$T1_PA_rec[PANASdata$globalPA == 2 | PANASdata$globalPA == 4],
        PANASdata$T2_PA_rec[PANASdata$globalPA == 2 | PANASdata$globalPA == 4],
        pooled = TRUE, 
        paired = TRUE,
        na.rm = FALSE, 
        hedges.correction = FALSE,
        conf.level = 0.95)

cohen.d(PANASdata$T1_NA_rec[PANASdata$globalNA == 2 | PANASdata$globalNA == 4],
        PANASdata$T2_NA_rec[PANASdata$globalNA == 2 | PANASdata$globalNA == 4],
        pooled = TRUE,
        paired = TRUE,
        na.rm = FALSE, 
        hedges.correction = FALSE,
        conf.level = 0.95)

#However, Farid, these 95% CI do not seem to be correct. 
#Let's use some random data
x<-c(441,462,445,446,426,459,459,423,400) 
y<-c(443,461,441,459,441,464,466,430,400)

#A t-test on the data above show a p-value close to 0.05, so the CI should border on 0.
t.test(y, x, paired = TRUE, conf.level = 0.95)
#But it doesn't: 
cohen.d(x,
        y,
        pooled = TRUE,
        paired = TRUE,
        na.rm = FALSE, 
        hedges.correction = FALSE,
        conf.level = 0.95)


#Just to show in my formula the 95% CI is close to 0 for the data above. 
effect_size_d_paired(x, y)

#For positive affect
effect_size_d_paired(PANASdata_little_change_PA$T1_PA_rec, PANASdata_little_change_PA$T2_PA_rec)
#For negative affect
effect_size_d_paired(PANASdata_little_change_NA$T1_NA_rec, PANASdata_little_change_NA$T2_NA_rec)

#Not reported in article, but out of interest, now for no changed and much changed group
#For positive
effect_size_d_paired(PANASdata_no_change_PA$T1_PA_rec, PANASdata_no_change_PA$T2_PA_rec)
effect_size_d_paired(PANASdata_much_change_PA$T1_PA_rec, PANASdata_much_change_PA$T2_PA_rec)

#For negative
effect_size_d_paired(PANASdata_no_change_NA$T1_NA_rec, PANASdata_no_change_NA$T2_NA_rec)
effect_size_d_paired(PANASdata_much_change_NA$T1_NA_rec, PANASdata_much_change_NA$T2_NA_rec)

#### to calculate cohen's dav effect size need Ms, SDs
# with subsetting to use only those in the "little-changed" groups
# but, for the SDs of mean PA at T1 and T2 separately for the little-changed groups, 
# need to reverse back the scores for those who said they got a little worse, 
# note: dav uses average SD of both repeated measures as standardiser,

ggplot(PANASdata, aes(T1_PA))  + 
  geom_histogram(colour = "black", fill = "grey") +
  xlab("Positive Affect Time 1") + ylab("Count") + theme_bw(base_size = 16)

ggplot(PANASdata_little_change_PA, aes(T1_PA))  + 
  geom_histogram(colour = "black", fill = "grey") +
  xlab("Positive Affect Time 1") + ylab("Count") + theme_bw(base_size = 16)

sdT1_PA = sd(PANASdata$T1_PA[PANASdata$globalPA == 2 | PANASdata$globalPA == 4])
sdT1_PA
sdT2_PA = sd(PANASdata$T2_PA[PANASdata$globalPA == 2 | PANASdata$globalPA == 4])
sdT2_PA

sPAav = sqrt((sdT2_PA^2 + sdT1_PA^2)/2)

cohen_d_av_PA = mPA/sPAav
cohen_d_av_PA


#now for NA
sdT1_NA = sd(PANASdata$T1_NA[PANASdata$globalNA == 2 | PANASdata$globalNA == 4])
#This is from the following distribution of values:
ggplot(PANASdata_little_change_NA, aes(T1_NA))  + 
  geom_histogram(colour = "black", fill = "grey") +
  xlab("Negative Affect Time 1") + ylab("Count") + theme_bw(base_size = 16)

sdT2_NA = sd(PANASdata$T2_NA[PANASdata$globalNA == 2 | PANASdata$globalNA == 4])

sNAav = sqrt((sdT2_NA^2 + sdT1_NA^2)/2)

cohen_d_av_NA = mNA/sNAav
cohen_d_av_NA

#Double check by Daniel
#We want the change scores based on the recoded data (otherwise positive and negative changes counteract)
x <- PANASdata$T1_NA_rec[PANASdata$globalNA == 2 | PANASdata$globalNA == 4]
y <- PANASdata$T2_NA_rec[PANASdata$globalNA == 2 | PANASdata$globalNA == 4]
N <- length(x) #number of pairs
#But the SD of the recoded scores is nonsensicle, as you can see from the plot:
ggplot(PANASdata, aes(T1_NA_rec))  + 
  geom_histogram(colour = "black", fill = "grey") +
  xlab("Negative Affect Time 1") + ylab("Count") + theme_bw(base_size = 16)
ggplot(PANASdata, aes(T2_NA_rec))  + 
  geom_histogram(colour = "black", fill = "grey") +
  xlab("Negative Affect Time 2") + ylab("Count") + theme_bw(base_size = 16)
# So we take the sd's from the original not transformed distribution:
sd1 <- sd(PANASdata$T1_NA[PANASdata$globalNA == 2 | PANASdata$globalNA == 4]) #standard deviation of measurement 1
sd2 <- sd(PANASdata$T2_NA[PANASdata$globalNA == 2 | PANASdata$globalNA == 4]) #standard deviation of measurement 2
s_diff <- sd(x-y) #standard deviation of the difference scores
s_av <- sqrt((sd1^2+sd2^2)/2) #averaged standard deviation of both measurements

es <- effect_size_d_paired(x, y)
# effect size using metafor
escalc(n1i = N, n2i = N, m1i = mean(x), m2i = mean(y), sd1i = sd1, sd2i = sd2, measure = "SMD")

m_diff <- mean(y-x)
d_av <- m_diff/s_av
d_av
d_av_unb <- (1-(3/(4*(N-1)-1)))*d_av
d_av_unb

t_value <- m_diff/(s_diff/sqrt(N))

nct_limits <- conf.limits.nct(t.value = t_value, df = N-1, conf.level = 0.95)
ci_l_d_av <- nct_limits$Lower.Limit*s_diff/(s_av*sqrt(N))
ci_u_d_av <- nct_limits$Upper.Limit*s_diff/(s_av*sqrt(N))
ci_l_d_av
ci_u_d_av

# You could also do this seperately for the 2 group and 4 group. 
# In this case, effect sizes should be in opposite directions but the calculation is easier. 
# For negative
x <- PANASdata$T1_NA_rec[PANASdata$globalNA == 2]
y <- PANASdata$T2_NA_rec[PANASdata$globalNA == 2]
es1 <- effect_size_d_paired(x, y)
es1

x <- PANASdata$T1_NA_rec[PANASdata$globalNA == 4]
y <- PANASdata$T2_NA_rec[PANASdata$globalNA == 4]
es2 <- effect_size_d_paired(x, y)
es2

# For positive
x <- PANASdata$T1_PA_rec[PANASdata$globalPA == 2]
y <- PANASdata$T2_PA_rec[PANASdata$globalPA == 2]
es3 <- effect_size_d_paired(x, y)
es3

x <- PANASdata$T1_PA_rec[PANASdata$globalPA == 2]
y <- PANASdata$T2_PA_rec[PANASdata$globalPA == 2]
es3 <- effect_size_d_paired(x, y)
es3


x <- PANASdata$T1_PA_rec[PANASdata$globalPA == 4]
y <- PANASdata$T2_PA_rec[PANASdata$globalPA == 4]
es4 <- effect_size_d_paired(x, y)
es4

#What is the no change group effect?
x <- PANASdata$T1_PA_rec[PANASdata$globalPA == 3]
y <- PANASdata$T2_PA_rec[PANASdata$globalPA == 3]
effect_size_d_paired(x, y)


ggplot(PANASdata, aes(T1_NA)) + 
  geom_histogram(colour = "black", fill = "grey") + 
  xlab("Negative Affect Time 1") + ylab("Count") +
  theme_bw(base_size = 16) + 
  facet_grid(rows = vars(PANASdata$globalNA))

ggplot(PANASdata, aes(T2_NA)) + 
  geom_histogram(colour = "black", fill = "grey") + 
  theme_bw(base_size = 16) + 
  facet_grid(rows = vars(PANASdata$globalNA))

ggplot(PANASdata, aes(T1_NA-T2_NA)) + 
  geom_histogram(colour = "black", fill = "grey") + 
  theme_bw(base_size = 16) + 
  facet_grid(rows = vars(PANASdata$globalNA))

ggplot(PANASdata, aes(T1_NA_rec-T2_NA_rec)) + 
  geom_histogram(colour = "black", fill = "grey") + 
  theme_bw(base_size = 16) + 
  facet_grid(rows = vars(PANASdata$globalNA))

ggplot(PANASdata, aes(T2_PA)) + 
  geom_histogram(colour = "black", fill = "grey") + 
  theme_bw(base_size = 16) + 
  facet_grid(rows = vars(PANASdata$globalPA))

ggplot(PANASdata, aes(T2_PA)) + 
  geom_histogram(colour = "black", fill = "grey") + 
  theme_bw(base_size = 16) + 
  facet_grid(rows = vars(PANASdata$globalPA))

ggplot(PANASdata, aes(T1_PA-T2_PA)) + 
  geom_histogram(colour = "black", fill = "grey") + 
  theme_bw(base_size = 16) + 
  facet_grid(rows = vars(PANASdata$globalPA))

ggplot(PANASdata, aes(T1_PA_rec-T2_PA_rec)) + 
  geom_histogram(colour = "black", fill = "grey") + 
  theme_bw(base_size = 16) + 
  facet_grid(rows = vars(PANASdata$globalPA))

# What if we analyzde this using meta-analysis for the 2 groups?

x <- PANASdata$T1_PA_rec[PANASdata$globalPA == 2]
y <- PANASdata$T2_PA_rec[PANASdata$globalPA == 2]
es3 <- effect_size_d_paired(x, y)
es3
g1 <- escalc(n1i = es3$N, n2i = es3$N, m1i = es3$m1, m2i = es3$m2, sd1i = es3$sd1, sd2i = es3$sd2, measure = "SMD")


x <- PANASdata$T1_PA_rec[PANASdata$globalPA == 4]
y <- PANASdata$T2_PA_rec[PANASdata$globalPA == 4]
es4 <- effect_size_d_paired(x, y)
es4





es_df <- as.data.frame(rbind(unlist(es1[c(4,13)]),unlist(es3[c(4,13)])))

library('metafor')
g <- escalc(ni = N, yi = d_av, data = es_df, measure = "SMD")

result <- rma(yi = d_av, vi = N, data=es_df)
result
forest(result)





# the correlations between T1 and T2 measures of PA and NA for little-changed groups
cor(PANASdata$T1_PA[PANASdata$globalPA == 2 | PANASdata$globalPA == 4],
    PANASdata$T2_PA[PANASdata$globalPA == 2 | PANASdata$globalPA == 4])

cor(PANASdata$T1_NA[PANASdata$globalNA == 2 | PANASdata$globalNA == 4],
    PANASdata$T2_NA[PANASdata$globalNA == 2 | PANASdata$globalNA == 4])

###WHY IS THIS DONE? BETTER TO RENAME?
# now to reverse back
PANASdata$T1_PA <- ifelse(PANASdata$globalPA == 2,PANASdata$T1_PA*-1,
                          PANASdata$T1_PA*1)
PANASdata$T2_PA <- ifelse(PANASdata$globalPA == 2,PANASdata$T2_PA*-1,
                          PANASdata$T2_PA*1)

PANASdata$T1_NA <- ifelse(PANASdata$globalNA == 2,PANASdata$T1_NA*-1,
                          PANASdata$T1_NA*1)
PANASdata$T2_NA <- ifelse(PANASdata$globalNA == 2,PANASdata$T2_NA*-1,
                          PANASdata$T2_NA*1)


###############
# other (exploratory) analyses, and descriptive statistics
###############


# number of participants in the "little changed" groups
sum(PANASdata$globalPA == 2 | PANASdata$globalPA == 4) # n = 107
sum(PANASdata$globalNA == 2 | PANASdata$globalNA == 4) # n = 98

### how many in the little changed groups showed change >= MID estimate
sum(PANASdata$PA_change[PANASdata$globalPA == 2 | PANASdata$globalPA == 4] >= 0.27)
# 47/107 in the PA little-changed group showed change at MID level or higher

sum(PANASdata$PA_change[PANASdata$globalPA == 2 | PANASdata$globalPA == 4] >= 0)
# 77/107 in the PA little-changed group showed change greater than or equal to zero
# scale points in the direction they said they changed on the global Qs

sum(PANASdata$PA_change[PANASdata$globalPA == 2 | PANASdata$globalPA == 4] < 0)
# 30/107 in the PA little-changed group showed change in the opposite direction to 
# what they said (e.g., said they felt a little more positive emotions but PA change
# shows they got a little worse)


sum(PANASdata$NA_change[PANASdata$globalNA == 2 | PANASdata$globalNA == 4] >= 0.29)
# 47/98 in the NA little-changed group showed change at MID level or higher

sum(PANASdata$NA_change[PANASdata$globalNA == 2 | PANASdata$globalNA == 4] >= 0)
# 75/98 in the NA little-changed group showed change greater than or equal to zero
# scale points in the direction they said they changed on the global Qs

sum(PANASdata$NA_change[PANASdata$globalNA == 2 | PANASdata$globalNA == 4] < 0)
# 23/98 in the NA little-changed group showed change in the opposite direction


#### to plot this (PA first then NA)
library('ggplot2')
#need to make it long format
library('reshape2') 

longPA = PANASdata

# first, to reverse back scores for those who were a little less positive
longPA$T1_PA <- ifelse(longPA$globalPA == 2,longPA$T1_PA*-1,
                       longPA$T1_PA*1)
longPA$T2_PA <- ifelse(longPA$globalPA == 2,longPA$T2_PA*-1,
                       longPA$T2_PA*1)
#NOW WE MAKE IT LONG FORMAT and we only want those in the little changed PA group first
longPA = melt(longPA[longPA$globalPA==2 | longPA$globalPA == 4,], 
            measure.vars = c("T1_PA", "T2_PA") )
head(longPA)
#looking at the dataframe created, can see that each participant now has a
# T1_PA, and T2_PA entry (i.e., in long format)

longPA$globalPA = as.factor(longPA$globalPA)
levels(longPA$globalPA) <- c("a little less positive", "a little more positive") #renaming the levels


ggplot(data = longPA, aes(x = variable, y = value)) + 
    geom_point(size = 2, aes(color = globalPA)) + #colour points by globalPA
    geom_path(aes(group = StudentID, color = globalPA)) + #spaghetti plot
    ylab("Positive Affect (mean)") +
    xlab("Time") +
    theme_bw() +
    facet_grid(~ globalPA) + theme(panel.grid.major = element_line(colour = "gray86", 
    size = 0.6), panel.grid.minor = element_line(size = 0.2), 
    strip.background = element_rect(colour = "gray96"), 
    legend.background = element_rect(fill = "gray94"), 
    legend.position = c(0.5, 0.5))


#now do the same for NA

longNA = PANASdata

# first, to reverse back scores for those who were a little less positive
longNA$T1_NA <- ifelse(longNA$globalNA == 2,longNA$T1_NA*-1,
                       longNA$T1_NA*1)
longNA$T2_NA <- ifelse(longNA$globalNA == 2,longNA$T2_NA*-1,
                       longNA$T2_NA*1)
#NOW WE MAKE IT LONG FORMAT and we only want those in the little changed PA group first
longNA = melt(longNA[longNA$globalNA==2 | longNA$globalNA == 4,], 
              measure.vars = c("T1_NA", "T2_NA") )
head(longNA)
#looking at the dataframe created, can see that each participant now has a
# T1_PA, and T2_PA entry (i.e., in long format)

longNA$globalNA = as.factor(longNA$globalNA)
levels(longNA$globalNA) <- c("a little less negative", "a little more negative") #renaming the levels


ggplot(data = longNA, aes(x = variable, y = value)) + 
    geom_point(size = 2, aes(color = globalNA)) + #colour points by globalPA
    geom_path(aes(group = StudentID, color = globalNA)) + #spaghetti plot
    ylab("Negative Affect (mean)") +
    xlab("Time") +
    theme_bw() +
    facet_grid(~ globalNA) + theme(panel.grid.major = element_line(colour = "gray86", 
        size = 0.6), panel.grid.minor = element_line(size = 0.2), 
    strip.background = element_rect(colour = "gray96"), 
    legend.background = element_rect(fill = "gray94"), 
    legend.position = c(0.5, 0.5))




#### looking at the little less vs little more groups separately,

sum(PANASdata$globalPA == 2) #a little less positive
sum(PANASdata$globalPA == 4) #a little more positive
sum(PANASdata$globalNA == 2) #a little less negative
sum(PANASdata$globalNA == 4) #a little more negative

#is the MID different for getting a little less vs a little more of affect
mean(PANASdata$PA_change[PANASdata$globalPA == 2]) #m=0.55
sd(PANASdata$PA_change[PANASdata$globalPA == 2]) #sd=0.60

mean(PANASdata$PA_change[PANASdata$globalPA == 4])#m=0.12
sd(PANASdata$PA_change[PANASdata$globalPA == 4]) #sd=0.44
#seems to be a bit of a difference between change scores for those who said
# they were a little less positive compared to those who were a little more positive
t.test(PANASdata$PA_change[PANASdata$globalPA == 2],
       PANASdata$PA_change[PANASdata$globalPA == 4])


mean(PANASdata$NA_change[PANASdata$globalNA == 2]) #m=0.30
sd(PANASdata$NA_change[PANASdata$globalNA == 2]) #sd=0.42

mean(PANASdata$NA_change[PANASdata$globalNA == 4]) #m=0.28
sd(PANASdata$NA_change[PANASdata$globalNA == 4]) #sd=0.53

t.test(PANASdata$NA_change[PANASdata$globalNA == 2],
       PANASdata$NA_change[PANASdata$globalNA == 4])


sum(PANASdata$PA_change[PANASdata$globalPA == 2] >= 0.55)
# 17/38 showed >= MID change

sum(PANASdata$PA_change[PANASdata$globalPA == 4] >= 0.12)
#34/69 showed >= MID change

sum(PANASdata$NA_change[PANASdata$globalNA == 2] >= 0.297)
# 28/60 showed >= MID change

sum(PANASdata$NA_change[PANASdata$globalNA == 4] >= 0.276)
# 19/38 showed >= MID change

# number of people who showed change opposite to what they reported
sum(PANASdata$PA_change[PANASdata$globalPA == 2] < 0) # 6/38
sum(PANASdata$PA_change[PANASdata$globalPA == 4] < 0) # 24/69
sum(PANASdata$NA_change[PANASdata$globalNA == 2] < 0) # 13/60
sum(PANASdata$NA_change[PANASdata$globalNA == 4] < 0) # 10/38



############ looking at those who felt the same
sum(PANASdata$globalPA == 3)
mean(PANASdata$PA_change[PANASdata$globalPA == 3])
sd(PANASdata$PA_change[PANASdata$globalPA == 3])

sum(PANASdata$globalNA == 3)
mean(PANASdata$NA_change[PANASdata$globalNA == 3])
sd(PANASdata$NA_change[PANASdata$globalNA == 3])

### test of difference between the "little-changed" groups and the "same" group
PAsame <- PANASdata$PA_change[PANASdata$globalPA == 3]
PAlittle <- PANASdata$PA_change[PANASdata$globalPA == 2 | PANASdata$globalPA == 4]
t.test(PAsame, PAlittle)

NAsame <- PANASdata$NA_change[PANASdata$globalNA == 3]
NAlittle <- PANASdata$NA_change[PANASdata$globalNA == 2 | PANASdata$globalNA == 4]
t.test(NAsame, NAlittle)


cohen.d(PAsame, PAlittle,
        pooled=TRUE,paired=FALSE,
        na.rm=FALSE, hedges.correction=FALSE,
        conf.level=0.95)

cohen.d(NAsame, NAlittle,
        pooled=TRUE,paired=FALSE,
        na.rm=FALSE, hedges.correction=FALSE,
        conf.level=0.95)




######### correlations of global Qs, including with individual items

###### changes in individual items
PANASdata$PAattentive = PANASdata$T2_PAattentive - PANASdata$T1_PAattentive
PANASdata$PAinterested = PANASdata$T2_PAinterested - PANASdata$T1_PAinterested
PANASdata$PAalert = PANASdata$T2_PAalert - PANASdata$T1_PAalert
PANASdata$PAexcited = PANASdata$T2_PAexcited - PANASdata$T1_PAexcited
PANASdata$PAenthusiastic = PANASdata$T2_PAenthusiastic - PANASdata$T1_PAenthusiastic
PANASdata$PAinspired = PANASdata$T2_PAinspired - PANASdata$T1_PAinspired
PANASdata$PAproud = PANASdata$T2_PAproud - PANASdata$T1_PAproud
PANASdata$PAdetermined = PANASdata$T2_PAdetermined - PANASdata$T1_PAdetermined
PANASdata$PAstrong = PANASdata$T2_PAstrong - PANASdata$T1_PAstrong
PANASdata$PAactive = PANASdata$T2_PAactive - PANASdata$T1_PAactive

PANASdata$NAdistressed = PANASdata$T2_NAdistressed - PANASdata$T1_NAdistressed
PANASdata$NAupset = PANASdata$T2_NAupset - PANASdata$T1_NAupset
PANASdata$NAhostile = PANASdata$T2_NAhostile - PANASdata$T1_NAhostile
PANASdata$NAirritable = PANASdata$T2_NAirritable - PANASdata$T1_NAirritable
PANASdata$NAscared = PANASdata$T2_NAscared - PANASdata$T1_NAscared
PANASdata$NAafraid = PANASdata$T2_NAafraid - PANASdata$T1_NAafraid
PANASdata$NAashamed = PANASdata$T2_NAashamed - PANASdata$T1_NAashamed
PANASdata$NAguilty = PANASdata$T2_NAguilty - PANASdata$T1_NAguilty
PANASdata$NAnervous = PANASdata$T2_NAnervous - PANASdata$T1_NAnervous
PANASdata$NAjittery = PANASdata$T2_NAjittery - PANASdata$T1_NAjittery

## but also need to reverse back the change scores for those who felt
# a little less postive/negative because the correlation of this change score
# for each participant wwith the global question will only make sense if
# the little less groups actually do have negative scores as would be expected
PANASdata$T1_PA <- ifelse(PANASdata$globalPA == 2,PANASdata$T1_PA*-1,
                          PANASdata$T1_PA*1)
PANASdata$T2_PA <- ifelse(PANASdata$globalPA == 2,PANASdata$T2_PA*-1,
                          PANASdata$T2_PA*1)

PANASdata$T1_NA <- ifelse(PANASdata$globalNA == 2,PANASdata$T1_NA*-1,
                          PANASdata$T1_NA*1)
PANASdata$T2_NA <- ifelse(PANASdata$globalNA == 2,PANASdata$T2_NA*-1,
                          PANASdata$T2_NA*1)

#calculate difference score, from T1 to T2, for PA and NA for each participant
PANASdata$PA_change <- PANASdata$T2_PA - PANASdata$T1_PA
PANASdata$NA_change <- PANASdata$T2_NA - PANASdata$T1_NA


#correlation matrix - extracting relevant data first to do cor matrix for dataframe
# to see correlation between global Qs and individual items
individual <- PANASdata[42:69]
head(individual)
individualPA <- individual[, grep('PA', colnames(individual))]
head(individualPA)
individualNA <- individual[, grep('NA', colnames(individual))]
head(individualNA)

cor(individualPA)
cor(individualNA)
?cor.test()
# results show that the global Qs are more strongly related to the change scores
# than to any of the other variables, including the T2 scores, suggesting that
# the criticism of this approach that the recall correlates more strongly with
# current state than it does with change does not hold.







##Plotting Data to Explore Assumptions----

ggplot(PANASdata, aes(T1_PA_rec))  + 
  geom_histogram(colour = "black", fill = "grey") +
  xlab("Positive Affect Time 1") + ylab("Count") + theme_bw(base_size = 16)

ggplot(PANASdata, aes(T2_PA_rec))  + 
  geom_histogram(colour = "black", fill = "grey") +
  xlab("Positive Affect Time 2") + ylab("Count") + theme_bw(base_size = 16)

ggplot(PANASdata, aes(T1_NA))  + 
  geom_histogram(colour = "black", fill = "grey") +
  xlab("Negative Affect Time 1") + ylab("Count") + theme_bw(base_size = 16)

ggplot(PANASdata, aes(T2_NA))  + 
  geom_histogram(colour = "black", fill = "grey") +
  xlab("Negative Affect Time 2") + ylab("Count") + theme_bw(base_size = 16)

#We will mainly analyse the change scores
ggplot(PANASdata, aes(PA_change))  + 
  geom_histogram(colour = "black", fill = "grey") +
  xlab("Change in Positive Affect") + ylab("Count") + theme_bw(base_size = 16)

ggplot(PANASdata, aes(NA_change))  + 
  geom_histogram(colour = "black", fill = "grey") +
  xlab("Change in Negative Affect") + ylab("Count") + theme_bw(base_size = 16)

#But we calculate using only the little changed group mostly so let's plot these:
#Create 2 dataframe containing only those people who answer a little change on the global transition item for positive and negative affect

PANASdata_little_change_PA <- filter(PANASdata, PANASdata$globalPA == 2 | PANASdata$globalPA == 4)
PANASdata_little_change_NA <- filter(PANASdata, PANASdata$globalNA == 2 | PANASdata$globalNA == 4)

ggplot(PANASdata_little_change_PA, aes(PA_change))  + 
  geom_histogram(colour = "black", fill = "grey") +
  xlab("Positive Affect Time 2 Little Change Group") + ylab("Count") + theme_bw(base_size = 16)

ggplot(PANASdata_little_change_PA, aes(NA_change))  + 
  geom_histogram(colour = "black", fill = "grey") +
  xlab("Negative Affect Time 2 Little Change Group") + ylab("Count") + theme_bw(base_size = 16)

ggplot(PANASdata_little_change_PA, aes(T1_PA))  + 
  geom_histogram(colour = "black", fill = "grey") +
  xlab("Positive Affect Time 2 Little Change Group") + ylab("Count") + theme_bw(base_size = 16)

ggplot(PANASdata_little_change_PA, aes(T1_PA_rec))  + 
  geom_histogram(colour = "black", fill = "grey") +
  xlab("Positive Affect Time 2 Little Change Group") + ylab("Count") + theme_bw(base_size = 16)



# The results differ because we are weighing the effects, and the variances, as a function of the sample sizes.
# I think 1) You probably simply don't want to combine the little worse and little better groups.
# 2) If you want to, the meta-analysis approach seems best. 
# # It makes sense to always report a forest plot. Gives both studies and the meta-analytic ES.

#Calculate all statistics for positive----

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
  temp <- t.test(PANASdata$PA_change[PANASdata$globalPA == i])
  sum_data[i,1] <- temp$statistic
  sum_data[i,2] <- temp$parameter
  sum_data[i,3] <- temp$p.value
  sum_data[i,4] <- temp$conf.int[1]
  sum_data[i,5] <- temp$conf.int[2]
  sum_data[i,6] <- temp$estimate[1]
  sum_data[i,7] <- temp$estimate[2]
  temp <- effect_size_d_paired(PANASdata$T2_PA[PANASdata$globalPA == i], PANASdata$T1_PA[PANASdata$globalPA == i])
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

sum_data_PA <- sum_data

#Calculate all statistics for negative

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
  temp <- t.test(PANASdata$NA_change[PANASdata$globalNA == i])
  sum_data[i,1] <- temp$statistic
  sum_data[i,2] <- temp$parameter
  sum_data[i,3] <- temp$p.value
  sum_data[i,4] <- temp$conf.int[1]
  sum_data[i,5] <- temp$conf.int[2]
  sum_data[i,6] <- temp$estimate[1]
  sum_data[i,7] <- temp$estimate[2]
  temp <- effect_size_d_paired(PANASdata$T2_NA[PANASdata$globalNA == i], PANASdata$T1_NA[PANASdata$globalNA == i])
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

sum_data_NA <- sum_data

# We can perform a meta-analysis based on this data for dz.--
# We can use the escalc function - which reports a bias corrected version of dz (so the estimates are slightly lower)
#for positive:
g <- escalc(ni = sum_data_PA$N, m1i = sum_data_PA$m1, m2i = sum_data_PA$m2, sd1i = sum_data_PA$sd1, sd2i = sum_data_PA$sd2, ri = sum_data_PA$cor, measure = "SMCC")
# We reverse the direction of the effect for individuals in group 2 by using abs(yi)
result <- rma(abs(yi), vi, data=g[c(2,4),])
result
forest(result)

#for negative:
g <- escalc(ni = sum_data_NA$N, m1i = sum_data_NA$m1, m2i = sum_data_NA$m2, sd1i = sum_data_NA$sd1, sd2i = sum_data_NA$sd2, ri = sum_data_NA$cor, measure = "SMCC")
# We reverse the direction of the effect for individuals in group 2 by using abs(yi)
result <- rma(abs(yi), vi, data=g[c(2,4),])
result
forest(result)

#We can also meta-analyze d_av (treating the 2 scores as independent)----
# For positive
g <- escalc(n1i = sum_data_PA$N, n2i = sum_data_PA$N, m1i = sum_data_PA$m1, m2i = sum_data_PA$m2, sd1i = sum_data_PA$sd1, sd2i = sum_data_PA$sd2, measure = "SMD")
# We reverse the direction of the effect for individuals in group 2 by using abs(yi)
result <- rma(abs(yi), vi, data=g[c(2,4),])
result
forest(result)

# For negative
g <- escalc(n1i = sum_data_NA$N, n2i = sum_data_NA$N, m1i = sum_data_NA$m1, m2i = sum_data_NA$m2, sd1i = sum_data_NA$sd1, sd2i = sum_data_NA$sd2, measure = "SMD")
# We reverse the direction of the effect for individuals in group 2 by using abs(yi)
result <- rma(abs(yi), vi, data=g[c(2,4),])
result
forest(result)
