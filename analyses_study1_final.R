library('plyr')
library("effsize")
library('ggplot2')
library('dplyr')
library('metafor')
library('reshape2') 
library('gridExtra')

#Added a dedicated function to perform all these calculations - DL ----
source("effect_size_d_paired_function.R")

#READ IN RAW DATA----

#to read in data for T1 (2018 and 2019)
T1PANAS <- read.csv(file('PANAS_study1_2018+2019_T1_data_anon.csv'), stringsAsFactors = FALSE)

#to see and remove duplicate entries, keeping only the first entry for each
# unique Student ID
duplicated(T1PANAS$StudentID)
sum(duplicated(T1PANAS$StudentID)) #13 duplicates in T1
T1PANAS <- T1PANAS[!duplicated(T1PANAS$StudentID),] #removing duplicates (keeping first entry)
sum(duplicated(T1PANAS$StudentID)) #0 duplicates in T1 data after removing duplicates


#to read in data for T2 (2018 and 2019)
T2PANAS <- read.csv(file('PANAS_study1_2018+2019_T2_data_anon.csv'), stringsAsFactors = FALSE)

#checking for duplicates from T2 data
duplicated(T2PANAS$StudentID)
sum(duplicated(T2PANAS$StudentID)) # 7 duplicates in T2 data
T2PANAS <- T2PANAS[!duplicated(T2PANAS$StudentID),] #removing duplicates (keeping first entry)
sum(duplicated(T2PANAS$StudentID))


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

#remove those with NA values (e.g., where there are T1 response but no T2) 
# to keep only participants with complete data, also removes those with NA for StudentID
# as these could not be matched due to typos (e.g., including a letter, which resulted,
# in NA when anonymizing the data)
PANASdata <- PANASdata[complete.cases(PANASdata), ]


#RUN THE PRE-REGISTERED SCRIPT----

#using subsetting to calculate mean of T1 and T2 PA and NA for each participant
PANASdata$T1_PA <- rowMeans(PANASdata[ , grep('T1_PA', colnames(PANASdata))], na.rm=TRUE)
PANASdata$T1_NA <- rowMeans(PANASdata[ , grep('T1_NA', colnames(PANASdata))], na.rm=TRUE)
PANASdata$T2_PA <- rowMeans(PANASdata[ , grep('T2_PA', colnames(PANASdata))], na.rm=TRUE)
PANASdata$T2_NA <- rowMeans(PANASdata[ , grep('T2_NA', colnames(PANASdata))], na.rm=TRUE)

#calculate difference score, from T1 to T2, for PA and NA for each participant
PANASdata$PA_change <- PANASdata$T2_PA - PANASdata$T1_PA
PANASdata$NA_change <- PANASdata$T2_NA - PANASdata$T1_NA

#Plot the data----
#For positive
ggplot(PANASdata, aes(PA_change)) + 
  geom_histogram(colour = "black", fill = "grey") + 
  xlab("Positive Affect Change T1-T2") + ylab("Count") +
  theme_bw(base_size = 16) + 
  facet_grid(rows = vars(PANASdata$globalNA))
#For negative
ggplot(PANASdata, aes(NA_change)) + 
  geom_histogram(colour = "black", fill = "grey") + 
  theme_bw(base_size = 16) + 
  facet_grid(rows = vars(PANASdata$globalNA))

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
  temp <- effect_size_d_paired(PANASdata$T1_PA[PANASdata$globalPA == i],PANASdata$T2_PA[PANASdata$globalPA == i])
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

#Calculate all statistics for negative----

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
  temp <- effect_size_d_paired(PANASdata$T1_NA[PANASdata$globalNA == i],PANASdata$T2_NA[PANASdata$globalNA == i])
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

# We can perform a meta-analysis based on this data for dz.----
# We can use the escalc function - which reports a bias corrected version of dz (so the estimates are slightly lower)
#for positive:
g <- escalc(ni = sum_data_PA$N, m1i = sum_data_PA$m1, m2i = sum_data_PA$m2, sd1i = sum_data_PA$sd1, sd2i = sum_data_PA$sd2, ri = sum_data_PA$cor, measure = "SMCC")
# We reverse the direction of the effect for individuals in group 2 by using abs(yi)
result <- rma(abs(yi), vi, data=g[c(2,4),], slab = c("a little worse","a little better"))
result
forest(result)

#for negative:
g <- escalc(ni = sum_data_NA$N, m1i = sum_data_NA$m1, m2i = sum_data_NA$m2, sd1i = sum_data_NA$sd1, sd2i = sum_data_NA$sd2, ri = sum_data_NA$cor, measure = "SMCC")
# We reverse the direction of the effect for individuals in group 2 by using abs(yi)
result <- rma(abs(yi), vi, data=g[c(2,4),], slab = c("a little worse","a little better"))
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

#Create summary stats table
sum_data <- rbind(sum_data_PA, sum_data_NA)
round(sum_data[,c(21,8,9,10,17,19,20,11,15,16)],2)

#And the combined means for the little change individuals:
PANASdata$PA_change_rec <- PANASdata$T2_PA - PANASdata$T1_PA
PANASdata$NA_change_rec <- PANASdata$T2_NA - PANASdata$T1_NA

#Need to flip around the scores from group 2 (which are in the opposite direction)
PANASdata$PA_change_rec <- ifelse(PANASdata$globalPA == 2 | PANASdata$globalPA == 1,
                                  PANASdata$PA_change_rec * -1,
                                  PANASdata$PA_change_rec * 1)

PANASdata$NA_change_rec <- ifelse(PANASdata$globalNA == 2 | PANASdata$globalNA == 1,
                                  PANASdata$NA_change_rec * -1,
                                  PANASdata$NA_change_rec * 1)

#Mean only for little change group (ttests to get : positive
mean(PANASdata$PA_change_rec[PANASdata$globalPA == 2 | PANASdata$globalPA == 4])
sd(PANASdata$PA_change_rec[PANASdata$globalPA == 2 | PANASdata$globalPA == 4])
t.test(PANASdata$PA_change_rec[PANASdata$globalPA == 2 | PANASdata$globalPA == 4])

#Mean only for little change group: negative
mean(PANASdata$NA_change_rec[PANASdata$globalNA == 2 | PANASdata$globalNA == 4])
sd(PANASdata$NA_change_rec[PANASdata$globalNA == 2 | PANASdata$globalNA == 4])
t.test(PANASdata$NA_change_rec[PANASdata$globalNA == 2 | PANASdata$globalNA == 4])


# Analysis of individual responses----

sum(PANASdata$PA_change_rec[PANASdata$globalPA == 2 | PANASdata$globalPA == 4] > 0)
# 138/210 in the PA little-changed group showed change greater than zero
# scale points in the direction they said they changed on the global Qs

sum(PANASdata$PA_change_rec[PANASdata$globalPA == 2 | PANASdata$globalPA == 4] == 0)
# 11/210 in the PA little-changed group showed no change
# scale points in the direction they said they changed on the global Qs

sum(PANASdata$PA_change_rec[PANASdata$globalPA == 2 | PANASdata$globalPA == 4] < 0)
# 61/210 in the PA little-changed group showed change in the opposite direction to 
# what they said (e.g., said they felt a little more positive emotions but PA change
# shows they got a little worse)


sum(PANASdata$NA_change_rec[PANASdata$globalNA == 2 | PANASdata$globalNA == 4] > 0)
# 132/188 in the NA little-changed group showed change greater than or equal to zero
# scale points in the direction they said they changed on the global Qs

sum(PANASdata$NA_change_rec[PANASdata$globalNA == 2 | PANASdata$globalNA == 4] == 0)
# 16/188 in the NA little-changed group showed no change

sum(PANASdata$NA_change_rec[PANASdata$globalNA == 2 | PANASdata$globalNA == 4] < 0)
# 40/188 in the NA little-changed group showed change in the opposite direction

#Create a graph showing individual change

longPA = PANASdata

#NOW WE MAKE IT LONG FORMAT and we only want those in the little changed PA group first
longPA = melt(longPA[longPA$globalPA==2 | longPA$globalPA == 4,], 
            measure.vars = c("T1_PA", "T2_PA") )
#looking at the dataframe created, can see that each participant now has a
# T1_PA, and T2_PA entry (i.e., in long format)

longPA$globalPA = as.factor(longPA$globalPA)
levels(longPA$globalPA) <- c("a little less positive", "a little more positive") #renaming the levels


plot1 <- ggplot(data = longPA, aes(x = variable, y = value)) + 
  geom_point(size = 2, color = "black", alpha = 0.3) + #colour points by globalPA
  geom_path(aes(group = StudentID, alpha = 0.3)) + #spaghetti plot
  ylab("Mean Positive Affect") +
  xlab("Time") +
  theme_bw(base_size = 18) +
  facet_grid(~ globalPA) +
  theme(legend.position="none") +
  scale_x_discrete(labels=c("T1_PA" = "Time 1", "T2_PA" = "Time 2")) +
  theme(axis.title.x=element_blank(),
        strip.background =element_rect(fill="white"),
        panel.grid.major.x = element_blank())
plot1

#code to save the plot
# jpeg(file="plot1.jpg",width=2000,height=1400, units = "px", res = 300)
# plot1
# dev.off()

#now do the same for NA
longNA = PANASdata
longNA = melt(longNA[longNA$globalNA==2 | longNA$globalNA == 4,], 
              measure.vars = c("T1_NA", "T2_NA") )

longNA$globalNA = as.factor(longNA$globalNA)
levels(longNA$globalNA) <- c("a little less negative", "a little more negative") #renaming the levels

plot2 <- ggplot(data = longNA, aes(x = variable, y = value)) + 
  geom_point(size = 2, color = "black", alpha = 0.3) + #colour points by globalPA
  geom_path(aes(group = StudentID, alpha = 0.3)) + #spaghetti plot
  ylab("Mean Negative Affect") +
  xlab("Time") +
  theme_bw(base_size = 18) +
  facet_grid(~ globalNA) +
  theme(legend.position="none") +
  scale_x_discrete(labels=c("T1_NA" = "Time 1", "T2_NA" = "Time 2")) +
  theme(axis.title.x=element_blank(),
        strip.background =element_rect(fill="white"),
        panel.grid.major.x = element_blank())
plot2
# jpeg(file="plot2.jpg",width=2000,height=1400, units = "px", res = 300)
# plot2
# dev.off()

#Save combined plot
# jpeg(file="plot_grid.jpg",width=2000,height=2400, units = "px", res = 300)
# grid.arrange(plot1, plot2, nrow = 2)
# dev.off()




#############
#other (exploratory) analyses, and descriptive statistics----


### test of difference between the "little-changed" groups and the "same" group
#for PA
t.test(PANASdata$PA_change_rec[PANASdata$globalPA == 2 | PANASdata$globalPA == 4], 
       PANASdata$PA_change_rec[PANASdata$globalPA == 3])
mean(PANASdata$PA_change_rec[PANASdata$globalPA == 3])
sd(PANASdata$PA_change_rec[PANASdata$globalPA == 3])

#for NA
t.test(PANASdata$NA_change_rec[PANASdata$globalNA == 2 | PANASdata$globalNA == 4], 
       PANASdata$NA_change_rec[PANASdata$globalNA == 3])
mean(PANASdata$NA_change_rec[PANASdata$globalNA == 3])
sd(PANASdata$NA_change_rec[PANASdata$globalNA == 3])

#seems to be a bit of a difference between change scores for those who said
# they were a little less positive compared to those who were a little more positive
t.test(PANASdata$PA_change[PANASdata$globalPA == 2],
       PANASdata$PA_change[PANASdata$globalPA == 4])




######### correlations of global Qs, including with individual items

cor_PA_NA <- PANASdata[42:51]
cor(cor_PA_NA)


# Compare two correlations based on two depenendent groups
# The correlations are overlapping
library(cocor)
cocor(~T2_PA + globalPA | PA_change + globalPA, cor_PA_NA)
cocor(~T2_NA + globalNA | NA_change + globalNA, cor_PA_NA)












PANASdata$Att_change <- PANASdata$T2_PAattentive - PANASdata$T1_PAattentive
PANASdata$Int_change <- PANASdata$T2_PAinterested - PANASdata$T1_PAinterested
PANASdata$Ale_change <- PANASdata$T2_PAalert - PANASdata$T1_PAalert
PANASdata$Exc_change <- PANASdata$T2_PAexcited - PANASdata$T1_PAexcited
PANASdata$Ent_change <- PANASdata$T2_PAenthusiastic - PANASdata$T1_PAenthusiastic
PANASdata$Ins_change <- PANASdata$T2_PAinspired - PANASdata$T1_PAinspired
PANASdata$Pro_change <- PANASdata$T2_PAproud - PANASdata$T1_PAproud
PANASdata$Det_change <- PANASdata$T2_PAdetermined - PANASdata$T1_PAdetermined
PANASdata$Str_change <- PANASdata$T2_PAstrong - PANASdata$T1_PAstrong
PANASdata$Act_change <- PANASdata$T2_PAactive - PANASdata$T1_PAactive

t.test(PANASdata$Att_change[PANASdata$globalPA == 2], 
       PANASdata$Att_change[PANASdata$globalPA == 4])

t.test(PANASdata$Int_change[PANASdata$globalPA == 2], 
       PANASdata$Int_change[PANASdata$globalPA == 4])

t.test(PANASdata$Ale_change[PANASdata$globalPA == 2], 
       PANASdata$Ale_change[PANASdata$globalPA == 4])

t.test(PANASdata$Exc_change[PANASdata$globalPA == 2], 
       PANASdata$Exc_change[PANASdata$globalPA == 4])

t.test(PANASdata$Ent_change[PANASdata$globalPA == 2], 
       PANASdata$Ent_change[PANASdata$globalPA == 4])

t.test(PANASdata$Ins_change[PANASdata$globalPA == 2], 
       PANASdata$Ins_change[PANASdata$globalPA == 4])

t.test(PANASdata$Pro_change[PANASdata$globalPA == 2], 
       PANASdata$Pro_change[PANASdata$globalPA == 4])

t.test(PANASdata$Det_change[PANASdata$globalPA == 2], 
       PANASdata$Det_change[PANASdata$globalPA == 4])

t.test(PANASdata$Str_change[PANASdata$globalPA == 2], 
       PANASdata$Str_change[PANASdata$globalPA == 4])

t.test(PANASdata$Act_change[PANASdata$globalPA == 2], 
       PANASdata$Act_change[PANASdata$globalPA == 4])


PANASdata$Att_change <- rowMeans(PANASdata[ , grep('T1_PA', colnames(PANASdata))], na.rm=TRUE)
