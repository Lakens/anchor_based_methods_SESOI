library('plyr')
library("effsize")
library('ggplot2')
library('dplyr')
library('metafor')
library('reshape2') 
library('gridExtra')

PANASdata <- read.csv(file('Studies 1&2, combined.csv'), stringsAsFactors = FALSE)

#assuming all data have been read in with only complete cases remaining----
#THE PRE-REGISTERED SCRIPT----

#Added a dedicated function to perform all these calculations - DL ----
source("effect_size_d_paired_function.R")

#using subsetting to calculate mean of T1 and T2 PA and NA for each participant
PANASdata$T1_PA <- rowMeans(PANASdata[ , grep('T1_PA', colnames(PANASdata))], na.rm=TRUE)
PANASdata$T1_NA <- rowMeans(PANASdata[ , grep('T1_NA', colnames(PANASdata))], na.rm=TRUE)
PANASdata$T2_PA <- rowMeans(PANASdata[ , grep('T2_PA', colnames(PANASdata))], na.rm=TRUE)
PANASdata$T2_NA <- rowMeans(PANASdata[ , grep('T2_NA', colnames(PANASdata))], na.rm=TRUE)

#calculate difference score, from T1 to T2, for PA and NA for each participant
PANASdata$PA_change <- PANASdata$T2_PA - PANASdata$T1_PA
PANASdata$NA_change <- PANASdata$T2_NA - PANASdata$T1_NA

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
#i.e., reverse score those who said they felt "a little less" positive or negative
PANASdata$PA_change_rec <- ifelse(PANASdata$globalPA == 2 | PANASdata$globalPA == 1,
                                  PANASdata$PA_change_rec * -1,
                                  PANASdata$PA_change_rec * 1)

PANASdata$NA_change_rec <- ifelse(PANASdata$globalNA == 2 | PANASdata$globalNA == 1,
                                  PANASdata$NA_change_rec * -1,
                                  PANASdata$NA_change_rec * 1)

#Mean only for little change group: positive
mean(PANASdata$PA_change_rec[PANASdata$globalPA == 2 | PANASdata$globalPA == 4])
sd(PANASdata$PA_change_rec[PANASdata$globalPA == 2 | PANASdata$globalPA == 4])

#Mean only for little change group: negative
mean(PANASdata$NA_change_rec[PANASdata$globalNA == 2 | PANASdata$globalNA == 4])
sd(PANASdata$NA_change_rec[PANASdata$globalNA == 2 | PANASdata$globalNA == 4])






### Non PRE-REGISTERED SCRIPT for visualization & other analyses----


#t-tests to get CIs around the mean for the little changed groups
t.test(PANASdata$PA_change_rec[PANASdata$globalPA == 2 | PANASdata$globalPA == 4])
t.test(PANASdata$NA_change_rec[PANASdata$globalNA == 2 | PANASdata$globalNA == 4])


### number of people who changed in each subgroup

sum(PANASdata$PA_change_rec[PANASdata$globalPA == 2 | PANASdata$globalPA == 4] > 0)
# 280/441 in the PA little-changed group showed change greater than zero
# scale points in the direction they said they changed on the global Qs

sum(PANASdata$PA_change_rec[PANASdata$globalPA == 2 | PANASdata$globalPA == 4] >= 0.26)
# 209/441 in the PA little-changed group had change-scores at least as large as
# 0.26

sum(PANASdata$PA_change_rec[PANASdata$globalPA == 3] >= 0.26)
# 58/441 in the same group showed change at least as large as 0.26


sum(PANASdata$PA_change_rec[PANASdata$globalPA == 2 | PANASdata$globalPA == 4] == 0)
# 23/441 in the PA little-changed group showed no change
# scale points in the direction they said they changed on the global Qs

sum(PANASdata$PA_change_rec[PANASdata$globalPA == 2 | PANASdata$globalPA == 4] < 0)
# 138/441 in the PA little-changed group showed change in the opposite direction to 
# what they said (e.g., said they felt a little more positive emotions but PA change
# shows they got a little worse)


sum(PANASdata$NA_change_rec[PANASdata$globalNA == 2 | PANASdata$globalNA == 4] > 0)
# 274/423 in the NA little-changed group showed change greater than or equal to zero
# scale points in the direction they said they changed on the global Qs

sum(PANASdata$NA_change_rec[PANASdata$globalNA == 2 | PANASdata$globalNA == 4] >= 0.28)
# 205/423 in the NA little changed group showed change at least as large as the 
# minimum subjectively experienced difference of 0.28

sum(PANASdata$NA_change_rec[PANASdata$globalNA == 2 | PANASdata$globalNA == 4] == 0)
# 41/423 in the NA little-changed group showed no change

sum(PANASdata$NA_change_rec[PANASdata$globalNA == 2 | PANASdata$globalNA == 4] < 0)
# 108/423 in the NA little-changed group showed change in the opposite direction




###plots

#NOW WE MAKE IT LONG FORMAT and we only want those in the little changed PA group first
longPA = PANASdata

longPA = melt(longPA[longPA$globalPA==2 | longPA$globalPA == 4,], 
              measure.vars = c("T1_PA", "T2_PA") )
#looking at the dataframe created, can see that each participant now has a
# T1_PA, and T2_PA entry (i.e., in long format)

longPA$globalPA = as.factor(longPA$globalPA)
levels(longPA$globalPA) <- c("a little less positive", "a little more positive") #renaming the levels


plot1 <- ggplot(data = longPA, aes(x = variable, y = value)) + 
  geom_point(size = 2, color = "black", alpha = 0.3) + #colour points by globalPA
  geom_path(aes(group = X, alpha = 0.3)) + #spaghetti plot
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
#jpeg(file="plot1.jpg",width=2000,height=1400, units = "px", res = 300)
#plot1
#dev.off()

#now do the same for NA
longNA = PANASdata
longNA = melt(longNA[longNA$globalNA==2 | longNA$globalNA == 4,], 
              measure.vars = c("T1_NA", "T2_NA") )

longNA$globalNA = as.factor(longNA$globalNA)
levels(longNA$globalNA) <- c("a little less negative", "a little more negative") #renaming the levels

plot2 <- ggplot(data = longNA, aes(x = variable, y = value)) + 
  geom_point(size = 2, color = "black", alpha = 0.3) + #colour points by globalPA
  geom_path(aes(group = X, alpha = 0.3)) + #spaghetti plot
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
#jpeg(file="plot2.jpg",width=2000,height=1400, units = "px", res = 300)
#plot2
#dev.off()

#Save combined plot
#jpeg(file="plot_grid.jpg",width=2000,height=2400, units = "px", res = 300)
#grid.arrange(plot1, plot2, nrow = 2)
#dev.off()



######### OTHER EXPLORATORY ANALYSES
#other (exploratory) analyses, and descriptive statistics----

### to calculate Redelmeier changes
t.test(PANASdata$PA_change[PANASdata$globalPA == 2], 
       PANASdata$PA_change[PANASdata$globalPA == 3])

t.test(PANASdata$PA_change[PANASdata$globalPA == 4], 
       PANASdata$PA_change[PANASdata$globalPA == 3])

t.test(PANASdata$NA_change[PANASdata$globalNA == 2], 
       PANASdata$NA_change[PANASdata$globalNA == 3])

t.test(PANASdata$NA_change[PANASdata$globalNA == 4], 
       PANASdata$NA_change[PANASdata$globalNA == 3])

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


# Compare two correlations based on two dependent groups
# to check whether responses on the global transition question correlate
# more strongly with current state (i.e., T2 response) or change score
library(cocor)
cocor(~T2_PA + globalPA | PA_change + globalPA, PANASdata)
cocor(~T2_NA + globalNA | NA_change + globalNA, PANASdata)

cocor(~T2_PA + globalPA | T1_PA + globalPA, PANASdata)
cocor(~T2_NA + globalNA | T1_NA + globalNA, PANASdata)




#### Baseline Dependency

# testing for differences in estimates depending on baseline (using
# quartile of baseline in PA and NA as grouping for participants)
quantile(PANASdata$T1_PA) 
#ranges of quantiles = 1.0-2.5; 2.5-3.0; 3.0-3.5; 3.5-5.0
quantile(PANASdata$T1_NA) 
#ranges of quantiles = 1.0-1.2; 1.2-1.6; 1.6-2.2; 2.2-4.5


#Mean for little change group: positive, from first through fourth quartiles

PA_1st = PANASdata[PANASdata$T1_PA >= 1 & PANASdata$T1_PA < 2.5,]
#t.test to get CI and mean
t.test(PA_1st$PA_change_rec[PA_1st$globalPA == 2 | PA_1st$globalPA == 4])
sd(PA_1st$PA_change_rec[PA_1st$globalPA == 2 | PA_1st$globalPA == 4])

PA_2nd = PANASdata[PANASdata$T1_PA >= 2.5 & PANASdata$T1_PA < 3,]
#t.test to get CI and mean
t.test(PA_2nd$PA_change_rec[PA_2nd$globalPA == 2 | PA_2nd$globalPA == 4])
sd(PA_2nd$PA_change_rec[PA_2nd$globalPA == 2 | PA_2nd$globalPA == 4])

PA_3rd = PANASdata[PANASdata$T1_PA >= 3 & PANASdata$T1_PA < 3.5,]
#t.test to get CI and mean
t.test(PA_3rd$PA_change_rec[PA_3rd$globalPA == 2 | PA_3rd$globalPA == 4])
sd(PA_3rd$PA_change_rec[PA_3rd$globalPA == 2 | PA_3rd$globalPA == 4])

PA_4th = PANASdata[PANASdata$T1_PA >= 3.5 & PANASdata$T1_PA <= 5,]
#t.test to get CI and mean
t.test(PA_4th$PA_change_rec[PA_4th$globalPA == 2 | PA_4th$globalPA == 4])
sd(PA_4th$PA_change_rec[PA_4th$globalPA == 2 | PA_4th$globalPA == 4])



#Mean for little change group: negative, from first through fourth quartiles

NA_1st = PANASdata[PANASdata$T1_NA >= 1 & PANASdata$T1_NA < 1.2,]
#t-tests to get CIs around the mean for the little changed groups
t.test(NA_1st$NA_change_rec[NA_1st$globalNA == 2 | NA_1st$globalNA == 4])
sd(NA_1st$NA_change_rec[NA_1st$globalNA == 2 | NA_1st$globalNA == 4])

NA_2nd = PANASdata[PANASdata$T1_NA >= 1.2 & PANASdata$T1_NA < 1.6,]
#t-tests to get CIs around the mean for the little changed groups
t.test(NA_2nd$NA_change_rec[NA_2nd$globalNA == 2 | NA_2nd$globalNA == 4])
sd(NA_2nd$NA_change_rec[NA_2nd$globalNA == 2 | NA_2nd$globalNA == 4])

NA_3rd = PANASdata[PANASdata$T1_NA >= 1.6 & PANASdata$T1_NA < 2.2,]
#t-tests to get CIs around the mean for the little changed groups
t.test(NA_3rd$NA_change_rec[NA_3rd$globalNA == 2 | NA_3rd$globalNA == 4])
sd(NA_3rd$NA_change_rec[NA_3rd$globalNA == 2 | NA_3rd$globalNA == 4])

NA_4th = PANASdata[PANASdata$T1_NA >= 2.2 & PANASdata$T1_NA <= 4.5,]
#t-tests to get CIs around the mean for the little changed groups
t.test(NA_4th$NA_change_rec[NA_4th$globalNA == 2 | NA_4th$globalNA == 4])
sd(NA_4th$NA_change_rec[NA_4th$globalNA == 2 | NA_4th$globalNA == 4])


# now the same but broken down by those who said they felt less positve/negative
# and those who said they felt more positive/negative

#positve affect

t.test(PA_1st$PA_change_rec[PA_1st$globalPA == 2])
t.test(PA_2nd$PA_change_rec[PA_2nd$globalPA == 2])
t.test(PA_3rd$PA_change_rec[PA_3rd$globalPA == 2])
t.test(PA_4th$PA_change_rec[PA_4th$globalPA == 2])
#those who said they felt less positive had higher estimates the higher
# were their baseline scores on PA. 
#to get the Ns for each group
sum(PA_1st$globalPA == 2)
sum(PA_2nd$globalPA == 2)
sum(PA_3rd$globalPA == 2)
sum(PA_4th$globalPA == 2)

t.test(PA_2nd$PA_change_rec[PA_2nd$globalPA == 4])
t.test(PA_1st$PA_change_rec[PA_1st$globalPA == 4])
t.test(PA_3rd$PA_change_rec[PA_3rd$globalPA == 4])
t.test(PA_4th$PA_change_rec[PA_4th$globalPA == 4])
#those who said they felt more positive had estimates in the opposite direction
# when baseline scores were in the higher quartiles whereas in the lower quartiles
# change was in the same direction as their global responses
sum(PA_1st$globalPA == 4)
sum(PA_2nd$globalPA == 4)
sum(PA_3rd$globalPA == 4)
sum(PA_4th$globalPA == 4)

#negative affect

t.test(NA_1st$NA_change_rec[NA_1st$globalNA == 2])
t.test(NA_2nd$NA_change_rec[NA_2nd$globalNA == 2])
t.test(NA_3rd$NA_change_rec[NA_3rd$globalNA == 2])
t.test(NA_4th$NA_change_rec[NA_4th$globalNA == 2])
sum(NA_1st$globalNA == 2)
sum(NA_2nd$globalNA == 2)
sum(NA_3rd$globalNA == 2)
sum(NA_4th$globalNA == 2)

t.test(NA_1st$NA_change_rec[NA_1st$globalNA == 4])
t.test(NA_2nd$NA_change_rec[NA_2nd$globalNA == 4])
t.test(NA_3rd$NA_change_rec[NA_3rd$globalNA == 4])
t.test(NA_4th$NA_change_rec[NA_4th$globalNA == 4])
sum(NA_1st$globalNA == 4)
sum(NA_2nd$globalNA == 4)
sum(NA_3rd$globalNA == 4)
sum(NA_4th$globalNA == 4)








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
