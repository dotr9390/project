dat = read.csv('C:/Users/dontr/Downloads/sex_ad1') 

table(dat$ad_grp)

tapply(dat$attitude, dat$ad_grp, mean) 
tapply(dat$attitude, dat$ad_grp, sd)
tapply(dat$attitude, dat$ad_grp, var)

boxplot(dat$attitude ~ dat$ad_grp, xlab = 'Group ID', ylab = 'Attitude score')

aov.out = aov(dat$attitude ~ dat$ad_grp)
summary(aov.out)
plot(aov.out)

TukeyHSD(aov(dat$attitude~as.factor(dat$ad_grp)))