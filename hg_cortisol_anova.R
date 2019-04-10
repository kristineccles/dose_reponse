##############################################################
# Dose Reponse ANOVAS Cortisol~Mercury
# Written in R Version 3.5.0
##############################################################
# Load Data
# All data
data=read.csv("compiled_cort_fur2.csv")
data$logcort=log10(data$fur_cort)
data$loghg=log10(data$fur_hg)

# Load Libraries
library(lmtest)
library(stats)
library(car)
library(psych)

# plots
library(ggpubr)
library(ggplot2)
library(sjPlot)
##############################################################
# EDA
# Summarize data
describeBy(data, data$Prov)

#Histograms
hist(data$fur_cort)
shapiro.test(data$fur_cort)
hist(data$logcort)
shapiro.test(data$logcort)

hist(data$fur_hg)
shapiro.test(data$fur_hg)
hist(data$loghg)
shapiro.test(data$loghg)

hist(data_site$hi)

#############################################################
#### Biomonitoring Data ####
#############################################################
# Sex Differences
# Subset Data
data_sex = subset(data, sex == "Female" | sex=="Male")

# Test for Variance for Hg
leveneTest(loghg ~ sex, data = data_sex) #homogenous
# Test for Variance for cortisol
leveneTest(logcort ~ sex, data = data_sex) #not homogenous

# T test and box plot for sex differences between Hg
t.test(data_sex$loghg~data_sex$sex)

p1 <- ggplot(data_sex, aes(x=sex, y=loghg)) + 
  geom_boxplot()+
  theme_classic(base_size=16)+
  xlab("Sex")+
  ylab("Log Fur THg (ug/g)")
p1

# T test and box plot for sex differences between cortisol
t.test(data_sex$logcort~data_sex$sex)

p2 <- ggplot(data_sex, aes(x=sex, y=loghg)) + 
  geom_boxplot()+
  theme_classic(base_size=16)+  
  xlab("Sex")+
  ylab("Log Fur Cortisol (pg/mg)")
p2

# NO DIFFERENCE BETWEEN SEXES FOR CORT OR HG

# Make Plots Together
plot1=ggarrange(p1, p2,
                labels = c("A", "B"),
                vjust = 1,
                hjust = -0.5,
                ncol = 2, nrow = 1,
                common.legend = TRUE,
                legend = "right")

#Plot figures with dpi=300
save_plot("sex_boxplot.tif", plot1, width = 30, height = 15, dpi = 300)

###############################################################
#### All data ####
###############################################################
# Test for location differences
boxplot(data$loghg~data$Prov)
anova1=lm(data$loghg~data$Prov)
anova(anova1) # difference between provinces
summary(anova1)

bptest(anova1)# homoscedastic
shapiro.test(resid(anova1)) 
#not normal
hist(resid(anova1))
resettest(anova1)


#### Tukey Follow up Test ####
old.par <- par(mai=c(1.5,2,1,1))
tky1=TukeyHSD(aov(loghg~Prov, data=data))
tky1
plot(tky1, las=1,cex.axis=.7)


p1 <- ggplot(data, aes(x=Prov, y=loghg)) + 
  geom_boxplot()+
  theme_classic(base_size=12)+
  xlab("Province")+
  ylab("Log Fur THg (ug/g)")
p1

# Difference by Province
boxplot(data$logcort~data$Prov)
anova2=lm(data$logcort~data$Prov)
anova(anova2) # difference between provinces
summary(anova2)

tky2=TukeyHSD(aov(logcort~Prov, data=data))
tky2
plot(tky2, las=1,cex.axis=.7)

p2 <- ggplot(data, aes(x=Prov, y=logcort)) + 
  geom_boxplot()+
  theme_classic(base_size=12)+
  xlab("Province")+
  ylab("Log Fur Cortisol (pg/mg)")
p2

# Make Plots Together
plot1=ggarrange(p1, p2,
                labels = c("A", "B"),
                vjust = 1,
                hjust = -0.5,
                ncol = 1, nrow = 2,
                common.legend = TRUE,
                legend = "right")
plot1
#Plot figures with dpi=300
save_plot("prov_boxplot.tif", plot1, width = 30, height = 15, dpi = 300)

