##############################################################
# Dose Reponse Relationship Cortisol~Mercury
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

############################################################
# Test for Relationship between Hg and Cortisol
############################################################
lm1=lm(data$logcort~data$loghg)
summary(lm1)

bptest(lm1) #homosedastic
shapiro.test(resid(lm1)) #not normal
dwtest(lm1) #autocorrelated
resettest(lm1) #not linear


# Plot
p3 = ggplot(data, aes(y=logcort, x=loghg)) + 
  geom_point(color="#4E4E4E") +
  theme_classic()+
  stat_smooth(method = "lm", color="black") +
  xlab("Log Fur Total Hg (ug/g)")+
  ylab("Log Fur Cortisol (pg/mg)")
p3

#Plot figures with dpi=300
save_plot("all_regression.tif", p3, width = 15, height = 15, dpi = 300)

p4=ggplot(data, aes(x=loghg, y=logcort, color=as.factor(hg_fac))) +
  geom_point() + 
  geom_smooth(method=lm)+
  theme_classic()+
  xlab("Log Fur Total Hg (ug/g)")+
  ylab("Log Fur Cortisol (pg/mg)")+
  scale_colour_manual(values=c("#D62F27","#4575B5"),name ="THg Exposure", labels=c("0-15 ug/g", ">15 ug/g"))
p4
#Plot figures with dpi=300
save_plot("all_high_low.tif", p4, width = 15, height = 15, dpi = 300)

#############################################################
# Dichotomous high, low Hg
# subset Data
hg_high = subset(data, hg_fac == "1")
hg_low = subset(data, hg_fac == "0")

# High Hg
lm1=lm(hg_high$logcort~hg_high$loghg)
summary(lm1)

bptest(lm1)
shapiro.test(resid(lm1)) 
dwtest(lm1) #autocorrelation
resettest(lm1)
#Rest of assumptions are met

lm1.1=lm(hg_high$logcort~hg_high$loghg+hg_high$hi)
summary(lm1.1)
#HI is not signifcant

#Low Hg
lm2=lm(hg_low$logcort~hg_low$loghg)
summary(lm2)

bptest(lm2)
shapiro.test(resid(lm2)) #not normal
hist(resid(lm2)) #non-normality is not too bad
dwtest(lm2) #auotocorrelation
resettest(lm2)

lm2.2=lm(hg_low$logcort~hg_low$loghg+hg_low$hi)
summary(lm2.2)

bptest(lm2.2)
shapiro.test(resid(lm2.2)) #not normal
hist(resid(lm2.2)) #non-normality is not too bad
dwtest(lm2.2) #auotocorrelation
resettest(lm2.2)

# Combine plots
# Make Plots Together
plot2=ggarrange(p3, p4,
                labels = c("A", "B"),
                vjust = 1,
                hjust = -0.5,
                ncol = 2, nrow = 1,
                common.legend = TRUE,
                legend = "right")
plot2

save_plot("combined_regression.tif", plot2, width = 30, height = 15, dpi = 300)
