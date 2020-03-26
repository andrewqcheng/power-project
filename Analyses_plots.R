# Data analysis and visualization script for the "Power and Accommodation" project
# (Andrew Cheng and Alice Shen). This script reads files created by "Data_prep.R",
# conducts linear regression analyses, and creates plots using ggplot.

library(lme4) # linear mixed effects regression
library(lmerTest) # test linear models
library(AICcmodavg) # compare linear models
library(stargazer) # output models to LaTeX
library(ggplot2) # make graphs
library(ggpubr) # add stats and pvalues to plots
library(plyr) # data wrangling
library(tidyr) # for gather and spread
library(dplyr) # for group_by

####################################################################################
setwd('C:/Users/andrew/Box Sync/Power and Accommodation/SC_analysis')
full <- read.csv("full_20200319.csv", header = T)
mean_pitch = read.csv("mean_pitch.csv",header=T)
summ.p <- read.csv('summary_personal.csv', header = T)
summ.i <- read.csv('summary_interpersonal.csv', header = T)
change_comparison <- read.csv('change_comparison.csv', header = T)

summ.p$phase <- factor(summ.p$phase, levels=c("pre","interaction","post"))
summ.i$phase <- factor(summ.i$phase, levels=c("pre","interaction","post"))

labs <- c("N"="Low-power Inventor","S"="High-power Investor",
          "L"="Low-power manipulation","F"="High-power manipulation",
          "female"="female participant","male"="male participant",
          "confederate" = "confederate", "subject" = "participant")
comp.labs <- c("conf.change"="Confederate Pre-Post",
               "conf.shortchange"="Confederate Pre-Interview",
               "subj.change"="Participant Pre-Post",
               "subj.shortchange"="Participant Pre-Interview")
pd <- position_dodge(0.1) # move bars and points .1 to the left and right

####################################################################################
# PART 0: MEAN PITCH PER TASK AND MEAN PITCH CHANGE COMPARISON

# Compare task means per speaker
compare_means(mean~phase,data=summ.p[summ.p$speaker=="subject",],method="anova",group.by="Gender") # ns
compare_means(mean~phase,data=summ.p[summ.p$speaker=="confederate",],method="anova",group.by="Gender") # sig
compare_means(mean~phase,data=summ.p[summ.p$speaker=="confederate",]) # pairwise: pre/int & int/post
taskmeanscomp <- list(c("pre","interaction"),c("pre","post"),c("interaction","post"))

compare_means(mean~phase,data=summ.i[summ.i$speaker=="subject",],method="anova",group.by="Gender")
compare_means(mean~phase,data=summ.i[summ.i$speaker=="confederate",],method="anova",group.by="Gender")
compare_means(mean~phase,data=summ.i[summ.i$speaker=="confederate",])

# Plot speaker means per task (split by gender)
means <- full %>% group_by(code,speaker,Gender,phase) %>% summarise(Bark = mean(Bark))
means$phase <- factor(means$phase, levels=c("pre","interaction","post"))

ggplot(means, aes(y=Bark, x=phase)) + 
  geom_boxplot() +
  labs(title="Mean pitch of speaker per task",y="pitch (Bark)") +
  facet_grid(speaker~Gender, labeller=as_labeller(labs), scales="free") +
  theme_minimal() + theme(legend.position="none") +
  stat_compare_means(ref.group="pre", label="p.format") +
  scale_color_brewer(palette="Set1")
ggsave("./pitch-means.pdf",plot=last_plot(),scale=1.5,width=4,height=4,dpi=300)

# Plot speaker means per task (split by gender and power manipulations)
ggplot(summ.p, aes(y=mean, x=phase, color=speaker, group=speaker)) + 
  geom_point(position=pd) + geom_line(position=pd) + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1, position=pd) +
  labs(title="Mean pitch of speaker per task",y="pitch (Bark)") +
  scale_color_brewer(palette="Set1") +
  facet_grid(personal~Gender, labeller=as_labeller(labs), scales="free") +
  coord_cartesian(ylim=c(50,250)) + theme_minimal() + theme(legend.position="bottom")
ggsave("./pitch-personalpower.pdf",plot=last_plot(),scale=1.5,width=3,height=3,dpi=300)

ggplot(summ.i, aes(y=mean, x=phase, color=speaker, group=speaker)) + 
  geom_point(position=pd) + geom_line(position=pd) + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1, position=pd) +
  labs(title="Mean pitch of speaker per task",y="pitch (Bark)") +
  scale_color_brewer(palette="Set1") +
  facet_grid(interpersonal~Gender, labeller=as_labeller(labs), scales="free") +
  coord_cartesian(ylim=c(50,250)) + theme_minimal() + theme(legend.position="bottom")
ggsave("./pitch-interpersonalpower.pdf",plot=last_plot(),scale=1.5,width=3,height=3,dpi=300)

# Compare task differences per speaker. None are significant.
compare_means(confederate_change~personal,data=mean_pitch)
compare_means(confederate_shortchange~personal,data=mean_pitch)
compare_means(subject_change~personal,data=mean_pitch)
compare_means(subject_shortchange~personal,data=mean_pitch)

# Plot speaker change between tasks
ggplot(change_comparison, aes(y=diff,x=comparison,color=speaker)) + geom_boxplot() +
  labs(title="Change in speaker pitch between tasks", y="Pitch difference (Hz)", x="comparison") +
  facet_grid(personal~Gender,labeller=as_labeller(labs)) + 
  theme_minimal() +   geom_hline(yintercept=0, linetype="dashed") +
  scale_color_brewer(palette="Set1",labels=c("confederate","participant")) +
  scale_x_discrete(labels=c("Pre - Post","Pre - Interview")) +
  theme(legend.position="bottom")
ggsave("./pitch-changes-personal.pdf",plot=last_plot(),scale=1.5,width=3,height=3,dpi=300)

ggplot(change_comparison, aes(y=diff,x=comparison,color=speaker)) + geom_boxplot() +
  labs(title="Change in speaker pitch between tasks", y="Pitch difference (Hz)", x="comparison") +
  facet_grid(interpersonal~Gender,labeller=as_labeller(labs)) + 
  theme_minimal() +   geom_hline(yintercept=0, linetype="dashed") +
  scale_color_brewer(palette="Set1",labels=c("confederate","participant")) +
  scale_x_discrete(labels=c("Pre - Post","Pre - Interview")) +
  theme(legend.position="bottom")
ggsave("./pitch-changes-interpersonal.pdf",plot=last_plot(),scale=1.5,width=3,height=3,dpi=300)

# no sig. change in confederate between pre- and post-interview.
summary(lm(confederate_change ~ (1), data=mean_pitch)) 
summary(lm(confederate_change ~ personal, data=mean_pitch)) # barely sig
summary(lm(confederate_change ~ interpersonal, data=mean_pitch)) # not sig
summary(lm(subject_change ~ (1), data=mean_pitch))
summary(lm(subject_change ~ personal, data=mean_pitch)) # not sig
summary(lm(subject_change ~ interpersonal, data=mean_pitch)) # not sig

####################################################################################
# PART 1: LONG-TERM CONVERGENCE

m1 <- lm(convergence ~ (1),data=mean_pitch) # AICc: -69.4729
m2 <- lm(convergence ~ personal, data=mean_pitch) # AICc: -72.50998
m3 <- lm(convergence ~ interpersonal, data=mean_pitch) # AICc: -68.409
m4 <- lm(convergence ~ Gender, data=mean_pitch) # AICc: -72.21124
m5 <- lm(convergence ~ Gender + personal, data=mean_pitch) # AICc: -73.97997  <--- LOWEST
m6 <- lm(convergence ~ personal * Gender, data=mean_pitch) # AICc: -71.50656
AICc(m5) # m5 has the lowest
summary(m5)
stargazer(m5, no.space=TRUE)

ggplot(mean_pitch, aes(y=convergence,x=interpersonal)) + geom_boxplot() +
  labs(title="Interpersonal power and long-term convergence",
       x="Participant as Inve(N)tor or Inve(S)tor",
       y="Pre-task mean pitch diff. - Post-task mean pitch diff.") + theme_minimal() +
  facet_grid(.~Gender) + stat_compare_means(label="p.format")
ggsave("./convergence-interpersonal.pdf",plot=last_plot(),scale=1.5,width=3,height=3,dpi=300)

ggplot(mean_pitch, aes(y=convergence,x=personal)) + geom_boxplot() +
  labs(title="Personal power and long-term convergence",
       x="Participant as Power(F)ul or Power(L)ess",
       y="Pre-task mean pitch diff. - Post-task mean pitch diff.") + theme_minimal() +
  facet_grid(.~Gender) + stat_compare_means(label="p.format")
ggsave("./convergence-personal.pdf",plot=last_plot(),scale=1.5,width=3,height=3,dpi=300)

####################################################################################
# PART 2: SHORT-TERM CONVERGENCE

s1 <- lm(short_convergence ~ (1),data=mean_pitch) # AICc: -49.69299
s2 <- lm(short_convergence ~ personal, data=mean_pitch) # AICc: -47.52808
s3 <- lm(short_convergence ~ interpersonal, data=mean_pitch) # AICc: -48.19684
s4 <- lm(short_convergence ~ Gender, data=mean_pitch) # AICc: -51.72258 <--- LOWEST
s5 <- lm(short_convergence ~ Gender + personal, data=mean_pitch) # AICc: -50.09685
s6 <- lm(short_convergence ~ personal * Gender, data=mean_pitch) # AICc: -47.59012
s7 <- lm(short_convergence ~ Gender * personal * interpersonal, data=mean_pitch) # AICc: -35.97169
s8 <- lm(short_convergence ~ Gender + personal + interpersonal, data=mean_pitch) # AICc: -48.11779
AICc(s4) # s4 has the lowest
summary(s4)
stargazer(s4, no.space=TRUE)

ggplot(mean_pitch, aes(y=short_convergence,x=personal)) + geom_boxplot() +
  labs(title="Personal power and short-term convergence",
       x="Participant as Power(F)ul or Power(L)ess",
       y="Pre-task mean pitch diff. - Interview mean pitch diff.") + theme_minimal() +
  facet_grid(.~Gender) + stat_compare_means(label="p.format")
ggsave("./shortconvergence-personal.pdf",plot=last_plot(),scale=1.5,width=3,height=3,dpi=300)

ggplot(mean_pitch, aes(y=short_convergence,x=interpersonal)) + geom_boxplot() +
  labs(title="Interpersonal power and short-term convergence",
       x="Participant as Inve(N)tor or Inve(S)tor",
       y="Pre-task mean pitch diff. - Interview mean pitch diff.") + theme_minimal() +
  facet_grid(.~Gender) + stat_compare_means(label="p.format")
ggsave("./shortconvergence-interpersonal.pdf",plot=last_plot(),scale=1.5,width=3,height=3,dpi=300)

####################################################################################
# PART 3: WITHIN-INTERVIEW CONVERGENCE

bin <- read.csv(file="mean_f0_bin_20200321.csv",header=T)
bin$code <- as.factor(bin$code)

ggplot(bin, aes(y=Bark,x=group_num,color=speaker)) + geom_point(alpha=0.5) +
  labs(title="Vocal pitch over interview duration",x="time in interview",y="pitch (Bark)") + theme_minimal() +
  facet_grid(personal~Gender,labeller=as_labeller(labs)) + geom_smooth(method="lm") +
  theme(legend.position="bottom") + scale_color_brewer(palette="Set1", labels=c("confederate","participant"))
ggsave("./interviewpitch-personal.pdf",plot=last_plot(),scale=1.5,width=3,height=3,dpi=300)

ggplot(bin, aes(y=Bark,x=group_num,color=speaker)) + geom_point(alpha=0.5) +
  labs(title="Vocal pitch over interview duration",x="time in interview",y="pitch (Bark)") + theme_minimal() +
  facet_grid(interpersonal~Gender,labeller=as_labeller(labs)) + geom_smooth(method="lm") +
  theme(legend.position="bottom") + scale_color_brewer(palette="Set1", labels=c("confederate","participant"))
ggsave("./interviewpitch-interpersonal.pdf",plot=last_plot(),scale=1.5,width=3,height=3,dpi=300)

ggplot(bin, aes(y=Bark,x=group_num,color=speaker,shape=interpersonal)) + geom_point(alpha=0.5) +
  labs(title="Vocal pitch over interview duration (per participant)",x="time in interview",y="pitch (Bark)") + theme_minimal() +
  facet_wrap(Gender~code,ncol=8,scales="free_x") + geom_smooth(method="lm") +
  scale_color_brewer(palette="Set1", labels=c("confederate","participant"))
ggsave("./interviewpitch-allsubj.pdf",plot=last_plot(),scale=1.5,width=6,height=4,dpi=300)

# get differences per bin, keeping only bins in which both confed and subject have values
bin.sub <- subset(bin,select=c("code","speaker","group_num","Bark","Gender","personal","interpersonal","Age"))
bin.diff <- bin.sub %>%
  group_by(code) %>%
  spread(key=speaker,value=Bark) %>%
  drop_na() %>%
  mutate(diff=abs(confederate-subject))

ggplot(bin.diff, aes(y=diff,x=group_num)) + geom_point(alpha=0.5) +
  labs(title="Pitch convergence during interview",x="time in interview (bins)",y="|Confederate mean pitch - Participant mean pitch (Bark)|") +
  theme_minimal() + facet_grid(personal~Gender,labeller=as_labeller(labs)) + 
  geom_smooth(method="lm") + scale_color_brewer(palette="Set2") + theme(legend.position="bottom")
ggsave("./interviewconvergence-personal.pdf",plot=last_plot(),scale=1.5,width=3,height=3,dpi=300)

ggplot(bin.diff, aes(y=diff,x=group_num)) + geom_point(alpha=0.5) +
  labs(title="Pitch convergence during interview",x="time in interview (bins)",y="|Confederate mean pitch - Participant mean pitch (Bark)|") +
  theme_minimal() + facet_grid(interpersonal~Gender,labeller=as_labeller(labs)) + 
  geom_smooth(method="lm") + scale_color_brewer(palette="Set2") + theme(legend.position="bottom")
ggsave("./interviewconvergence-interpersonal.pdf",plot=last_plot(),scale=1.5,width=3,height=3,dpi=300)

d1 <- lm(diff~(1),data=bin.diff) # AICc: 319.1772
d2 <- lm(diff~group_num,data=bin.diff) # AICc: 303.5806 
d3 <- lm(diff~group_num + Gender, data=bin.diff) # AICc: 295.2068 
d4 <- lm(diff~group_num + Gender + personal, data=bin.diff) # AICc: 297.2322
d5 <- lm(diff~group_num + Gender + interpersonal, data=bin.diff) # AICc: 288.4751 <--- BEST
d6 <- lm(diff~group_num * Gender * interpersonal, data=bin.diff) # AICc: 285.4815 <--- LOWEST
d7 <- lm(diff~group_num + Gender + personal + interpersonal, data=bin.diff) # AICc: 290.5054
AICc(d6) # d6 has the lowest, but d5 is better
summary(d6)
stargazer(d6, no.space=TRUE)

####################################################################################
# PART 4: BIG 5 PERSONALITY CORRELATIONS

ggplot(change_comparison, aes(y=convergence,x=Conscientiousness)) + geom_point(color="orange") + 
  geom_smooth(method="lm") + facet_grid(.~Gender) + theme_minimal() + theme(legend.position="none") + 
  labs(title="Long-term convergence and Conscientiousness", y="long-term convergence") +
  stat_cor() #+ stat_regline_equation()
ggsave("./convergence-C.pdf",plot=last_plot(),scale=1.5,width=3,height=3,dpi=300)
ggplot(change_comparison, aes(y=short_convergence,x=Conscientiousness)) + geom_point(color="goldenrod1") + 
  geom_smooth(method="lm") + facet_grid(.~Gender) + theme_minimal() + theme(legend.position="none") +
  labs(title="Short-term convergence and Conscientiousness", y="short-term convergence") +
  stat_cor() #+ stat_regline_equation()
ggsave("./shortconvergence-C.pdf",plot=last_plot(),scale=1.5,width=3,height=3,dpi=300)
ggplot(change_comparison, aes(y=convergence,x=Extraversion)) + geom_point(color="darkgreen") + 
  geom_smooth(method="lm") + facet_grid(.~Gender) + theme_minimal() + theme(legend.position="none") +
  labs(title="Long-term convergence and Extraversion", y="long-term convergence") +
  stat_cor() #+ stat_regline_equation()
ggsave("./convergence-E.pdf",plot=last_plot(),scale=1.5,width=3,height=3,dpi=300)
ggplot(change_comparison, aes(y=short_convergence,x=Extraversion)) + geom_point(color="forestgreen") + 
  geom_smooth(method="lm") + facet_grid(.~Gender) + theme_minimal() + theme(legend.position="none") +
  labs(title="Short-term convergence and Extraversion", y="short-term convergence") +
  stat_cor() #+ stat_regline_equation()
ggsave("./shortconvergence-E.pdf",plot=last_plot(),scale=1.5,width=3,height=3,dpi=300)

stargazer(lm(convergence~Conscientiousness*Gender,data=change_comparison),no.space=TRUE) # sig C, gender, int.
stargazer(lm(convergence~Extraversion*Gender,data=change_comparison),no.space=TRUE) # sig E

stargazer(lm(short_convergence~Conscientiousness*Gender,data=change_comparison),no.space=TRUE) # sig gender, int.
stargazer(lm(short_convergence~Extraversion*Gender,data=change_comparison),no.space=TRUE) # sig gender, int.

# long convergence ~ Big 5 (other)
summary(lm(convergence~Agreeableness*Gender,data=change_comparison)) # not sig
summary(lm(convergence~Neuroticism*Gender,data=change_comparison)) # sig gender
summary(lm(convergence~Openness*Gender,data=change_comparison)) # not sig
# short convergence ~ Big 5 (other)
summary(lm(short_convergence~Agreeableness*Gender,data=change_comparison)) # sig A
summary(lm(short_convergence~Neuroticism*Gender,data=change_comparison)) # not sig
summary(lm(short_convergence~Openness*Gender,data=change_comparison)) # sig O, gender, int.