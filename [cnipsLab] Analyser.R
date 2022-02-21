# ---------*----------*---------*---------*---------*---------*---------*---------#
# ------------------------------------------------------------------------------- #
#    Coglab & Cognitive & Neural Information Processing System Lab - R Tool v3.0  
#
# ------------------------------------------------------------------------------- #
#
#    Edited by Eunhye Choe (Thanks to Prof. Do-joon Yi & Taehoon Kim), 20-02-13
#    Edited by Taehoon Kim, 20-03-20
#
# ------------------------------------------------------------------------------- #
# Date: 2020-03-20
#
# Environment: 
#  - R version 3.6.2 (2019-12-12)
#  - Platform: x86_64-w64-mingw32/x64 (64-bit)
#  - Running under: Windows 10 x64 (build 18362)
#
# Reference 
# 1) Mixed Effect Model
# - Singmann, & Kellen (2017). An Introduction to Mixed Models ~
# - Baayen, Davison, & Bates (2008). Mixed-effects modeling with ~
# - Lo, & Andrew (2015). To transform or not to transform ~
# - Jaeger (2008). Categorical data analysis: Away from ANOVAs ~
# - Mixed Model Tutorial (http://www.bodowinter.com/resources.html -> part one & two)
# - Mixed Model Reanalysis of RT data 
#   (https://cran.r-project.org/web/packages/afex/vignettes/afex_mixed_example.html#overview)
#
# 2) ANOVA, t-test, etc...
# - A Language, not a Letter: Learning Statistics in R (https://ademos.people.uic.edu/index.html)
# - ANOVA and Post-Hoc Contrasts: Reanalysis of Singmann and Klauer (2011)
#   (https://cran.r-project.org/web/packages/afex/vignettes/afex_anova_example.html)
# - Just Enough R 
#   (https://benwhalley.github.io/just-enough-r/contrasts-lmer.html)
#
# 3) power analysis in Linear Mixed Effects Model
# - https://jakewestfall.shinyapps.io/crossedpower/
# 
# Example Data
# - Eye-specific Repetition Priming by Binocular Rivalry (Choe, 2020)
# - Reconsolidataion-based Memory updating (Kim, in prepraration)
# Ask eunhye.choe@yonsei.ac.kr or eunhye.choe@kbri.re.kr / taehoon.kim@yonsei.ac.kr
# ------------------------------------------------------------------------------- #
# ---------*----------*---------*---------*---------*---------*---------*---------#

# -- REQUIRED PACKAGES -- #
# pacman, Rmisc, tidyverse, emmeans, papaja, knitr, psych, dplyr, afex, BayesFactor,
# car, ggplot2, lme4, lmerTest, simr, parallel, psycho, ggpubr, optimx, tibble, cowplot
# sjPlot, effects, readr, pwr2.
# -- Download packages before start -- #

# ===== CONTENTS ================================================================ #
#
# 1. load data
#
# 2. Accuracy Analysis
#   2.1. Descriptive Statistical Analysis
#   2.2. Plot - Acc
#   2.3. Inferential Statistical Analysis
#     2.3.1. t-Test
#     2.3.2. ANOVA
#     2.3.3. Correlation
#     2.3.5. LMM
#     2.3.6. LMM with Afex
#     2.3.7. GLMM
#     2.3.8. GLMM with Afex
# 
# 3. Response Time Analysis
#   3.1. Descriptive Statistical Analysis
#   3.2. Plot - RT
#   3.3. Inferential Statistical Analysis
#     3.3.1. t-Test
#     3.3.2. ANOVA
#     3.3.3. Correlation
#     3.3.4. LMM
#     3.3.5. LMM with Afex
#     3.3.6. GLMM
#     3.3.7. GLMM with Afex
#     3.3.8. BayesFactor
#
# 4. Calculate Power & Sample Size
#   4.1. for ANOVA
#   4.2. for Mixed Models
# 
# Appendix 1. ggplot Guide
#   1-1. One-way Plot
#   1-2. Two-way Plot (2 X 2)
#
# =============================================================================== #

# get ready
rm(list=ls())
getwd()
setwd("C:/Users/sorel/Dropbox/Research/YS공유/Analyser")
set.seed(4228) # for replication
options(scipen=4)

# load packages 
# Some packages need to be loaded. We use `pacman` as a package manager, which takes care of the other packages. 
# install.packages("devtools")
# devtools::install_github("crsh/papaja")
if (!require("pacman", quietly = TRUE)) install.packages("pacman")
if (!require("Rmisc", quietly = TRUE)) install.packages("Rmisc") # Never load it directly.
pacman::p_load(tidyverse, emmeans, papaja, knitr, psych, dplyr, afex, BayesFactor,
               car, ggplot2, lme4, lmerTest, simr, parallel, psycho, ggpubr, optimx, tibble, cowplot,
               sjPlot, effects, readr, pwr2)
# pacman::p_load_gh("thomasp85/patchwork", "RLesur/klippy")
devtools::install_github("thomasp85/patchwork")
devtools::install_github("RLesur/klippy")


options(knitr.kable.NA = '') # hide NA with knitr function
klippy::klippy()


# ---------------------------------------------------------- #
# 1. load data ####
# ---------------------------------------------------------- #

# Example Data
urlfile = "https://raw.githubusercontent.com/sorellrno-th/analysis/master/example_p.csv"
p1 <- read_csv(url(urlfile))
# p1 <- read_csv("example_p", header=T)

# you can upload & read your data by
# DATA_NAME <- read.csv
# *** change DATA_NAME with your own data name ***
# [Ctrl + F] > [Find all p1] > [Replace DATA_NAME]
# In this example, replace p1 -> p1

glimpse(p1, width = 70)

# ------- Variables ------- #
# SN: subject number, 16
# Btw: Group. 1 = Experimental, 2 = Control
# Trial: 320 trials per subject
# Block: total 4 blocks
# bTrial: 80 trials per block
# cCue: color cue condition, 1-red, 2-blue, 3-green, 4-yellow
# IMidx: object image index
# IMname: name of target object image
# CueName: name of color cue image
# Resp: Association cue response, 1-red, 2-blue, 3-green, 4-yellow
# RT: reaction time in ms
# Corr: correctness for the response, 0 = incorrect, 1 = correct, 7 = no response

table(p1$SN)

# check number of trials for each condition/SN
table(p1$Block, p1$SN) 
table(p1$cCue, p1$SN) 
table(p1$Btw, p1$SN) 
table(p1$Btw, p1$Block, p1$SN) 

# change class of main factors: double to factor
p1$SN = factor(p1$SN); p1$Btw = factor(p1$Btw, labels=c("exp","con")) # for lmer/glmer
p1$Block = factor(p1$Block, levels=c(1,2,3,4), labels=c("b1","b2","b3","b4")) # for lmer/glmer

p1$Btw = factor(p1$Btw, levels=c(2,1), labels=c("con","exp")) # for mixed()
p1$Block = factor(p1$Block, levels=c(2,3,4,1), labels=c("b2","b3","b4","b1")) # for mixed()

p1$cCue = factor(p1$cCue, levels=c(1,2,3,4), labels=c("c1","c2","c3", "c4")) 
p1$CueName = factor(p1$CueName, labels=c("c1","c2","c3", "c4"))
p1$RT <- p1$RT*1000; p1$Corr <- as.numeric(p1$Corr==1)

headTail(p1)

# remove bad participants
# to remove participants number 3, use the code below
# p1 <- p1[which(p1$SN!=3),] 
# p1 <- p1 %>% filter(SN !=3)

# ---------------------------------------------------------- #
# 2. Accuracy Analysis ####
# ---------------------------------------------------------- #

# ---------------------------------------------------------- #
# 2.1. Descriptive Statistical Analysis - Acc ####
# ---------------------------------------------------------- #

# subject-level, long format (SN/Btw/Block)
p1accL <- p1 %>% group_by(SN, Btw, Block) %>%
  dplyr::summarise(Acc = mean(Corr)*100) %>%
  ungroup()
p1accL %>% kable(digits=2)

# subject-level, wide format (SN/Btw/Block)
p1accW <- p1accL %>% spread(key=Block, value = Acc)
p1accW %>% kable(digits=2)

# summary table: grand mean (eyerep/locrep)
p1accG <- p1accL %>% group_by(Btw, Block) %>%
  summarise(Acc.M = mean(Acc), Acc.SD = sd(Acc)) %>%
  ungroup()
p1accG$Acc.se <- Rmisc::summarySEwithin(data = p1accL, measurevar = "Acc", 
                                        idvar = "SN", betweenvars = "Btw", withinvars = "Block")$se
p1accG$Acc.ci <- Rmisc::summarySEwithin(data = p1accL, measurevar = "Acc", 
                                        idvar = "SN", betweenvars = "Btw", withinvars = "Block")$ci
p1accG <- p1accG %>% 
  mutate(lower.ci = Acc.M-Acc.ci,
         upper.ci = Acc.M+Acc.ci)
# for between-subject design. check help(summarySE)
p1accG %>% kable(digits=2)

# marginal means of group conditions
p1accL %>% group_by(Btw) %>%
  summarise(M = mean(Acc), SD = sd(Acc)) %>%
  ungroup() %>% kable(digits=2)

# marginal means of block conditions
p1accL %>% group_by(Block) %>%
  summarise(M = mean(Acc), SD = sd(Acc)) %>%
  ungroup() %>% kable(digits=2)

# ----------------------------------------------------------#
# 2.2. Plot - Acc ####
# ----------------------------------------------------------#

# simple plot
apa_barplot(data=as.data.frame(p1accL), 
            id="SN", dv="Acc", 
            ylab = "Accuracy (%)", xlab = "Btw", 
            main = c("Accuracy"),  
            # dispersion =  within_subjects_conf_int, # w/n confidence interval
            factors=c("Btw", "Block"), 
            ylim = c(0, 105),
            las=1)

apa_beeplot(data=as.data.frame(p1accL), 
            id="SN", dv="Acc", 
            ylab = "Accuracy (%)", xlab = "Btw", 
            main = c("Accuracy"),  
            # dispersion =  within_subjects_conf_int, # w/n confidence interval
            factors=c("Btw", "Block"), 
            ylim = c(0, 105),
            las=1)

# Accuracy by participant
ggplot(data=p1accL, aes(x = SN, y= Acc)) + 
  ggtitle("참가자별 평균 정확도") +
  xlab("참가자") + ylab("Accuracy") + theme_bw() +
  theme(text=element_text(size=14)) +
  theme(legend.title = element_text(face = 1,size = 15)) +
  geom_boxplot()

# ggplot 1
plot1 <- ggplot(data=p1accL, aes(x=Block, y=Acc, fill=Btw)) +
  stat_summary(fun.y = mean, geom = "bar", position="dodge", na.rm = TRUE, alpha = .9, 
               width = 0.8, colour="black", size = 1.02, show.legend = FALSE) +
  geom_pointrange(data=p1accG, aes(x = Block, y=Acc.M, ymin = lower.ci, ymax = upper.ci),
                  position = position_dodge(0.80), color = "darkred", size = 1, show.legend = FALSE) +
  facet_grid(.~Btw, scales="free_x", space = "free",
             labeller = labeller(Btw = c("exp" = "Experimental Group","con" = "Control Group"))) +
  geom_point(position=position_dodge(0.5), color="gray80", size=2, show.legend = FALSE) +
  geom_segment(data=filter(p1accW, Btw == "exp"), inherit.aes = FALSE,
               aes(x=1, y=filter(p1accW, Btw == "exp")$b1,
                   xend=2, yend=filter(p1accW, Btw == "exp")$b2),
               color="gray80") +
  geom_segment(data=filter(p1accW, Btw == "exp"), inherit.aes = FALSE,
               aes(x=2, y=filter(p1accW, Btw == "exp")$b2,
                   xend=3, yend=filter(p1accW, Btw == "exp")$b3),
               color="gray80") +
  geom_segment(data=filter(p1accW, Btw == "exp"), inherit.aes = FALSE,
               aes(x=3, y=filter(p1accW, Btw == "exp")$b3,
                   xend=4, yend=filter(p1accW, Btw == "exp")$b4),
               color="gray80") +
  geom_segment(data=filter(p1accW, Btw == "con"), inherit.aes = FALSE,
               aes(x=1, y=filter(p1accW, Btw == "con")$b1,
                   xend=2, yend=filter(p1accW, Btw == "con")$b2),
               color="gray80") +
  geom_segment(data=filter(p1accW, Btw == "con"), inherit.aes = FALSE,
               aes(x=2, y=filter(p1accW, Btw == "con")$b2,
                   xend=3, yend=filter(p1accW, Btw == "con")$b3),
               color="gray80") +
  geom_segment(data=filter(p1accW, Btw == "con"), inherit.aes = FALSE,
               aes(x=3, y=filter(p1accW, Btw == "con")$b3,
                   xend=4, yend=filter(p1accW, Btw == "con")$b4),
               color="gray80") +
  scale_x_discrete(labels=c("1","2","3","4")) +
  scale_fill_manual(values = c("#feb24c", "#91bfdb"),
                    labels = c("Experimental", "Control")) +
  coord_cartesian(ylim = c(0, 105), clip = "on") +
  labs(x = "Block", y = "Accuracy (%)", fill ="Group") +
  # ggtitle("Association Memory Accuracy") +
  theme_bw(base_size = 15) +
  theme(axis.title = element_text(face = "bold", size = 16, color = "black"),
        axis.text = element_text(face = "plain", hjust = 0.5, size = 15, color = "black"),
        axis.line=element_line(),
        strip.text.x = element_text(face = "plain", size = 15, color = "black"),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.spacing=unit(1, "lines"),
        # aspect.ratio = 0.2,
        plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"), 
        # plot.title = element_text(hjust = 0.5),
        legend.position=c(0.8, 0.85))
plot1

# ggplot 2
plot2 <- ggplot(p1accG, mapping=aes(x=Block, y=Acc.M, group=Btw)) + 
  geom_ribbon(p1accG, mapping=aes(x=Block, ymin=lower.ci, ymax=upper.ci, fill=Btw), alpha=0.5) + 
  geom_line(p1accG, mapping=aes(x=Block, y=Acc.M, color=Btw), size = 1, show.legend = FALSE) +
  geom_pointrange(aes(x = Block, ymin=lower.ci, ymax=upper.ci, color=Btw), position = position_dodge(0), 
                  size = 0.8, show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue"), labels = c("Experimental", "Control")) +
  scale_color_manual(values = c("darkred", "darkblue")) +
  coord_cartesian(ylim = c(25, 105), clip = "on") +
  labs(x = "Block", y = "Accuracy (%)", fill="Group") +
  scale_x_discrete(labels = c("1", "2", "3", "4")) +
  # ggtitle("A) All Group") +
  theme_bw(base_size = 15) +
  theme(axis.title = element_text(face = "bold", size = 16, color = "black"),
        axis.text = element_text(face = "plain", hjust = 0.5, size = 15, color = "black"),
        axis.line=element_line(),
        aspect.ratio = 1,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border=element_blank(),
        plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
        legend.position = c(0.65, 0.35))

# ggplot 3
p1accG$Grp <- factor(p1accG$Btw, labels=c("Experimental", "Control"))
plot3 <- ggplot(data=p1accG, aes(x=Block, y=Acc.M, ymin=lower.ci, ymax=upper.ci, color=Grp, shape=Grp)) +
  geom_point(size = 4, position = position_dodge(.3)) +
  geom_errorbar(width = .2, position = position_dodge(.3)) +
  geom_line(aes(group = Grp), position = position_dodge(.3)) + 
  scale_color_manual(values = c("red", "black")) +
  coord_cartesian(ylim = c(20,100), clip = "on") +
  # scale_y_continuous(breaks = seq(0,80)) +
  scale_x_discrete(labels = c("1", "2", "3", "4")) +
  labs(x = "Block", y = "Accuracy (%)") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.65, 0.35),
        legend.title = element_blank(),
        aspect.ratio = 1,
        legend.background = element_blank(),
        plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
        legend.key = element_blank())

ggarrange(plot1, ggarrange(plot2, plot3, ncol = 2, labels=c("B", "C"), 
                           hjust = -0.05, vjust=1, font.label = list(size = 20)), labels=c("A"), 
          hjust = -0.05, vjust=1, font.label = list(size = 20), ncol=1, nrow=2)
cowplot::plot_grid(plot2, plot3, ncol = 2, 
          labels = c('B', 'C'), label_size = 20)


# *** SEE Appendix to adjust things in ggplot ***

# ---------------------------------------------------------- #
# 2.3. Inferential Statistical Analysis - Acc ####
# ---------------------------------------------------------- #

# ---------------- #
# 2.3.1. t-Test ####

# 2.3.1.1. independent samples t-test
p1.acc.ttest1 <- t.test(Acc ~ Btw, data = p1accL,
                        alternative = c("two.sided"),
                        paired=F, var.equal=F,
                        conf.level=.95)
p1.acc.ttest1

p1accL.exp <- p1accL %>% filter(Btw=="exp")
p1accL.con <- p1accL %>% filter(Btw=="con")
p1.acc.ttest2 <- t.test(p1accL.exp$Acc, p1accL.con$Acc,
       alternative = c("two.sided"),
       paired=F, var.equal=F,
       conf.level=.95)
p1.acc.ttest2

p1.acc.ttest3 <- compare.2.vectors(p1accL.exp$Acc, p1accL.con$Acc, 
                  paired=F, tests=c("parametric"), coin=T,
                  alternative = "two.sided") # afex pacakage
p1.acc.ttest3

# effect size 
lsr::cohensD(Acc ~ Btw,
             data = p1accL)

# 2.3.1.2. paired samples t-test
p1accL.b12 <- p1accL %>% filter(Block==c("b1","b2"))
p1.acc.ttest4 <- t.test(Acc ~ Block, data=p1accL.b12,
                        alternative = c("two.sided"),
                        paired=T, var.equal=F,
                        conf.level=.95)
p1.acc.ttest4


p1accL.b1 <- p1accL %>% filter(Block=="b1")
p1accL.b2 <- p1accL %>% filter(Block=="b2")
p1.acc.ttest5 <- t.test(p1accL.b1$Acc, p1accL.b2$Acc,
                        alternative = c("two.sided"),
                        paired=T, var.equal=F,
                        conf.level=.95)
p1.acc.ttest5

p1.acc.ttest6 <- compare.2.vectors(p1accL.b1$Acc, p1accL.b2$Acc, 
                                   paired=T, tests=c("parametric"), coin=T,
                                   alternative = "two.sided") # afex pacakage
p1.acc.ttest6

# effect size 
p1accL.b12$Block <-factor(p1accL.b12$Block, labels=c("b1","b2"))
lsr::cohensD(Acc ~ Block,
             data = p1accL.b12)

# --------------- #
# 2.3.2. ANOVA ####

# check contrast coding scheme
set_sum_contrasts()
set_treatment_contrasts()

contrasts(p1accL$Block)
contrasts(p1accL$Btw)

# 2.3.2.1. ANOVA
p1.acc.aov1 <- aov_ez(id="SN", dv = "Acc", data = p1accL, between = "Btw")
test_levene(p1.acc.aov1)
summary(p1.acc.aov1)
anova(p1.acc.aov1)
anova(p1.acc.aov1, es = "pes")
nice(p1.acc.aov1, es = "pes")

# 2.3.2.2. RM ANOVA
p1.acc.aov2 <- aov_ez(id="SN", dv = "Acc", data = p1accL, within = "Block")
test_sphericity(p1.acc.aov2)
summary(p1.acc.aov2)
anova(p1.acc.aov2)
anova(p1.acc.aov2, es = "pes")
nice(p1.acc.aov2, es="pes")

# 2.3.2.3. Mixed Factorial ANOVA
p1.acc.aov3 <- aov_ez(id="SN", dv = "Acc", data = p1accL, between = c("Btw"), within = c("Block"))
test_levene(p1.acc.aov3)
test_sphericity(p1.acc.aov3)
summary(p1.acc.aov3)
anova(p1.acc.aov3)
anova(p1.acc.aov3, es = "pes") %>% kable(digits=3)
nice(p1.acc.aov3, es="pes") 
# pes = partial eta-squared

# simple plot for aov_ez
afex_plot(p1.acc.aov3, x = c("Block"), 
          trace = "Btw", 
          error = "none")

# 2.3.2.4. post-hoc
p1.acc.m1 <- emmeans(p1.acc.aov3, pairwise ~ Block | Btw, type = "response") # adjust="bon"
p1.acc.m1$contrasts %>% kable(digits=2)

p1.acc.m2 <- emmeans(p1.acc.aov3, "Block", by = "Btw", type = "response")
p1.acc.m2.pair <- update(pairs(p1.acc.m2), by = NULL, adjust = "tukey")
p1.acc.m2.pair %>% summary(infer = TRUE) %>% kable(digits=2)

# main effect only
p1.acc.m3  <- emmeans(p1.acc.aov3, ~ Block)
pairs(p1.acc.m3)
p1.acc.m3.pair<- update(pairs(p1.acc.m3), by = NULL, adjust = "tukey")
p1.acc.m3.pair

summary(as.glht(pairs(p1.acc.m3)), test=multcomp::adjusted("free"))

# simple interaction only
p1.acc.m4  <- emmeans(p1.acc.aov3, ~ Block | Btw)
# equal: emmeans(p1.acc.aov3, "Block", by = "Btw")
pairs(p1.acc.m4)

p1.acc.m5  <- emmeans(p1.acc.aov3, ~ Block*Btw)
# equal: emmeans(p1.acc.aov3, c("Block", "Btw"))
pairs(p1.acc.m5)

# --------------------- #
# 2.3.3. Correlation ####

p1accL.cor <- p1accL %>% filter(Block==c("b1", "b2")) %>% spread(key = "Block", value = Acc)
cor.test(formula = ~ b1 + b2, data = p1accL.cor,
         method = "pearson", alternative = "two.sided")

ggplot(p1accL.cor, aes(x=b1, y=b2)) +
  geom_point(size = 4) +
  geom_smooth(method=lm) +
  labs(x = "Block 1 Acc", 
       y = "Block 2 Acc") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        aspect.ratio = 1)

# --------------------- #
# 2.3.4. Regression ####

# 2.3.4.1. Linear regression
set_treatment_contrasts()
set_sum_contrasts()

glimpse(p1accL, width = 70)
contrasts(p1accL$Btw)
#     con
# exp   0  <- dummy code. reference group은 exp
# con   1

contrasts(p1accL$Block)
#    b2 b3 b4
# b1  0  0  0 <- reference는 b1
# b2  1  0  0
# b3  0  1  0
# b4  0  0  1

p1accL.lm1 <- lm(Acc ~ Block*Btw, data=p1accL)
summary(p1accL.lm1)
anova(p1accL.lm1)
coef(p1accL.lm1)
# b2 = 0; b3 = 0; b4= 1
# 49.37500 + 33.82813*b2 + 44.6875*b3 + 48.4375*b4

ggplot(p1accL, aes(x=Block, y=Acc)) +
  geom_point(size = 4) +
  geom_smooth(method="lm", data=p1accL, aes(x=Block, y=Acc)) +
  labs(x = "Block", 
       y = "Acc") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        aspect.ratio = 1)

layout(matrix(c(1,2,3,4),2,2)) 
plot(p1accL.lm1)
outlierTest(p1accL.lm1)

hist(MASS::studres(p1accL.lm1))

hist(resid(p1accL.lm1))
plot(MASS::studres(p1accL.lm1))
qqnorm(MASS::studres(p1accL.lm1))

# 2.3.4.2. Logistic regression
p1.glm <- p1
glimpse(p1.glm, width=70)

p1.glm$Block <- as.numeric(p1.glm$Block)
p1.glm <- (plyr::arrange(p1.glm, Block))
p1.glm1 <- glm(Corr ~ Block,data=p1.glm, family=binomial()) 
summary(p1.glm1) # display results
anova(p1.glm1)

coef(p1.glm1)
confint(p1.glm1)
exp(coef(p1.glm1))
exp(confint(p1.glm1))
predict(p1.glm1, type="response")
hist(residuals(p1.glm1, type="deviance"))
qqnorm(residuals(p1.glm1, type="deviance"))

# plotting
p1.glm1$coef

newdat <- data.frame(Block=seq(min(p1.glm$Block), max(p1.glm$Block),len=5120))
newdat$Corr = predict(p1.glm1, newdata=p1.glm, type="response")

ggplot(p1.glm, aes(x=Block, y=Corr)) +
  ggbeeswarm::geom_quasirandom(dodge.width = 1, color = "blue", size = 1, alpha = 0.2,
                               show.legend = FALSE) +
  geom_smooth(data= newdat, mapping = aes(x=Block, y=Corr), 
              method = "glm", 
              method.args = list(family = "binomial"), 
              se = T) 

# ------------------------------- #
# 2.3.5. Linear Mixed Modeling ####

# 2.3.5.1. testing fixed effect with LMM
p1.acc.lmer.m1 <- lmer(Acc ~ Block + (1|SN), p1accL)
p1.acc.lmer.m2 <- lmer(Acc ~ Block+Btw + (1|SN), p1accL)
p1.acc.lmer.m3 <- lmer(Acc ~ Block*Btw + (1|SN), p1accL) 
anova(p1.acc.lmer.m1, p1.acc.lmer.m2, p1.acc.lmer.m3)
anova(p1.acc.lmer.m1, p1.acc.lmer.m2)
anova(p1.acc.lmer.m2, p1.acc.lmer.m3)

# 2.3.5.2. fitting LMM
p1.acc.lmer <- lmer(Acc ~ Block*Btw + (1|SN), p1accL)
Anova(p1.acc.lmer, test.statistic="Chisq")
Anova(p1.acc.lmer, test.statistic="F")
anova(p1.acc.lmer)
summary(p1.acc.lmer)

# 2.3.5.3. post-hoc
p1.acc.m6 <- emmeans(p1.acc.lmer, pairwise ~ Block | Btw, type = "response") # adjust="bon"
p1.acc.m6$contrasts %>% kable(digits=2)

# 2.3.5.4. assumption check
# 1) Linearity & Homoskedasticity(equal variance)
plot(fitted(p1.acc.lmer),residuals(p1.acc.lmer)) + 
  abline(h=0, col="red", lwd=1, lty=2)
plot(residuals(p1.acc.lmer))

# 2) Normality of residuals
hist(residuals(p1.acc.lmer))
qqnorm(residuals(p1.acc.lmer)) + 
  qqline(residuals(p1.acc.lmer), col='2')

# 2.3.5.5. coefficient - random intercept
coef(p1.acc.lmer) # 추정된 참가자별 Intercept (개인차)
ranef(p1.acc.lmer) # 추정된 참가자별 변화량 (grandMean - coefficient)
coef(p1.acc.lmer)$SN$`(Intercept)` - ranef(p1.acc.lmer)$SN$`(Intercept)`

# 2.3.5.6. LRT(likelihood ratio test) on random effects
lmerTest::rand(p1.acc.lmer)

# -------------------------------------------------- #
# 2.3.6. Linear Mixed Modeling with Afex packages ####
load("p1acc_lmer_mixed.Rdata")

# 2.3.6.1. fitting LMM - S method
# (nc <- detectCores())
# cl <- makeCluster(rep("localhost", nc))
# p1.acc.lmixed <- afex::mixed(Acc ~ Btw*Block + (1|SN), 
#                         data = p1accL,
#                         method = "S", cl = cl,
#                         control = lmerControl(optCtrl = list(maxfun = 1e6)))

# nested model을 비교하는 method (Singmann, & Kellen, 2017)
# 1) Kenward-Roger approximation : method ="KR"
# 2) Satterthwaite approximation : method = "S"
# 3) Likelihood Ratio Test : method = "LRT"
# 4) Parametric bootstrapping : method = "PB"

# save(p1.acc.lmixed, file="p1acc_lmer_mixed.Rdata")
# stopCluster(cl)

summary(p1.acc.lmixed)
anova(p1.acc.lmixed)
nice(p1.acc.lmixed)

# 2.3.6.2. post-hoc
p1.acc.m7 <- emmeans(p1.acc.lmixed, pairwise ~ Block | Btw, type = "response") # adjust="bon"
p1.acc.m7$contrasts %>% kable(digits=3)

p1.acc.m7 <- emmeans(p1.acc.lmixed, "Block", by="Btw", type = "response") # adjust="bon"
p1.acc.m7.pair <- update(pairs(p1.acc.m7), by = NULL, adjust = "holm")
p1.acc.m7.pair %>% summary(infer = T) %>% kable(digits=3)

# 2.3.6.3. assumption check
# 1) Linearity & Homoskedasticity(equal variance)
m1 <- p1.acc.lmixed.m1$full_model
plot(fitted(m1), residuals(m1))+
  abline(h=0, col="red", lwd=1, lty=2)
plot(residuals(m1))

# 2) Normality of residuals
hist(residuals(m1))
qqnorm(residuals(m1)) + 
  qqline(residuals(m1), col='2')
describe(residuals(m1)) # check skewness / kurtosis

# 2.3.6.4. coefficient - random intercept
coef(m1) # 추정된 참가자별 Intercept (개인차)
ranef(m1) # 추정된 참가자별 변화량 (grandMean - coefficient)
coef(m1)$SN$`(Intercept)` - ranef(m1)$SN$`(Intercept)`

# ------------------------------------------- #
# 2.3.7. Generalized Linear Mixed Modeling ####
load("p1acc_glmer.Rdata")


# 2.3.7.1 testing fixed effect with GLMM
# p1.acc.glmer.m1 <- glmer(Corr ~ Block + (1|SN) + (1|IMname), p1,
#                       family = binomial(link="logit")) # binomial for accuracy data
# p1.acc.glmer.m2 <- glmer(Corr ~ Block+Btw + (1|SN) + (1|IMname), p1,
#                          family = binomial(link="logit")) # binomial for accuracy data
# p1.acc.glmer.m3 <- glmer(Corr ~ Block*Btw + (1|SN) + (1|IMname), p1,
#                          family = binomial(link="logit"), 
#                          glmerControl(optimizer = c("bobyqa", "Nelder_Mead"), 
#                                       optCtrl = list(maxfun = 1e7))) # binomial for accuracy data
anova(p1.acc.glmer.m1, p1.acc.glmer.m2, p1.acc.glmer.m3)
anova(p1.acc.glmer.m1, p1.acc.glmer.m2)
anova(p1.acc.glmer.m2, p1.acc.glmer.m3)

# 2.3.7.2 fitting GLMM
p1.acc.glmer <- glmer(Corr ~ Block*Btw + (Block|SN) + (1|IMname), p1,
                      family = binomial(link="logit"), 
                      glmerControl(optimizer = c("bobyqa"), 
                                   optCtrl = list(maxfun = 1e7))) # binomial for accuracy data
# 만약 singular fit이 나타난다면, 무선 효과 구조를 축소한다. 
# p1.acc.glmer.reduced <- glmer(Corr ~ Block*Btw + (Block||SN) + (1|IMname), p1,
#                       family = binomial(link="logit"), 
#                       glmerControl(optimizer = c("bobyqa"), 
#                                    optCtrl = list(maxfun = 1e7))) # binomial for accuracy data

# p1.acc.glmer.reduced1 <- glmer(Corr ~ Block*Btw + (1|SN) + (1|IMname), p1,
#                               family = binomial(link="logit"), 
#                               glmerControl(optimizer = c("bobyqa"), 
#                                            optCtrl = list(maxfun = 1e7))) # binomial for accuracy data

# save(p1.acc.glmer.m1, p1.acc.glmer.reduced, p1.acc.glmer.reduced1, 
#      p1.acc.glmer.m2, p1.acc.glmer.m3, p1.acc.glmer, file = "p1acc_glmer.Rdata")

Anova(p1.acc.glmer.reduced1, test.statistic="Chisq")
anova(p1.acc.glmer.reduced, p1.acc.glmer.reduced1)
anova(p1.acc.glmer.reduced1)
summary(p1.acc.glmer.reduced1)

# 2.3.7.3 post-hoc
p1.acc.m7 <- emmeans(p1.acc.glmer.reduced1, pairwise ~ Block | Btw, type = "response") # adjust="bon"
p1.acc.m7$contrasts %>% kable(digits=2)

# 2.3.7.4 assumption check
# 1) Linearity & Homoskedasticity(equal variance)
plot(fitted(p1.acc.glmer.reduced1), residuals(p1.acc.glmer.reduced1))+
  abline(h=0, col="red", lwd=1, lty=2)
plot(residuals(p1.acc.glmer.reduced1))

# 2) Normality of residuals
hist(residuals(p1.acc.glmer.reduced1))
qqnorm(residuals(p1.acc.glmer.reduced1)) + 
  qqline(residuals(p1.acc.glmer.reduced1), col='2')

describe(residuals(p1.acc.glmer)) # check skewness / kurtosis

# 2.3.7.5 coefficient - random intercept
coef(p1.acc.glmer.reduced1) # 추정된 참가자별 Intercept (개인차)
ranef(p1.acc.glmer.reduced1) # 추정된 참가자별 변화량 (grandMean - coefficient)
coef(p1.acc.glmer)$SN$`(Intercept)` - ranef(p1.acc.glmer)$SN$`(Intercept)`

# 2.3.7.6 Best Linear Unbias Predictions (BLUPs)
ranef(p1.acc.glmer)$SN
AA <- ranef(p1.acc.glmer)$SN
colnames(AA) <- c("intercept","Block2","Block3","Block4")
A1 <- AA %>% rownames_to_column() %>% 
  select(rowname, intercept) %>% 
  column_to_rownames(var = "rowname")
A2 <- AA %>% rownames_to_column() %>% 
  select(rowname, Block3) %>% 
  column_to_rownames(var = "rowname")
BLUPs <- cbind(A1, A2)

cor.test(formula = ~ intercept + Block3, data = BLUPs,
         method = "pearson", alternative = "two.sided")
BLUPs %>% kable(digits=3)

B.P <- ggplot(BLUPs, aes(x=intercept, y=Block3)) +
  geom_point(size = 4) +
  geom_smooth(method=lm) +
  labs(x = expression(Delta*" b1 to b2"), 
       y = expression(Delta*" b1 to b3")) +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        aspect.ratio = 1)
B.P

# ---------------------- #
# GLMM visualization 1 (not recommended)
sjPlot::plot_model(p1.acc.glmer, type = c("int"), mdrt.values="all")

# --------------------- #
# GLMM visualization 2
# extract effects
library(effects) # download
model_predicted_effects <- as.data.frame(effects::effect("Block * Btw", p1.acc.glmer))
ggplot(data = model_predicted_effects,
       aes(x = Btw, y = fit, colour = Block)) +
  geom_pointrange(aes(ymax = upper,
                      ymin = lower),
                  position = position_dodge(width = 1)) +
  # geom_line(aes(x = Block, y = fit, group = Btw),
            # position = position_dodge(width = 1)) +
  ylab("RT") +
  xlab("Block") +
  scale_colour_grey() +
  theme_classic() +
  theme(legend.justification=c(1,1), legend.position=c(1,1))
# these are estimated/predicted plots
# you can check exact numbers with model_predicted_effects


# -------------------------------------------------------------- #
# 2.3.8. Generalized Linear Mixed Modeling with Afex packages ####
load("p1acc_glmer_mixed.Rdata")

# 2.3.8.1. fitting LMM - S method
# (nc <- detectCores())
# cl <- makeCluster(rep("localhost", nc))
# p1.acc.glmixed <- afex::mixed(Corr ~ Btw*Block + (Block|SN) + (1|IMname),
#                              data = p1,
#                              family = binomial(link="logit"),
#                              method = "LRT", cl = cl,
#                              expand_re = F,
#                              control = lmerControl(optimizer = "bobyqa",
#                                                    optCtrl = list(maxfun = 1e7)))

# nested model을 비교하는 method (Singmann, & Kellen, 2017)
# 1) Kenward-Roger approximation : method ="KR"
# 2) Satterthwaite approximation : method = "S"
# 3) Likelihood Ratio Test : method = "LRT"
# 4) Parametric bootstrapping : method = "PB"

# save(p1.acc.glmixed, file="p1acc_glmer_mixed.Rdata")
# stopCluster(cl)

summary(p1.acc.glmixed)
anova(p1.acc.glmixed)
nice(p1.acc.glmixed)

# 2.3.8.2. post-hoc
p1.acc.m8 <- emmeans(p1.acc.glmixed, pairwise ~ Block | Btw, type = "response") # adjust="bon"
p1.acc.m8$contrasts %>% kable(digits=3)
plot(p1.acc.m8, horizontal = FALSE, comparisons = T)

p1.acc.m9 <- emmeans(p1.acc.glmixed, pairwise ~ Btw | Block, type = "response") # adjust="bon"
p1.acc.m9$contrasts %>% kable(digits=3)
plot(p1.acc.m9, horizontal = FALSE, comparisons = T)

p1.acc.m8 <- emmeans(p1.acc.glmixed, "Block", by="Btw", type = "response") # adjust="bon"
p1.acc.m8.pair <- update(pairs(p1.acc.m8), by = NULL, adjust = "holm")
p1.acc.m8.pair %>% summary(infer = T) %>% kable(digits=3)

# 2.3.8.3. assumption check
# 1) Linearity & Homoskedasticity(equal variance)
m1 <- p1.acc.glmixed$full_model
plot(fitted(m1), residuals(m1))+
  abline(h=0, col="red", lwd=1, lty=2)
plot(residuals(m1))

# 2) Normality of residuals
hist(residuals(m1))
qqnorm(residuals(m1)) + 
  qqline(residuals(m1), col='2')
describe(residuals(m1)) # check skewness / kurtosis

# 2.3.8.4. coefficient - random intercept
coef(m1) # 추정된 참가자별 Intercept (개인차)
ranef(m1) # 추정된 참가자별 변화량 (grandMean - coefficient)
coef(m1)$SN$`(Intercept)` - ranef(m1)$SN$`(Intercept)`


# ---------------------------------------------------------- #
# 3. Response Time Analysis ####
# ---------------------------------------------------------- #

# ------------------ #
# 3.0.1 Preprocessing

# filter null/incorrect data
cp1 <- p1 %>% filter(Corr ==1) # remove incorrect trial
# cp1 <- cp1 %>% filter(locrep!="NULL") # remove null trial

# check Accuracy
100-100*(nrow(cp1)/nrow(p1))
# [1] 18.89
# 18.89% incorrect trials were not analyzed

# check distribution
hist(cp1$RT)

# trimming 3sd outlier trials
tp1 <- cp1 %>% filter(RT > 200 & RT < 10000) %>%
  group_by(SN) %>% # grouping by participants
  nest() %>%
  mutate(lbound = map(data, ~mean(.$RT)-3*sd(.$RT)),
         ubound = map(data, ~mean(.$RT)+3*sd(.$RT))) %>% # make new data (3sd cut)
  unnest(c(lbound, ubound))%>% 
  unnest(data) %>% 
  mutate(Outlier = (RT < lbound)|(RT > ubound)) %>% # set outlier
  filter(Outlier == FALSE) %>% # filtering outlier
  ungroup() %>% 
  select(SN, Btw, Block, cCue, IMname, Resp, RT, Corr) # select variables to analyze

# outlier trial ratio
100-100*(nrow(tp1)/nrow(cp1))
## [1] 1.83
# RTs were trimmed
# when 1) faster than 200ms 2) slower than 10 secs 3) 3SD away from the participants mean

# mean number of trials for each conditions
tp1 %>% group_by(SN, Block) %>% 
  summarise(NumTrial = length(RT)) %>%
  ungroup() %>%
  group_by(Block) %>%
  summarise(Mean = mean(NumTrial), 
            Median = median(NumTrial), 
            Min = min(NumTrial), 
            Max = max(NumTrial)) %>% 
  ungroup %>%
  kable(digits=2)


# -------------------------- #
# 3.0.2 Check Distribution

# before trimming
den1 <- ggplot(cp1, aes(x=RT)) + 
  geom_density() + 
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 
den1

# after trimming
den2 <- ggplot(tp1, aes(x=RT)) + 
  geom_density() + 
  theme_bw(base_size = 18) + 
  labs(x = "Trimmed RT") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 
den2


# transformation (R in Action 8.5.2를 참조한다. p199)
# 데이터가 normality 가정을 충족하지 않는 경우, 변수의 특성을 살펴보고 변환한다. 
# 일반적인 데이터 변환은 변수 y를 y^λ로 바꾸어 수행한다. 

# 대표적인 데이터 변환 방법
#     (Y^λ)    λ |   -2   |  -1   |    -0.5    |    0    |   0.5    |  1  |   2   |
# Transformation |  1/Y^2 |  1/Y  |  1/sqrt(Y) |  log(Y) |  sqrt(Y) |  Y  |  Y^2  |

# 어떻게 변환할지는 변환할 데이터의 특성을 살펴보고 결정한다. 
# car::powerTransform() function : 변수 Y^λ를 정규화하는 데에 가장 좋은 λ을 MLE로 찾아준다. 
# car::powerTransform(cp1$RT) # p1$lambda : Est.Power -0.6816, 

ttp1 <- cp1 %>% filter(RT > 200) %>% 
  mutate(logRT = log(RT), sqrtRT = sqrt(RT), transRT = 1/RT, sq.transRT = 1/(RT^2))
# after transformation
den3 <- ttp1 %>% 
  ggplot(aes(x=logRT)) + 
  geom_density() + 
  theme_bw(base_size = 18) + 
  labs(x = "Log(RT)") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 
den3

den4 <- ttp1 %>% 
  ggplot(aes(x=sqrtRT)) + 
  geom_density() + 
  theme_bw(base_size = 18) + 
  labs(x = "sqrt(RT)") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 
den4

den5 <- ttp1 %>% 
  ggplot(aes(x=transRT)) + 
  geom_density() + 
  theme_bw(base_size = 18) + 
  labs(x = "Trans RT)") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 
den5

den6 <- ttp1 %>% 
  ggplot(aes(x=sq.transRT)) + 
  geom_density() + 
  theme_bw(base_size = 18) + 
  labs(x = "sq.Trans RT)") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 
den6

ggarrange(den1,den2,den3,den4,den5,den6)

# see structure
glimpse(tp1)

# ---------------------------------------------------------- #
# 3.1. Descriptive Statistical Analysis - RT ####
# ---------------------------------------------------------- #

# subject-level, long format (SN/Btw/Block)
p1rtL <- tp1 %>% group_by(SN, Btw, Block) %>%
  summarise(RT = mean(RT)) %>%
  ungroup()
p1rtL %>% kable(digits=2)

# subject-level, wide format (SN/Btw/Block)
p1rtW <- p1rtL %>% spread(key=Block, value = RT)
p1rtW %>% kable(digits=2)

# summary table: grand mean (eyerep/locrep)
p1rtG <- p1rtL %>% group_by(Btw, Block) %>%
  summarise(RT.M = mean(RT), RT.SD = sd(RT)) %>%
  ungroup()
p1rtG$RT.se <- Rmisc::summarySEwithin(data = p1rtL, measurevar = "RT", 
                                        idvar = "SN", betweenvars = "Btw", withinvars = "Block")$se
p1rtG$RT.ci <- Rmisc::summarySEwithin(data = p1rtL, measurevar = "RT", 
                                        idvar = "SN", betweenvars = "Btw", withinvars = "Block")$ci
p1rtG <- p1rtG %>% 
  mutate(lower.ci = RT.M-RT.ci,
         upper.ci = RT.M+RT.ci)
# for between-subject design. check help(summarySE)
p1rtG %>% kable(digits=2)

# marginal means of group conditions
p1rtL %>% group_by(Btw) %>%
  summarise(M = mean(RT), SD = sd(RT)) %>%
  ungroup() %>% kable(digits=2)

# marginal means of block conditions
p1rtL %>% group_by(Block) %>%
  summarise(M = mean(RT), SD = sd(RT)) %>%
  ungroup() %>% kable(digits=2)


# ----------------------------------------------------------#
# 3.2. Plot - RT ####
# ----------------------------------------------------------#

# simple plot
apa_barplot(data=as.data.frame(p1rtL), 
            id="SN", dv="RT", 
            ylab = "RT (ms)", xlab = "Btw", 
            main = c("Response Time"),  
            # dispersion =  within_subjects_conf_int, # w/n confidence interval
            factors=c("Btw", "Block"), 
            ylim = c(0, 2000),
            las=1)

apa_beeplot(data=as.data.frame(p1rtL), 
            id="SN", dv="RT", 
            ylab = "RT (ms)", xlab = "Btw", 
            main = c("Response Time"),  
            # dispersion =  within_subjects_conf_int, # w/n confidence interval
            factors=c("Btw", "Block"), 
            ylim = c(0, 2000),
            las=1)

# RT by participant
ggplot(data=p1rtL, aes(x = SN, y = RT)) + 
  ggtitle("참가자별 평균 반응시간") +
  xlab("참가자") + ylab("RT") + theme_bw() +
  theme(text=element_text(size=14)) +
  theme(legend.title = element_text(face = 1,size = 15)) +
  geom_boxplot()
ggplot(data=p1rtL, aes(x = Item, y = RT)) + 
  ggtitle("자극별 평균 반응시간") +
  xlab("자극") + ylab("RT") + theme_bw() +
  theme(text=element_text(size=14)) +
  theme(legend.title = element_text(face = 1,size = 15)) +
  geom_boxplot()


# ggplot 1
plot1 <- ggplot(data=p1rtL, aes(x=Block, y=RT, fill=Btw)) +
  stat_summary(fun.y = mean, geom = "bar", position="dodge", na.rm = TRUE, alpha = .9, 
               width = 0.8, colour="black", size = 1.02, show.legend = FALSE) +
  geom_pointrange(data=p1rtG, aes(x = Block, y=RT.M, ymin = lower.ci, ymax = upper.ci),
                  position = position_dodge(0.80), color = "darkred", size = 1, show.legend = FALSE) +
  facet_grid(.~Btw, scales="free_x", space = "free",
             labeller = labeller(Btw = c("exp" = "Experimental Group","con" = "Control Group"))) +
  geom_point(position=position_dodge(0.5), color="gray80", size=2, show.legend = FALSE) +
  geom_segment(data=filter(p1rtW, Btw == "exp"), inherit.aes = FALSE,
               aes(x=1, y=filter(p1rtW, Btw == "exp")$b1,
                   xend=2, yend=filter(p1rtW, Btw == "exp")$b2),
               color="gray80") +
  geom_segment(data=filter(p1rtW, Btw == "exp"), inherit.aes = FALSE,
               aes(x=2, y=filter(p1rtW, Btw == "exp")$b2,
                   xend=3, yend=filter(p1rtW, Btw == "exp")$b3),
               color="gray80") +
  geom_segment(data=filter(p1rtW, Btw == "exp"), inherit.aes = FALSE,
               aes(x=3, y=filter(p1rtW, Btw == "exp")$b3,
                   xend=4, yend=filter(p1rtW, Btw == "exp")$b4),
               color="gray80") +
  geom_segment(data=filter(p1rtW, Btw == "con"), inherit.aes = FALSE,
               aes(x=1, y=filter(p1rtW, Btw == "con")$b1,
                   xend=2, yend=filter(p1rtW, Btw == "con")$b2),
               color="gray80") +
  geom_segment(data=filter(p1rtW, Btw == "con"), inherit.aes = FALSE,
               aes(x=2, y=filter(p1rtW, Btw == "con")$b2,
                   xend=3, yend=filter(p1rtW, Btw == "con")$b3),
               color="gray80") +
  geom_segment(data=filter(p1rtW, Btw == "con"), inherit.aes = FALSE,
               aes(x=3, y=filter(p1rtW, Btw == "con")$b3,
                   xend=4, yend=filter(p1rtW, Btw == "con")$b4),
               color="gray80") +
  scale_x_discrete(labels=c("1","2","3","4")) +
  scale_fill_manual(values = c("#feb24c", "#91bfdb"),
                    labels = c("Experimental", "Control")) +
  coord_cartesian(ylim = c(0, 2000), clip = "on") +
  labs(x = "Block", y = "Response Time (ms)", fill ="Group") +
  # ggtitle("Association Memory Accuracy") +
  theme_bw(base_size = 15) +
  theme(axis.title = element_text(face = "bold", size = 16, color = "black"),
        axis.text = element_text(face = "plain", hjust = 0.5, size = 15, color = "black"),
        axis.line=element_line(),
        strip.text.x = element_text(face = "plain", size = 15, color = "black"),
        strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.spacing=unit(1, "lines"),
        # aspect.ratio = 0.2,
        plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"), 
        # plot.title = element_text(hjust = 0.5),
        legend.position=c(0.8, 0.85))
plot1

# ggplot 2
plot2 <- ggplot(p1rtG, mapping=aes(x=Block, y=RT.M, group=Btw)) + 
  geom_ribbon(p1rtG, mapping=aes(x=Block, ymin=lower.ci, ymax=upper.ci, fill=Btw), alpha=0.5) + 
  geom_line(p1rtG, mapping=aes(x=Block, y=RT.M, color=Btw), size = 1, show.legend = FALSE) +
  geom_pointrange(aes(x = Block, ymin=lower.ci, ymax=upper.ci, color=Btw), position = position_dodge(0), 
                  size = 0.8, show.legend = FALSE) +
  scale_fill_manual(values = c("darkred", "darkblue"), labels = c("Experimental", "Control")) +
  scale_color_manual(values = c("darkred", "darkblue")) +
  coord_cartesian(ylim = c(0, 2000), clip = "on") +
  labs(x = "Block", y = "Response Time (ms)", fill="Group") +
  scale_x_discrete(labels = c("1", "2", "3", "4")) +
  # ggtitle("A) All Group") +
  theme_bw(base_size = 15) +
  theme(axis.title = element_text(face = "bold", size = 16, color = "black"),
        axis.text = element_text(face = "plain", hjust = 0.5, size = 15, color = "black"),
        axis.line=element_line(),
        aspect.ratio = 1,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border=element_blank(),
        plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
        legend.position = c(0.65, 0.75))
plot2

# ggplot 3
p1rtG$Grp <- factor(p1rtG$Btw, labels=c("Experimental", "Control"))
plot3 <- ggplot(data=p1rtG, aes(x=Block, y=RT.M, ymin=lower.ci, ymax=upper.ci, color=Grp, shape=Grp)) +
  geom_point(size = 4, position = position_dodge(.3)) +
  geom_errorbar(width = .2, position = position_dodge(.3)) +
  geom_line(aes(group = Grp), position = position_dodge(.3)) + 
  scale_color_manual(values = c("red", "black")) +
  coord_cartesian(ylim = c(0,2000), clip = "on") +
  # scale_y_continuous(breaks = seq(0,80)) +
  scale_x_discrete(labels = c("1", "2", "3", "4")) +
  labs(x = "Block", y = "Response Time (ms)") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.65, 0.75),
        legend.title = element_blank(),
        aspect.ratio = 1,
        legend.background = element_blank(),
        plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
        legend.key = element_blank())
plot3

ggarrange(plot1, ggarrange(plot2, plot3, ncol = 2, labels=c("B", "C"), 
                           hjust = -0.05, vjust=1, font.label = list(size = 20)), labels=c("A"), 
          hjust = -0.05, vjust=1, font.label = list(size = 20), ncol=1, nrow=2)
cowplot::plot_grid(plot2, plot3, ncol = 2, 
                   labels = c('B', 'C'), label_size = 20)


# *** SEE Appendix to adjust things in ggplot ***

# ---------------------------------------------------------- #
# 3.3. Inferential Statistical Analysis - RT ####
# ---------------------------------------------------------- #
# 3.3.1. t-Test ####

# 3.3.1.1. independent samples t-test
p1.rt.ttest1 <- t.test(RT ~ Btw, data = p1rtL,
                        alternative = c("two.sided"),
                        paired=F, var.equal=F,
                        conf.level=.95)
p1.rt.ttest1

p1rtL.exp <- p1rtL %>% filter(Btw=="exp")
p1rtL.con <- p1rtL %>% filter(Btw=="con")
p1.rt.ttest2 <- t.test(p1rtL.exp$RT, p1rtL.con$RT,
                        alternative = c("two.sided"),
                        paired=F, var.equal=F,
                        conf.level=.95)
p1.rt.ttest2

p1.rt.ttest3 <- compare.2.vectors(p1rtL.exp$RT, p1rtL.con$RT, 
                                   paired=F, tests=c("parametric"), coin=T,
                                   alternative = "two.sided") # afex pacakage
p1.rt.ttest3

# effect size 
lsr::cohensD(RT ~ Btw,
             data = p1rtL)

# 3.3.1.2. paired samples t-test
p1rtL.b12 <- p1rtL %>% filter(Block==c("b1","b2"))
p1.rt.ttest4 <- t.test(RT ~ Block, data=p1rtL.b12,
                        alternative = c("two.sided"),
                        paired=T, var.equal=F,
                        conf.level=.95)
p1.rt.ttest4


p1rtL.b1 <- p1rtL %>% filter(Block=="b1")
p1rtL.b2 <- p1rtL %>% filter(Block=="b2")
p1.rt.ttest5 <- t.test(p1rtL.b1$RT, p1rtL.b2$RT,
                        alternative = c("two.sided"),
                        paired=T, var.equal=F,
                        conf.level=.95)
p1.rt.ttest5

p1.rt.ttest6 <- compare.2.vectors(p1rtL.b1$RT, p1rtL.b2$RT, 
                                   paired=T, tests=c("parametric"), coin=T,
                                   alternative = "two.sided") # afex pacakage
p1.rt.ttest6

# effect size 
p1rtL.b12$Block <-factor(p1rtL.b12$Block, labels=c("b1","b2"))
lsr::cohensD(RT ~ Block,
             data = p1rtL.b12)

# --------------- #
# 3.3.2. ANOVA ####

# 3.3.2.1. ANOVA
p1.rt.aov1 <- aov_ez(id="SN", dv = "RT", data = p1rtL, between = "Btw")
test_levene(p1.rt.aov1)
summary(p1.rt.aov1)
anova(p1.rt.aov1)
anova(p1.rt.aov1, es = "pes")
nice(p1.rt.aov1, es = "pes")

# 3.3.2.2. RM ANOVA
p1.rt.aov2 <- aov_ez(id="SN", dv = "RT", data = p1rtL, within = "Block")
test_sphericity(p1.rt.aov2)
summary(p1.rt.aov2)
anova(p1.rt.aov2)
anova(p1.rt.aov2, es = "pes")
nice(p1.rt.aov2, es="pes")

# 3.3.2.3. Mixed Factorial ANOVA
p1.rt.aov3 <- aov_ez(id="SN", dv = "RT", data = p1rtL, between = c("Btw"), within = c("Block"))
test_levene(p1.rt.aov3)
test_sphericity(p1.rt.aov3)
summary(p1.rt.aov3)
anova(p1.rt.aov3)
anova(p1.rt.aov3, es = "pes") %>% kable(digits=3)
nice(p1.rt.aov3, es="pes") 
# pes = partial eta-squared

# simple plot for aov_ez
afex_plot(p1.rt.aov3, x = c("Block"), 
          trace = "Btw", 
          error = "none")

# 3.3.2.4. post-hoc
p1.rt.m1 <- emmeans(p1.rt.aov3, pairwise ~ Block | Btw, type = "response") # adjust="bon"
p1.rt.m1$contrasts %>% kable(digits=2)

p1.rt.m2 <- emmeans(p1.rt.aov3, "Block", by = "Btw", type = "response")
p1.rt.m2.pair <- update(pairs(p1.rt.m2), by = NULL, adjust = "tukey")
p1.rt.m2.pair %>% summary(infer = TRUE) %>% kable(digits=2)

# main effect only
p1.rt.m3  <- emmeans(p1.rt.aov3, ~ Block)
pairs(p1.rt.m3)
p1.rt.m3.pair<- update(pairs(p1.rt.m3), by = NULL, adjust = "tukey")
p1.rt.m3.pair

summary(as.glht(pairs(p1.rt.m3)), test=multcomp::adjusted("free"))

# simple interaction only
p1.rt.m4  <- emmeans(p1.rt.aov3, ~ Block | Btw)
# equal: emmeans(p1.rt.aov3, "Block", by = "Btw")
pairs(p1.rt.m4)

p1.rt.m5  <- emmeans(p1.rt.aov3, ~ Block*Btw)
# equal: emmeans(p1.rt.aov3, c("Block", "Btw"))
pairs(p1.rt.m5)

# --------------------- #
# 3.3.3. Correlation ####

p1rtL.cor <- p1rtL %>% filter(Block==c("b1", "b2")) %>% spread(key = "Block", value = RT)
cor.test(formula = ~ b1 + b2, data = p1rtL.cor,
         method = "pearson", alternative = "two.sided")

ggplot(p1rtL.cor, aes(x=b1, y=b2)) +
  geom_point(size = 4) +
  geom_smooth(method=lm) +
  labs(x = "Block 1 RT", 
       y = "Block 2 RT") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        aspect.ratio = 1)

# ------------------------------- #
# 3.3.4. Linear Mixed Modeling ####

# 3.3.4.1. testing fixed effect with LMM

p1.rt.lmer.m1 <- lmer(logRT ~ Block + (Block|SN) + (1|IMname), ttp1, REML = F, 
                      control = lmerControl(optimizer = "bobyqa",
                        optCtrl = list(maxfun = 1e8)))
p1.rt.lmer.m2 <- lmer(logRT ~ Block+Btw + (Block|SN) + (1|IMname), ttp1, REML = F,
                      control = lmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 1e8)))
p1.rt.lmer.m3 <- lmer(logRT ~ Block*Btw + (Block|SN) + (1|IMname), ttp1, REML = F,
                      control = lmerControl(optimizer = "bobyqa",
                                            optCtrl = list(maxfun = 1e8)))
anova(p1.rt.lmer.m1, p1.rt.lmer.m2, p1.rt.lmer.m3)
anova(p1.rt.lmer.m1, p1.rt.lmer.m2)
anova(p1.rt.lmer.m2, p1.rt.lmer.m3)

# 3.3.4.2. fitting LMM - maximal model
p1.rt.lmer <- lmer(logRT ~ Block*Btw + (Block|SN) + (1|IMname), ttp1, REML = T, # item에 block slope 추가 시 singular fit
                   control = lmerControl(optimizer = "bobyqa",
                                         optCtrl = list(maxfun = 1e8)))
Anova(p1.rt.lmer, test.statistic="Chisq")
Anova(p1.rt.lmer, test.statistic="F")
anova(p1.rt.lmer)
summary(p1.rt.lmer)

save(p1.rt.lmer, p1.rt.lmer.m1, p1.rt.lmer.m2, p1.rt.lmer.m3, file="p1rt_lmer.Rdata")

# 3.3.4.3. post-hoc
p1.rt.m6 <- emmeans(p1.rt.lmer, pairwise ~ Block | Btw, type = "response") # adjust="bon"
p1.rt.m6$contrasts %>% kable(digits=2)

p1.rt.m6 <- emmeans(p1.rt.lmer, "Block", by=c("Btw"), type = "response",
                    lmerTest.limit = 4153) # adjust="bon"
p1.rt.m6.pair <- update(pairs(p1.rt.m6), by = NULL, adjust = "tukey")
p1.rt.m6.pair %>% summary(infer=T) %>% kable(digits=3)
p1.rt.m6$contrasts %>% kable(digits=2)

# 3.3.4.4. assumption check
# 1) Linearity & Homoskedasticity(equal variance)
plot(fitted(p1.rt.lmer),residuals(p1.rt.lmer)) + 
  abline(h=0, col="red", lwd=1, lty=2)
plot(residuals(p1.rt.lmer))

# 2) Normality of residuals
hist(residuals(p1.rt.lmer))
qqnorm(residuals(p1.rt.lmer)) + 
  qqline(residuals(p1.rt.lmer), col='2')

# 3.3.4.5. coefficient - random intercept
coef(p1.rt.lmer) # 추정된 참가자별 Intercept (개인차)
ranef(p1.rt.lmer) # 추정된 참가자별 변화량 (grandMean - coefficient)
coef(p1.rt.lmer)$SN$`(Intercept)` - ranef(p1.rt.lmer)$SN$`(Intercept)`

# 3.3.4.6. LRT(likelihood ratio test) on random effects
lmerTest::rand(p1.rt.lmer)

# -------------------------------------------------- #
# 3.3.5. Linear Mixed Modeling with Afex packages ####
load("p1rt_lmer_mixed.Rdata")

# 3.3.5.1. fitting LMM - S method
# (nc <- detectCores())
# cl <- makeCluster(rep("localhost", nc))
# p1.rt.lmixed <- afex::mixed(logRT ~ Btw*Block + (Block|SN) + (1|IMname),
#                         data = ttp1, REML = T, expand_re=T,
#                         method = "S", cl = cl,
#                         control = lmerControl(optimizer = "bobyqa",
#                                               optCtrl = list(maxfun = 1e6)))

# nested model을 비교하는 method (Singmann, & Kellen, 2017)
# 1) Kenward-Roger approximation : method ="KR"
# 2) Satterthwaite approximation : method = "S"
# 3) Likelihood Ratio Test : method = "LRT"
# 4) Parametric bootstrapping : method = "PB"

# save(p1.rt.lmixed, file="p1rt_lmer_mixed.Rdata")
# stopCluster(cl)

summary(p1.rt.lmixed)
anova(p1.rt.lmixed)
nice(p1.rt.lmixed)

# 3.3.5.2. post-hoc
p1.rt.m7 <- emmeans(p1.rt.lmixed, pairwise ~ Block | Btw, type = "response", 
                    lmerTest.limit = 4153) # adjust="bon"
p1.rt.m7$contrasts %>% kable(digits=3)

p1.rt.m7 <- emmeans(p1.rt.lmixed, "Block", by="Btw", type = "response", 
                    lmerTest.limit = 4153) # adjust="bon"
p1.rt.m7.pair <- update(pairs(p1.rt.m7), by = NULL, adjust = "holm")
p1.rt.m7.pair %>% summary(infer = T) %>% kable(digits=3)

# 3.3.5.3. assumption check
# 1) Linearity & Homoskedasticity(equal variance)
m1 <- p1.rt.lmixed$full_model
plot(fitted(m1), residuals(m1))+
  abline(h=0, col="red", lwd=1, lty=2)
plot(residuals(m1))

# 2) Normality of residuals
hist(residuals(m1))
qqnorm(residuals(m1)) + 
  qqline(residuals(m1), col='2')
describe(residuals(m1)) # check skewness / kurtosis

# 3.3.5.4. coefficient - random intercept
coef(m1) # 추정된 참가자별 Intercept (개인차)
ranef(m1) # 추정된 참가자별 변화량 (grandMean - coefficient)
coef(m1)$SN$`(Intercept)` - ranef(m1)$SN$`(Intercept)`

# ------------------------------------------- #
# 3.3.6. Generalized Linear Mixed Modeling ####
load("p1rt_glmer.Rdata")


# 3.3.6.1 testing fixed effect with GLMM
p1.rt.glmer.m1 <- glmer(RT ~ Block + (1|SN) + (1|IMname), tp1,
                      family = inverse.gaussian(link="identity")) 
p1.rt.glmer.m2 <- glmer(RT ~ Block+Btw + (1|SN) + (1|IMname), tp1,
                         family = inverse.gaussian(link="identity")) 
p1.rt.glmer.m3 <- glmer(RT ~ Block*Btw + (1|SN) + (1|IMname), tp1,
                         family = inverse.gaussian(link="identity"),
                         glmerControl(optimizer = c("bobyqa"),
                                      optCtrl = list(maxfun = 1e7))) 
anova(p1.rt.glmer.m1, p1.rt.glmer.m2, p1.rt.glmer.m3)
anova(p1.rt.glmer.m1, p1.rt.glmer.m2)
anova(p1.rt.glmer.m2, p1.rt.glmer.m3)

# 2.3.7.2 fitting GLMM
p1.rt.glmer <- glmer(RT ~ Block*Btw + (Block|SN) + (1|IMname), tp1,
                     family = inverse.gaussian(link="identity"), expand_re=T,
                     glmerControl(optimizer = c("bobyqa"), 
                                  optCtrl = list(maxfun = 1e7))) # binomial for rturacy data
# 만약 singular fit이 나타난다면, 무선 효과 구조를 축소한다. 
p1.rt.glmer.reduced <- glmer(RT ~ Block*Btw + (Block||SN) + (1|IMname), tp1,
                      family = inverse.gaussian(link="identity"), expand_re=T,
                      glmerControl(optimizer = c("bobyqa"),
                                   optCtrl = list(maxfun = 1e7))) # binomial for rturacy data

p1.rt.glmer.reduced1 <- glmer(RT ~ Block*Btw + (1|SN) + (1|IMname), tp1,
                              family = inverse.gaussian(link="identity"), expand_re=T,
                              glmerControl(optimizer = c("bobyqa"), 
                                           optCtrl = list(maxfun = 1e8))) # binomial for rturacy data

save(p1.rt.glmer.m1, p1.rt.glmer.reduced, p1.rt.glmer.reduced1,
     p1.rt.glmer.m2, p1.rt.glmer.m3, p1.rt.glmer, file = "p1rt_glmer.Rdata")

Anova(p1.rt.glmer.reduced1, test.statistic="Chisq")
anova(p1.rt.glmer.reduced, p1.rt.glmer.reduced1)
anova(p1.rt.glmer.reduced1)
summary(p1.rt.glmer.reduced1)

# 2.3.7.3 post-hoc
p1.rt.m7 <- emmeans(p1.rt.glmer.reduced1, pairwise ~ Block | Btw, type = "response") # adjust="bon"
p1.rt.m7$contrasts %>% kable(digits=2)

# 2.3.7.4 assumption check
# 1) Linearity & Homoskedasticity(equal variance)
plot(fitted(p1.rt.glmer.reduced1), residuals(p1.rt.glmer.reduced1))+
  abline(h=0, col="red", lwd=1, lty=2)
plot(residuals(p1.rt.glmer.reduced1))

# 2) Normality of residuals
hist(residuals(p1.rt.glmer.reduced1))
qqnorm(residuals(p1.rt.glmer.reduced1)) + 
  qqline(residuals(p1.rt.glmer.reduced1), col='2')

describe(residuals(p1.rt.glmer)) # check skewness / kurtosis

# 2.3.7.5 coefficient - random intercept
coef(p1.rt.glmer.reduced1) # 추정된 참가자별 Intercept (개인차)
ranef(p1.rt.glmer.reduced1) # 추정된 참가자별 변화량 (grandMean - coefficient)
coef(p1.rt.glmer)$SN$`(Intercept)` - ranef(p1.rt.glmer)$SN$`(Intercept)`

# 2.3.7.6 Best Linear Unbias Predictions (BLUPs)
ranef(p1.rt.glmer)$SN
AA <- ranef(p1.rt.glmer)$SN
colnames(AA) <- c("intercept","Block2","Block3","Block4")
A1 <- AA %>% rownames_to_column() %>% 
  select(rowname, intercept) %>% 
  column_to_rownames(var = "rowname")
A2 <- AA %>% rownames_to_column() %>% 
  select(rowname, Block3) %>% 
  column_to_rownames(var = "rowname")
BLUPs <- cbind(A1, A2)

cor.test(formula = ~ intercept + Block3, data = BLUPs,
         method = "pearson", alternative = "two.sided")
BLUPs %>% kable(digits=3)

B.P <- ggplot(BLUPs, aes(x=intercept, y=Block3)) +
  geom_point(size = 4) +
  geom_smooth(method=lm) +
  labs(x = expression(Delta*" b1 to b2"), 
       y = expression(Delta*" b1 to b3")) +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        aspect.ratio = 1)
B.P

# ---------------------- #
# GLMM visualization 1 (not recommended)
sjPlot::plot_model(p1.rt.glmer, type = c("int"), mdrt.values="all")

# --------------------- #
# GLMM visualization 2
# extract effects
library(effects) # download
model_predicted_effects <- as.data.frame(effects::effect("Block * Btw", p1.rt.glmer))
ggplot(data = model_predicted_effects,
       aes(x = Btw, y = fit, colour = Block)) +
  geom_pointrange(aes(ymax = upper,
                      ymin = lower),
                  position = position_dodge(width = 1)) +
  # geom_line(aes(x = Block, y = fit, group = Btw),
  # position = position_dodge(width = 1)) +
  ylab("RT") +
  xlab("Block") +
  scale_colour_grey() +
  theme_classic() +
  theme(legend.justification=c(1,1), legend.position=c(1,1))
# these are estimated/predicted plots
# you can check exact numbers with model_predicted_effects


# -------------------------------------------------------------- #
# 3.3.7. Generalized Linear Mixed Modeling with Afex packages ####
load("p1rt_glmer_mixed.Rdata")

# 3.3.7.1. fitting GLMM - LRT method
(nc <- detectCores())
cl <- makeCluster(rep("localhost", nc))
p1.rt.glmixed <- afex::mixed(RT ~ Btw*Block + (1|SN) + (1|IMname),
                             data = tp1,
                             family = inverse.gaussian(link="identity"),
                             method = "LRT", cl = cl,
                             expand_re = F,
                             control = lmerControl(optimizer = "bobyqa",
                                                   optCtrl = list(maxfun = 1e7)))

# nested model을 비교하는 method (Singmann, & Kellen, 2017)
# 1) Kenward-Roger approximation : method ="KR"
# 2) Satterthwaite approximation : method = "S"
# 3) Likelihood Ratio Test : method = "LRT"
# 4) Parametric bootstrapping : method = "PB"

save(p1.rt.glmixed, file="p1rt_glmer_mixed.Rdata")
stopCluster(cl)

summary(p1.rt.glmixed)
anova(p1.rt.glmixed)
nice(p1.rt.glmixed)

# 3.3.7.2. post-hoc
p1.rt.m8 <- emmeans(p1.rt.glmixed, pairwise ~ Block | Btw, type = "response") # adjust="bon"
p1.rt.m8$contrasts %>% kable(digits=3)
plot(p1.rt.m8, horizontal = FALSE, comparisons = T)

p1.rt.m9 <- emmeans(p1.rt.glmixed, pairwise ~ Btw | Block, type = "response") # adjust="bon"
p1.rt.m9$contrasts %>% kable(digits=3)
plot(p1.rt.m9, horizontal = FALSE, comparisons = T)

p1.rt.m8 <- emmeans(p1.rt.glmixed, "Block", by="Btw", type = "response") # adjust="bon"
p1.rt.m8.pair <- update(pairs(p1.rt.m8), by = NULL, adjust = "holm")
p1.rt.m8.pair %>% summary(infer = T) %>% kable(digits=3)

# 3.3.5.3. assumption check
# 1) Linearity & Homoskedasticity(equal variance)
m1 <- p1.rt.glmixed$full_model
plot(fitted(m1), residuals(m1))+
  abline(h=0, col="red", lwd=1, lty=2)
plot(residuals(m1))

# 2) Normality of residuals
hist(residuals(m1))
qqnorm(residuals(m1)) + 
  qqline(residuals(m1), col='2')
describe(residuals(m1)) # check skewness / kurtosis

# 3.3.5.4. coefficient - random intercept
coef(m1) # 추정된 참가자별 Intercept (개인차)
ranef(m1) # 추정된 참가자별 변화량 (grandMean - coefficient)
coef(m1)$SN$`(Intercept)` - ranef(m1)$SN$`(Intercept)`


# ------------------------- #
# 3.3.8. Bayes Factor ####

# Let's get bayes factor (it's so easy)
library(BayesFactor)
bf.rt <- anovaBF(RT ~ Btw*Block, data=tp1, whichRandom=c("SN","IMname"))
summary(bf.rt)
plot(bf.rt)

# ---------------------------------------------------------- #
# 4. Calculate Power & Sample Size ####
# ---------------------------------------------------------- #

# ---------------------------------------------------------- #
# 4.1. for ANOVA ####
# ---------------------------------------------------------- #

# calculate sample size (2x2)
library(pwr2)
ss.2way(a=2, b=2, alpha=0.05, beta=0.2, f.A = 20.59, f.B = 161.35, B=100)
# a = factor A, b = factor B, power = 1-beta, f.A/f.B = effect size of factor A/B, B = iteration

# calculate power (2x2)
pwr.2way(a=2, b=2, alpha=0.05, size.A = 27, size.B = 27, f.A = 20.59, f.B = 161.350)
# a = factor A, b = factor B, power = 1-beta, f.A/f.B = effect size of factor A/B, B = iteration

# use ss.1way or pwr.1way for one-way design
# a/b -> k, f.A/f.B -> f, size.A/B -> size

# ---------------------------------------------------------- #
# 4.2. for Mixed Models ####
#      (...Updating...)
# ---------------------------------------------------------- #

library(simr)
# for lmer
power.lmer <- lmer(RT ~ eyerep*locrep + (1|SN) + (1|tarimg), tp1)
powerSim(power.lmer, nsim=200) # nsim = number of simulation

# for glmer
# but not working with glmer distribution arguments!!!!
# (I'm fixing it)
power.glmer <- glmer(RT ~ eyerep + (eyerep|SN) + (eyerep|tarimg), data = tp1
                     # ,family = inverse.gaussian(link="identity")
)
powerSim(power.glmer, nsim=10)

# ------------------------------------------------------------------ #
# ------------------------------------------------------------------ #
# Appendix 1. ggplot Guide ####
# ------------------------------------------------------------------ #
# ------------------------------------------------------------------ #


# ----------------------------------------------------------#
# Other Plots ####
# ----------------------------------------------------------#

# -------------------- #
# Violin Plot

ggplot(data=p1rtL, aes(x=Btw, y=RT, fill=Block)) +
  geom_violin(position=position_dodge(1), width = 1, trim=TRUE) +
  geom_point(position=position_dodge(1), color="gray80", size=1.8, show.legend = FALSE) +
  geom_segment(data=filter(p1rtW, Btw == "exp"), inherit.aes = FALSE,
               aes(x=1-.375, y=filter(p1rtW, Btw == "exp")$b1,
                   xend=1-.125, yend=filter(p1rtW, Btw == "exp")$b2),
               color="gray80") +
  geom_segment(data=filter(p1rtW, Btw == "exp"), inherit.aes = FALSE,
               aes(x=1-.125, y=filter(p1rtW, Btw == "exp")$b2,
                   xend=1+.125, yend=filter(p1rtW, Btw == "exp")$b3),
               color="gray80") +
  geom_segment(data=filter(p1rtW, Btw == "exp"), inherit.aes = FALSE,
               aes(x=1+.125, y=filter(p1rtW, Btw == "exp")$b3,
                   xend=1+.375, yend=filter(p1rtW, Btw == "exp")$b4),
               color="gray80") +
  geom_segment(data=filter(p1rtW, Btw == "con"), inherit.aes = FALSE,
               aes(x=2-.375, y=filter(p1rtW, Btw == "con")$b1,
                   xend=2-.125, yend=filter(p1rtW, Btw == "con")$b2),
               color="gray80") +
  geom_segment(data=filter(p1rtW, Btw == "exp"), inherit.aes = FALSE,
               aes(x=2-.125, y=filter(p1rtW, Btw == "con")$b2,
                   xend=2+.125, yend=filter(p1rtW, Btw == "con")$b3),
               color="gray80") +
  geom_segment(data=filter(p1rtW, Btw == "con"), inherit.aes = FALSE,
               aes(x=2+.125, y=filter(p1rtW, Btw == "con")$b3,
                   xend=2+.375, yend=filter(p1rtW, Btw == "con")$b4),
               color="gray80") +
  geom_pointrange(data=p1rtG,
                  aes(x = Btw, y = RT.M, ymin = lower.ci, ymax = upper.ci, group = Block),
                  position = position_dodge(1), color = "darkred", size = 1, show.legend = FALSE) +
  labs(x = "Group", 
       y = "Reaction Times (ms)", 
       fill='Block') +
  coord_cartesian(ylim = c(600, 2000), clip = "on") +
  scale_x_discrete(labels=c("Experimental", "Control")) +
  scale_fill_manual(values=c("#ff80b0", "#9399ff", "#ff80b0", "#9399ff"), 
                    labels=c("Block 1","Block 2" ,"Block 3" ,"Block 4")) +
  theme_bw(base_size = 15) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank()) 

# -------------------- #
# 3.2.1.2 Violin Plot 2

ggplot(data=p1rtL, aes(x=Btw, y=RT, fill=Block)) +
  geom_violin(position=position_dodge(1), width = 1, trim=TRUE) +
  geom_point(position=position_dodge(1), color="gray80", size=1.8, show.legend = FALSE) +
  geom_segment(data=filter(p1rtW, Btw == "exp"), inherit.aes = FALSE,
               aes(x=1-.375, y=filter(p1rtW, Btw == "exp")$b1,
                   xend=1-.125, yend=filter(p1rtW, Btw == "exp")$b2),
               color="gray80") +
  geom_segment(data=filter(p1rtW, Btw == "exp"), inherit.aes = FALSE,
               aes(x=1-.125, y=filter(p1rtW, Btw == "exp")$b2,
                   xend=1+.125, yend=filter(p1rtW, Btw == "exp")$b3),
               color="gray80") +
  geom_segment(data=filter(p1rtW, Btw == "exp"), inherit.aes = FALSE,
               aes(x=1+.125, y=filter(p1rtW, Btw == "exp")$b3,
                   xend=1+.375, yend=filter(p1rtW, Btw == "exp")$b4),
               color="gray80") +
  geom_segment(data=filter(p1rtW, Btw == "con"), inherit.aes = FALSE,
               aes(x=2-.375, y=filter(p1rtW, Btw == "con")$b1,
                   xend=2-.125, yend=filter(p1rtW, Btw == "con")$b2),
               color="gray80") +
  geom_segment(data=filter(p1rtW, Btw == "exp"), inherit.aes = FALSE,
               aes(x=2-.125, y=filter(p1rtW, Btw == "con")$b2,
                   xend=2+.125, yend=filter(p1rtW, Btw == "con")$b3),
               color="gray80") +
  geom_segment(data=filter(p1rtW, Btw == "con"), inherit.aes = FALSE,
               aes(x=2+.125, y=filter(p1rtW, Btw == "con")$b3,
                   xend=2+.375, yend=filter(p1rtW, Btw == "con")$b4),
               color="gray80") +
  geom_pointrange(data=p1rtG,
                  aes(x = Btw, y = RT.M, ymin = lower.ci, ymax = upper.ci, group = Block),
                  position = position_dodge(1), color = "darkred", size = 1, show.legend = FALSE) +
  labs(x = "Group", 
       y = "Reaction Times (ms)", 
       fill='Block') +
  coord_cartesian(ylim = c(600, 2000), clip = "on") +
  scale_x_discrete(labels=c("Experimental", "Control")) +
  scale_fill_manual(values=c("#ff80b0", "#9399ff", "#ff80b0", "#9399ff"), 
                    labels=c("Block 1","Block 2" ,"Block 3" ,"Block 4")) +
  theme_bw(base_size = 15) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        #        legend.background = element_rect(color = "black", size = 0.35), # legend box
        legend.position = c(0.80, 0.80), # legend position
        legend.key.size = unit(0.3,"cm"), # legend key color size
        legend.text = element_text(size = 12), # legend text size
        legend.title = element_text(size = 12), # legend title size
        legend.direction = "vertical") + # legend text direction
  guides(fill = guide_legend(title.position = "top")) # legend title position


# ------------------------------------------------------- #
# 3.2.2. Segment Plot (grand mean, ci, individual mean)

ggplot() +
  geom_bar(data = p1rtG, aes(x=Btw, y=RT.M, fill=Block), 
           stat = "identity", position = position_dodge(1), colour = "black", size = 0.4) +
  geom_point(data=p1rtL, aes(x=Btw, y=RT, fill=Block), 
             position=position_dodge(1), color="gray80", size=1.8, show.legend = FALSE) +
  geom_segment(data=filter(p1rtW, Btw == "exp"), inherit.aes = FALSE,
               aes(x=1-.375, y=filter(p1rtW, Btw == "exp")$b1,
                   xend=1-.125, yend=filter(p1rtW, Btw == "exp")$b2),
               color="gray80") +
  geom_segment(data=filter(p1rtW, Btw == "exp"), inherit.aes = FALSE,
               aes(x=1-.125, y=filter(p1rtW, Btw == "exp")$b2,
                   xend=1+.125, yend=filter(p1rtW, Btw == "exp")$b3),
               color="gray80") +
  geom_segment(data=filter(p1rtW, Btw == "exp"), inherit.aes = FALSE,
               aes(x=1+.125, y=filter(p1rtW, Btw == "exp")$b3,
                   xend=1+.375, yend=filter(p1rtW, Btw == "exp")$b4),
               color="gray80") +
  geom_segment(data=filter(p1rtW, Btw == "con"), inherit.aes = FALSE,
               aes(x=2-.375, y=filter(p1rtW, Btw == "con")$b1,
                   xend=2-.125, yend=filter(p1rtW, Btw == "con")$b2),
               color="gray80") +
  geom_segment(data=filter(p1rtW, Btw == "exp"), inherit.aes = FALSE,
               aes(x=2-.125, y=filter(p1rtW, Btw == "con")$b2,
                   xend=2+.125, yend=filter(p1rtW, Btw == "con")$b3),
               color="gray80") +
  geom_segment(data=filter(p1rtW, Btw == "con"), inherit.aes = FALSE,
               aes(x=2+.125, y=filter(p1rtW, Btw == "con")$b3,
                   xend=2+.375, yend=filter(p1rtW, Btw == "con")$b4),
               color="gray80") +
  geom_errorbar(data=p1rtG,
                aes(x = Btw, ymin = lower.ci, ymax = upper.ci, group = Block),
                position = position_dodge(1), width = 0.1, color = "black", size = 0.3, show.legend = FALSE) +
  labs(x = "Group", 
       y = "Reaction Times (ms)", 
       fill='Block') +
  coord_cartesian(ylim = c(600, 2000), clip = "on") +
  scale_x_discrete(labels=c("Experimental", "Control")) +
  scale_fill_manual(values=c("#ff80b0", "#9399ff", "#ff80b0", "#9399ff"),
                    labels=c("Block 1","Block 2" ,"Block 3" ,"Block 4")) +
  theme_bw(base_size = 15) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank())


# ------------------------------------------------------------- #
# 3.2.3.1. MSK-preferred Plot (grand mean, ci, individual mean)

ggplot(data=p1rtL, aes(x=Btw, y=RT.M, fill=Block)) +
  geom_bar(data = p1rtG, stat = "identity", position = position_dodge(1), colour = "black", size = 0.3)+
  geom_errorbar(data=p1rtG,
                aes(x = Btw, ymin = lower.ci, ymax = upper.ci, group = Block),
                position = position_dodge(1), width = 0.1, color = "black", size = 0.3, show.legend = FALSE) +
  labs(x = "Group", 
       y = "Reaction Times (ms)", 
       fill='Block') +
  coord_cartesian(ylim = c(0, 2000), clip = "on") +
  theme_bw(base_size = 15) +
  # add aterisk ------------------------------------------ change x, y, or label (*, **, n.s.) #
  # solution to inconsistent thickness: adjust y & yend (e.g. 1420 to 1421)
  geom_segment(aes(x=0.7, y=1521, xend=1.3, yend=1521), size = 0.3) +
  annotate("text", x=1, y=1526, label="***")+
  geom_segment(aes(x=1.7, y=1580, xend=2.3, yend=1580), size = 0.3) +
  annotate("text", x=2, y=1585, label="*")+
  geom_segment(aes(x=1, y=1646, xend=2, yend=1646), size = 0.3) +
  annotate("text", x=1.5, y=1660, label="italic(n.s.)", parse="TRUE")+ # TRUE parse for italic
  # ------------------------------------------------------------------------------------------ #
  scale_x_discrete(labels=c("Experimental", "Control")) +
  scale_fill_manual(values=c("#ff80b0", "#9399ff", "#ff80b0", "#9399ff"),
                    labels=c("Block 1", "Block 2", "Block 3", "Block 4")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(panel.grid.major.y = element_line(color="#DBDBDB"),
        panel.grid.minor.y = element_line(color="#DBDBDB"),
        axis.line = element_line(size = 0.5))

# ------------------------------------------------------------- #
# 3.2.3.2 MSK-preferred Plot (grand mean, ci, individual mean)

ggplot() +
  geom_bar(data = p1rtG, aes(x=Btw, y=RT.M, fill=Block), 
           stat = "identity", position = position_dodge(1), colour = "black", size = 0.3)+
  geom_errorbar(data=p1rtG,
                aes(x = Btw, ymin = lower.ci, ymax = upper.ci, group = Block),
                position = position_dodge(1), width = 0.1, color = "black", size = 0.3, show.legend = FALSE) +
  labs(x = "Group", 
       y = "Reaction Times (ms)", 
       fill='Block') +
  coord_cartesian(ylim = c(0, 2000), clip = "on") +
  theme_bw(base_size = 15) +
  # add aterisk ------------------------------------------ change x, y, or label (*, **, n.s.) #
  # solution to inconsistent thickness: adjust y & yend (e.g. 1420 to 1421)
  #    geom_segment(aes(x=0.7, y=1421, xend=1.3, yend=1421), size = 0.3) +
  #    annotate("text", x=1, y=1426, label="***")+
  #    geom_segment(aes(x=1.7, y=1480, xend=2.3, yend=1480), size = 0.3) +
  #    annotate("text", x=2, y=1485, label="***")+
  #    geom_segment(aes(x=1, y=1526, xend=2, yend=1526), size = 0.3) +
  #    annotate("text", x=1.5, y=1531, label="***")+ # TRUE parse for italic
  # ------------------------------------------------------------------------------------------ #
  scale_x_discrete(labels=c("Experimental", "Control")) +
  scale_fill_manual(values=c("#ff80b0", "#9399ff", "#ff80b0", "#9399ff"),
                    labels=c("Block 1", "Block 2", "Block 3", "Block 4")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(panel.grid.major.y = element_line(color="#DBDBDB"),
        panel.grid.minor.y = element_line(color="#DBDBDB"),
        axis.line = element_line(size = 0.5),
        legend.position = c(0.8, 0.8), # legend position
        legend.key.size = unit(0.3,"cm"), # legend key color size
        legend.text = element_text(size = 13), # legend text size
        legend.title = element_text(size = 13), # legend title size
        legend.direction = "vertical") + # legend text direction
  guides(fill = guide_legend(title.position = "top")) # legend title position

# ----------------------------------------- #
# 3.2.4. journal of vision preferred plot

ggplot(p1rtG, aes(x=eyerep, y=RT, colour=locrep, group=locrep))+
  geom_line(aes(linetype=locrep, size = 0.9, colour=locrep),show.legend = FALSE) +
  geom_point(aes(size = 1),  show.legend = FALSE)+
  geom_errorbar(data=p1rtG,
                aes(x = eyerep, ymin = RT-ci, ymax = RT+ci, group = locrep),
                width = 0.1, size = 0.6) +
  labs(x = "Eye Repetition", 
       y = "Reaction Times (ms)", 
       colour='Location Repetition') +
  coord_cartesian(ylim = c(1000, 1600), clip = "on") +
  theme_bw(base_size = 15) +
  scale_x_discrete(labels=c("Repeated", "Unrepeated")) +
  scale_colour_manual(labels=c("Repeated", "Unrepeated"),
                      values=c("#ff80b0", "#9399ff")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(axis.line = element_line(size = 0.5),
        legend.position = c(0.3, 0.9), # legend position
        legend.key.size = unit(0.3,"cm"), # legend key color size
        legend.text = element_text(size = 13), # legend text size
        legend.title = element_text(size = 13), # legend title size
        legend.direction = "vertical")  # legend text direction
#  guides(fill = guide_legend(title.position = "top")) # legend title position


# -------------------------------------- #
# 3.2.5. Bar Plot (eyerep Only)

# if integer type is required for analysis or plot
# long format for each condition
# p1rtL.eyerep.Eye_Rep<-filter(p1rtL.eyerep, eyerep == "Eye_Rep")
# p1rtL.eyerep.Eye_Unrep<-filter(p1rtL.eyerep, eyerep == "Eye_Unrep")

# plot 1 (individual point)
ggplot(data=p1rtL.eyerep, aes(x=eyerep, y=RT, fill=eyerep)) +
  geom_bar(data = p1rtG.eyerep, stat = "identity", colour = "black", size = 0.3, position = position_dodge(1))+
  geom_point(position=position_dodge(0.5), color="gray80", size=1.8, show.legend = FALSE) +
  geom_segment(data=p1rtW.eyerep, inherit.aes = FALSE,
               aes(x=1, y=p1rtW.eyerep$Eye_Rep,
                   xend=2, yend=p1rtW.eyerep$Eye_Unrep),
               color="gray80") +
  geom_errorbar(data=p1rtG.eyerep,
                aes(x = eyerep, ymin = RT-ci, ymax = RT+ci),
                position = position_dodge(1), width = 0, color = "black", size = 0.3, show.legend = FALSE) +
  labs(x = "Eye Repetition", 
       y = "Reaction Times (ms)") +
  coord_cartesian(ylim = c(800, 1800), clip = "on") +
  theme_bw(base_size = 15) +
  scale_x_discrete(labels=c("Repeated", "Unrepeated")) +
  scale_fill_manual(values=c("#ff80b0", "#9399ff")) +
  theme(legend.position="none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(panel.grid.major.y = element_line(color="#DBDBDB"),
        panel.grid.minor.y = element_line(color="#DBDBDB"),
        axis.line = element_line(size = 0.5))


# plot 2 (MSK-prefered)
ggplot(data=p1rtL.eyerep, aes(x=eyerep, y=RT, fill=eyerep)) +
  geom_bar(data = p1rtG.eyerep, stat = "identity", colour = "black", size = 0.3, position = position_dodge(1))+
  geom_errorbar(data=p1rtG.eyerep,
                aes(x = eyerep, ymin = RT-ci, ymax = RT+ci),
                position = position_dodge(1), width = 0.1, color = "black", size = 0.3, show.legend = FALSE) +
  labs(x = "Eye Repetition", 
       y = "Reaction Times (ms)") +
  coord_cartesian(ylim = c(1000, 1500), clip = "on") +
  theme_bw(base_size = 15) +
  geom_segment(aes(x=1, y=1422, xend=2, yend=1422), size = 0.2) +
  annotate("text", x=1.5, y=1426, label="**")+
  scale_x_discrete(labels=c("Repeated", "Unrepeated")) +
  scale_fill_manual(values=c("#ff80b0", "#9399ff")) +
  theme(legend.position="none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(panel.grid.major.y = element_line(color="#DBDBDB"),
        panel.grid.minor.y = element_line(color="#DBDBDB"),
        axis.line = element_line(size = 0.5))


# ------------------------------------------------------------------ #
# One-way Plot ####
# ------------------------------------------------------------------ #

# Getting started with ggplot!
# full-data base (for individual mean plotting)
# set aes with long format data: x(i.v), y(d.v), fill(i.v)
ggplot(data=p1rtL.eyerep, aes(x=eyerep, y=RT, fill=eyerep)) +
  
  # >p1rtG.eyerep
  #   SN   eyerep       RT    SD
  #  <fct>  <fct>      <dbl>  <dbl>
  # 1 1     Eye_Rep    801.    NA
  # 2 1     Eye_Unrep  818.    NA
  # 3 2     Eye_Rep   1421.    NA
  # 4 2     Eye_Unrep 1471.    NA
  # 5 3     Eye_Rep   1567.    NA
  
  
# bar graph (you can use geom_violin or geom_line)
# set data with grand mean data
# to remove boundary line, delete colour/size (or adjust size)
geom_bar(data = p1rtG.eyerep, stat = "identity", colour = "black", size = 0.3, position = position_dodge(1))+
  
  # > p1rtG.eyerep
  # # A tibble: 2 x 5
  # eyerep        M    SD    ci    RT
  # <fct>     <dbl> <dbl> <dbl> <dbl>
  # 1 Eye_Rep   1278.  266.  31.8 1278.
  # 2 Eye_Unrep 1340.  270.  31.8 1340.
  
  
  # = NOT NECESSARY =
  # individual mean point (from long format data)
# set color and size
geom_point(position=position_dodge(0.5), color="gray80", size=1.8, show.legend = FALSE) +
  
  
  # = NOT NECESSARY =
  # connect individual mean for each condition using segment
  # set data with wide format data
  # length of aes data should be same with length of segment data
  # be careful with name of dataframe! see below 
  geom_segment(data=p1rtW.eyerep, inherit.aes = FALSE,
               aes(x=1, y=p1rtW.eyerep$Eye_Rep,
                   xend=2, yend=p1rtW.eyerep$Eye_Unrep),
               color="gray80") +
  
  #   > p1rtW.eyerep (wide format)
  # # A tibble: 17 x 4
  # SN      SD Eye_Rep Eye_Unrep
  # <fct> <dbl>   <dbl>     <dbl>
  #   1 1        NA    801.      818.
  # 2 2        NA   1421.     1471.
  # 3 3        NA   1567.     1643.
  # 4 4        NA   1094.     1122.
  # 5 5        NA   1391.     1363.
  # 6 6        NA   1309.     1300.

# error bar using grand mean & ci/se (see above to calculate ci)
# you can plot se by replacing ci with se (if there is se in grandmean data)
# set data & aes with ERrtG data: x(factor 1), y with ci/se(d.v), group(factor 2)
# remove upper/lower line with width = 0
geom_errorbar(data=p1rtG.eyerep,
              aes(x = eyerep, ymin = RT-ci, ymax = RT+ci),
              position = position_dodge(1), width = 0, color = "black", size = 0.3, show.legend = FALSE) +
  
  
  # label x, y (i.v., d.v.) 
  labs(x = "Eye Repetition", 
       y = "Reaction Times (ms)") +
  
  # gragh range: ylim = c(lower limit, upper limit)
  coord_cartesian(ylim = c(800, 1800), clip = "on") +
  
  # remove background and adjust size
  theme_bw(base_size = 15) +
  
  # = NOT NECESSARY =
  # add aterisk
  # change x, y, or label (*, **, n.s.)
  # modify thickness with size
  # solution to inconsistent thickness: adjust y & yend (e.g. 1420 to 1421)
  geom_segment(aes(x=1, y=1422, xend=2, yend=1422), size = 0.2) +
  annotate("text", x=1.5, y=1426, label="***")+
  #  annotate("text", x=1.5, y=1426, label="italic(n.s.)", parse="TRUE")+ # TRUE parse for italic
  
  # legend & color setting
  scale_x_discrete(labels=c("Repeated", "Unrepeated")) +
  scale_fill_manual(values=c("#ff80b0", "#9399ff")) +
  
  # grid & axis setting
  theme(legend.position="none",
        panel.grid.major = element_blank(), # remove grid (which is default in gglot)
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), # remove panel border line
        axis.ticks.x = element_blank()) + # remove axis ticks for x
  theme(panel.grid.major.y = element_line(color="#DBDBDB"),# add light-gray y grid (major: in this case, 200 ms)
        panel.grid.minor.y = element_line(color="#DBDBDB"),# (minor: 100 ms)
        axis.line = element_line(size = 0.5)) # left/bottom black axis line

# ------------------------------------------------------------------ #
# Two-way Plot (2 X 2) ####
# ------------------------------------------------------------------ #

# Getting started with ggplot!
# full-data base (for individual mean plotting)
# set aes with long format data: x(factor 1), y(d.v), fill(factor 2)
ggplot(data=p1rtL, aes(x=eyerep, y=RT, fill=locrep)) +
  
  #   > p1rtL (long format)
  # # A tibble: 68 x 4
  # SN   eyerep    locrep       RT
  # <fct> <fct>     <fct>     <dbl>
  #   1 1     Eye_Rep   Loc_Rep    758.
  # 2 1     Eye_Rep   Loc_Unrep  845.
  # 3 1     Eye_Unrep Loc_Rep    776.
  # 4 1     Eye_Unrep Loc_Unrep  859.
  # 5 2     Eye_Rep   Loc_Rep    987.
  # 6 2     Eye_Rep   Loc_Unrep 1854.


# bar graph (you can use geom_violin or geom_line)
# set data with grand mean data
# to remove boundary line, delete colour/size (or adjust size)
geom_bar(data = p1rtG, stat = "identity", position = position_dodge(1), colour = "black", size = 0.3)+
  
  #   > p1rtG (grand mean data with ci)
  # # A tibble: 4 x 6
  # eyerep    locrep        M    SD    ci    RT
  # <fct>     <fct>     <dbl> <dbl> <dbl> <dbl>
  #   1 Eye_Rep   Loc_Rep   1170.  200.  75.4 1170.
  # 2 Eye_Rep   Loc_Unrep 1385.  286.  71.2 1385.
  # 3 Eye_Unrep Loc_Rep   1271.  253.  78.5 1271.
  # 4 Eye_Unrep Loc_Unrep 1409.  275.  66.4 1409.
  
  
# = NOT NECESSARY =
# individual mean point (from long format data)
# set color and size
geom_point(position=position_dodge(0.5), color="gray80", size=1.8, show.legend = FALSE) +
  
  
  # = NOT NECESSARY =
  # connect individual mean for each condition using segment
  # set data with wide format data
  # length of aes data should be same with length of segment data
  # be careful with name of dataframe! see below
  geom_segment(data=filter(p1rtW, eyerep == "Eye_Rep"), inherit.aes = FALSE, # when factor 1 = 1
               aes(x=1-.12, y=filter(p1rtW, eyerep == "Eye_Rep")$Loc_Rep, # factor 1 = 1, factor 2 = 1 on left (left panel)
                   xend=1+.12, yend=filter(p1rtW, eyerep == "Eye_Rep")$Loc_Unrep), # factor 1 = 1, factor 2 = 2 on right (left panel)
               color="gray80") +
  geom_segment(data=filter(p1rtW, eyerep == "Eye_Unrep"), inherit.aes = FALSE, # when factor 1 = 2
               aes(x=2-.12, y=filter(p1rtW, eyerep == "Eye_Unrep")$Loc_Rep, # factor 1 = 2, factor 2 = 1 on left (right panel)
                   xend=2+.12, yend=filter(p1rtW, eyerep == "Eye_Unrep")$Loc_Unrep), # factor 1 = 2, factor 2 = 2 on right (right panel)
               color="gray80") +
  
  
  #   > p1rtW (wide format)
  # # A tibble: 34 x 4
  # SN   eyerep    Loc_Rep Loc_Unrep
  # <fct> <fct>       <dbl>     <dbl>
  #   1 1     Eye_Rep      758.      845.
  # 2 1     Eye_Unrep    776.      859.
  # 3 2     Eye_Rep      987.     1854.
  # 4 2     Eye_Unrep   1289.     1654.
  # 5 3     Eye_Rep     1553.     1581.


# error bar using grand mean & ci/se (see above to calculate ci)
# you can plot se by replacing ci with se (if there is se in grandmean data)
# set data & aes with ERrtG data: x(factor 1), y with ci/se(d.v), group(factor 2)
# remove upper/lower line with width = 0
geom_errorbar(data=p1rtG,
              aes(x = eyerep, ymin = RT-ci, ymax = RT+ci, group = locrep),
              position = position_dodge(1), width = 0.1, color = "black", size = 0.3, show.legend = FALSE) +
  
  
  # label x, y, fill (factor 1, d.v., factor 2)  
  labs(x = "Eye Repetition", 
       y = "Reaction Times (ms)", 
       fill='Location Repetition') +
  
  # gragh range: ylim = c(lower limit, upper limit)
  coord_cartesian(ylim = c(1000, 1600), clip = "on") +
  
  # remove background and adjust size
  theme_bw(base_size = 15) + 
  
  
  # = NOT NECESSARY =
  # add aterisk
  # change x, y, or label (*, **, n.s.)
  # solution to inconsistent thickness: adjust y & yend (e.g. 1420 to 1421)
  
  geom_segment(aes(x=0.7, y=1421, xend=1.3, yend=1421), size = 0.3) +
  annotate("text", x=1, y=1426, label="***")+
  
  geom_segment(aes(x=1.7, y=1480, xend=2.3, yend=1480), size = 0.3) +
  annotate("text", x=2, y=1485, label="*")+
  
  geom_segment(aes(x=1, y=1546, xend=2, yend=1546), size = 0.3) +
  annotate("text", x=1.5, y=1560, label="italic(n.s.)", parse="TRUE")+ # TRUE parse for italic
  
  
  scale_x_discrete(labels=c("Repeated", "Unrepeated")) +
  
  
  # legend for factor 2 (and color setting)
  scale_fill_manual(values=c("#ff80b0", "#9399ff"),
                    labels=c("Repeated", "Unrepeated")) +
  
  # grid & axis
  theme(panel.grid.major = element_blank(), # remove grid (which is default in gglot)
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), # remove panel border line
        axis.ticks.x = element_blank()) + # remove axis ticks for x
  
  theme(panel.grid.major.y = element_line(color="#DBDBDB"), # add light-gray y grid (major: in this case, 200 ms)
        panel.grid.minor.y = element_line(color="#DBDBDB"), # (minor: 100 ms)
        axis.line = element_line(size = 0.5)) # left/bottom black axis line

sessionInfo()


# ------------------------------------------------------------------ #
# ------------------------------------------------------------------ #
# Appendix 2. data transformation ####
# ------------------------------------------------------------------ #
# ------------------------------------------------------------------ #


# 참고. transformation
# 데이터 변환 방법 (R in Action 8.5.2를 참조한다. p199)
# 데이터가 normality 가정을 충족하지 않는 경우, 변수의 특성을 살펴보고 변환한다. 
# 일반적인 데이터 변환은 변수 y를 y^λ로 바꾸어 수행한다. 

# 대표적인 데이터 변환 방법
#     (Y^λ)    λ |   -2   |  -1   |    -0.5    |    0    |   0.5    |  1  |   2   |
# Transformation |  1/Y^2 |  1/Y  |  1/sqrt(Y) |  log(Y) |  sqrt(Y) |  Y  |  Y^2  |

# 어떻게 변환할지는 변환할 데이터의 특성을 살펴보고 결정한다. 
# car::powerTransform() function : 변수 Y^λ를 정규화하는 데에 가장 좋은 λ을 MLE로 찾아준다. 
# car::powerTransform(cp1$RT) # p1$lambda : Est.Power -0.6816, 


# ------------------------------------------------------------------ #
# ------------------------------------------------------------------ #
# Appendix 3. Testing the Assumptions of Multilevel Models ####
# ------------------------------------------------------------------ #
# ------------------------------------------------------------------ #

load("p1acc_lmer_mixed.Rdata")
load("p1acc_glmer_mixed.Rdata")
load("p1rt_lmer_mixed.Rdata")

p1.acc.lmixed
p1.acc.glmixed
p1.rt.lmixed

#install.packages("lme4")
#install.packages("ggplot2")
#install.packages("HLMdiag")
#install.packages("DHARMa")
#install.packages("car")

#load the libraries so you have access to them in your workflow

library("lme4")
library("ggplot2")
library("HLMdiag")
library("DHARMa")
library("car") #for the Levene test which we will not discuss here
library("Matrix")
        
# Assumption 1 - Linearity
Plot.Model.F.Linearity<-plot(resid(p1.rt.lmixed$full_model),fitted(p1.rt.lmixed$full_model)) 

# Assumption 2 - Homogeneity of Variance
ttp1$Model.F.Res<- residuals(p1.rt.lmixed$full_model) #extracts the residuals and places them in a new column in our original data table
ttp1$Abs.Model.F.Res <-abs(ttp1$Model.F.Res) #creates a new column with the absolute value of the residuals
ttp1$Model.F.Res2 <- ttp1$Model.F.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.Model.F <- lm(Model.F.Res2 ~ SN, data=ttp1) #ANOVA of the squared residuals
anova(Levene.Model.F) #displays the results

Plot.Model.F <- plot(p1.rt.lmixed$full_model) #creates a fitted vs residual plot
Plot.Model.F

# Assumption 3 - The residuals of the model are normally distributed.
lattice::qqmath(p1.rt.lmixed$full_model, id=.01)
hist(resid(p1.rt.lmixed$full_model))
describe(resid(p1.rt.lmixed$full_model))

