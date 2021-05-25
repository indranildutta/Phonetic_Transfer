formant_data_5 <- read.csv("formant_data_5.csv")
attach(formant_data_5)
detach(formant_data_5)

library(lattice)
library(plyr)
library(phonR)
library(lme4)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stargazer)

?gather
normF1 <- with(formant_data_5,normLobanov(cbind(F1_5,F1_15,F1_25,F1_35,F1_45)),group=Subject)
normF1 <- as.data.frame(normF1)

normF2 <- with(formant_data_5,normLobanov(cbind(F2_5,F2_15,F2_25,F2_35,F2_45)),group=Subject)
normF2 <- as.data.frame(normF2)


with(formant_data_5, plotVowels(normF1$F1_15, normF2$F2_15, Vowel, group = Context, plot.tokens = FALSE, 
              plot.means = TRUE , var.sty.by = Vowel,var.col.by = Context, pch.means = Vowel, cex.tokens = 0.8,
              cex.means = 0.8, alpha.means = 0.3,alpha.tokens = 0.3,ellipse.line = TRUE,  ellipse.fill = TRUE,
              fill.opacity = 0.1, pretty = TRUE))



## bengali baseline data
bengali <- read.csv("anti.csv", stringsAsFactors = TRUE, header = TRUE)
attach(bengali)
detach(bengali)
bengaliAE <- subset (bengali, V1=="A"|V1=="E")
attach(bengaliAE)
detach(bengaliAE)
bengaliAE_bd


#####

#subset by context
contexte <- formant_data_5[formant_data_5$Context=="e",]
contexte <- droplevels(contexte)

attach(contexte)

enormF1 <- with(contexte,normLobanov(cbind(F1_5,F1_15,F1_25,F1_35,F1_45)),group=Subject)
enormF1 <- as.data.frame(enormF1)
boxplot(enormF1$F1_25 ~ Vowel*Subject, las = 2, ylab = "Lobanov-normalized F1")

detach(contexte)

contexte_vt <- formant_data_5[formant_data_5$Context=="e" & formant_data_5$Vowel=="\\vt",]
contexte_vt <- droplevels(contexte_vt)

evt_normF1 <- with(contexte_vt,normLobanov(cbind(F1_5,F1_15,F1_25,F1_35,F1_45)),group=Subject)
evt_normF1 <- as.data.frame(evt_normF1)

evt_normF2 <- with(contexte_vt,normLobanov(cbind(F2_5,F2_15,F2_25,F2_35,F2_45)),group=Subject)
evt_normF2 <- as.data.frame(evt_normF2)


bengaliA <- subset (bengali, V1=="A")
ben_f1A <- with(bengaliA, mean(bengaliA$F1_V1_T55))
ben_f2A <- with(bengaliA, mean(bengaliA$F2_V1_T55))



eng_f1vt <- mean(evt_normF1$F1_45, na.rm = TRUE)
eng_f2vt <- mean(evt_normF2$F2_45, na.rm = TRUE)



#subset by task

taskp <- formant_data_5[formant_data_5$Task=="p",]
taskp <- droplevels(taskp)

attach(taskp)
pnormF1 <- with(taskp,normLobanov(cbind(F1_5,F1_15,F1_25,F1_35,F1_45)),group=Subject)
pnormF1 <- as.data.frame(pnormF1)

p_phon_acco.model = lmer(pnormF1$F1_5 ~ Gender + Vowel*Context + (1+Context|Word), data=taskp, REML=FALSE)
p_null.model = lmer(pnormF1$F1_5 ~ Gender + (1+Context|Word), data=taskp, REML=FALSE)
summary(p_phon_acco.model)
summary(p_null.model)
anova(p_phon_acco.model,p_null.model) 
boxplot(pnormF1$F1_5~Context*Vowel)
detach(taskp)

#### 




## plots for writeup

# subject-wise F1 in English context
attach(contexte)
ggplot(contexte, aes(x = Subject, y = enormF1$F1_25, fill = Vowel)) + ylab("Normalized F1") +geom_boxplot()
detach(contexte)


# vowels: unilingual English, mixed, and baseline Bengali
xlim <- c(3,-3)
ylim = c(3,-3)

remapping <- c(A = "a:", E = "\\ae")
bengaliAE$unicodevowel <- remapping[as.character(bengaliAE$V1)]
with(bengaliAE,plotVowels(F1_V1_T55,F2_V1_T55, unicodevowel, plot.tokens = F, plot.means = TRUE,
                        pch.tokens = V1, pch.means = unicodevowel, cex.tokens = 0.8, var.col.by = V1,
                        cex.means = 0.8, alpha.means = 0.8,alpha.tokens = 0.1,ellipse.line = F,  
                        ellipse.fill = T, pretty = TRUE, xlim = xlim, ylim = ylim, axes=FALSE))
par(new = TRUE)

with(formant_data_5, plotVowels(normF1$F1_45, normF2$F2_45, Vowel, group = Context, plot.tokens = FALSE, 
                                plot.means = TRUE , var.sty.by = Context, pch.means = Vowel, cex.tokens = 0.8,
                                cex.means = 0.8, alpha.means = 0.5,alpha.tokens = 0.3,ellipse.line = T,  ellipse.fill = T,
                                fill.opacity = 0.1, pretty = TRUE, xlim = xlim, ylim = ylim))




## main model
phon_acco.model = lmer(normF1$F1_25 ~ Gender + Context*Vowel + Task + (1+Task+Context|Word) + 
                         (1+Task+Context*Vowel|Subject), data=formant_data_5, REML=FALSE)
null.model = lmer(normF1$F1_25 ~ Gender + Context*Vowel + Task + (1+Task+Context|Word) + 
                    (1+Task+Context+Vowel|Subject), data=formant_data_5, REML=FALSE)
summary(phon_acco.model)
anova(phon_acco.model,null.model) 
coef(phon_acco.model)
boxplot(normF1$F1_25~ Vowel*Subject,las=2)





## output for writeup: F1
phon_acco.model5 = lmer(normF1$F1_5 ~ Gender + Context*Vowel + Task + (1+Task+Context|Word) + 
                         (1+Task+Context*Vowel|Subject), data=formant_data_5, REML=FALSE)
null.model5 = lmer(normF1$F1_5 ~ Gender + Task + (1+Task+Context|Word) + 
                     (1+Task+Context*Vowel|Subject), data=formant_data_5, REML=FALSE)


phon_acco.model15 = lmer(normF1$F1_15 ~ Gender + Context*Vowel + Task + (1+Task+Context|Word) + 
                          (1+Task+Context*Vowel|Subject), data=formant_data_5, REML=FALSE)
null.model15 = lmer(normF1$F1_15 ~ Gender + Task + (1+Task+Context|Word) + 
                     (1+Task+Context*Vowel|Subject), data=formant_data_5, REML=FALSE)


phon_acco.model25 = lmer(normF1$F1_25 ~ Gender + Context*Vowel + Task + (1+Task+Context|Word) + 
                          (1+Task+Context*Vowel|Subject), data=formant_data_5, REML=FALSE)
null.model25 = lmer(normF1$F1_25 ~ Gender + Task + (1+Task+Context|Word) + 
                      (1+Task+Context*Vowel|Subject), data=formant_data_5, REML=FALSE)


phon_acco.model35 = lmer(normF1$F1_35 ~ Gender + Context*Vowel + Task + (1+Task+Context|Word) + 
                          (1+Task+Context*Vowel|Subject), data=formant_data_5, REML=FALSE)
null.model35 = lmer(normF1$F1_35 ~ Gender + Task + (1+Task+Context|Word) + 
                      (1+Task+Context*Vowel|Subject), data=formant_data_5, REML=FALSE)


phon_acco.model45 = lmer(normF1$F1_45 ~ Gender + Context*Vowel + Task + (1+Task+Context|Word) + 
                          (1+Task+Context*Vowel|Subject), data=formant_data_5, REML=FALSE)
null.model45 = lmer(normF1$F1_45 ~ Gender + Task + (1+Task+Context|Word) + 
                      (1+Task+Context*Vowel|Subject), data=formant_data_5, REML=FALSE)

stargazer(phon_acco.model5,phon_acco.model15,phon_acco.model25,phon_acco.model35,phon_acco.model45, 
          order = c("Constant","Gendermale"),out = 'stargazer_F1_final.tex')


# ANOVAs:
af15 <- anova(phon_acco.model5,null.model5)
af115 <- anova(phon_acco.model15,null.model15)
af125 <- anova(phon_acco.model25,null.model25)
af135 <- anova(phon_acco.model35,null.model35)
af145 <- anova(phon_acco.model45,null.model45)
  
stargazer(af15,af115,af125,af135,af145, out = 'anova_f1.tex')


## output for writeup: F2
f2phon_acco.model5 = lmer(normF2$F2_5 ~ Gender + Context*Vowel + Task + (1+Task+Context|Word) + 
                          (1+Task+Context*Vowel|Subject), data=formant_data_5, REML=FALSE)
f2null.model5 = lmer(normF2$F2_5 ~ Gender + Task + (1+Task+Context|Word) + 
                     (1+Task+Context*Vowel|Subject), data=formant_data_5, REML=FALSE)


f2phon_acco.model15 = lmer(normF2$F2_15 ~ Gender + Context*Vowel + Task + (1+Task+Context|Word) + 
                           (1+Task+Context*Vowel|Subject), data=formant_data_5, REML=FALSE)
f2null.model15 = lmer(normF2$F2_15 ~ Gender + Task + (1+Task+Context|Word) + 
                      (1+Task+Context*Vowel|Subject), data=formant_data_5, REML=FALSE)

f2phon_acco.model25 = lmer(normF2$F2_25 ~ Gender + Context*Vowel + Task + (1+Task+Context|Word) + 
                           (1+Task+Context*Vowel|Subject), data=formant_data_5, REML=FALSE)
f2null.model25 = lmer(normF2$F2_25 ~ Gender + Task + (1+Task+Context|Word) + 
                      (1+Task+Context*Vowel|Subject), data=formant_data_5, REML=FALSE)


f2phon_acco.model35 = lmer(normF2$F2_35 ~ Gender + Context*Vowel + Task + (1+Task+Context|Word) + 
                           (1+Task+Context*Vowel|Subject), data=formant_data_5, REML=FALSE)
f2null.model35 = lmer(normF2$F2_35 ~ Gender + Task + (1+Task+Context|Word) + 
                      (1+Task+Context*Vowel|Subject), data=formant_data_5, REML=FALSE)


f2phon_acco.model45 = lmer(normF2$F2_45 ~ Gender + Context*Vowel + Task + (1+Task+Context|Word) + 
                           (1+Task+Context*Vowel|Subject), data=formant_data_5, REML=FALSE)
f2null.model45 = lmer(normF2$F2_45 ~ Gender + Task + (1+Task+Context|Word) + 
                      (1+Task+Context*Vowel|Subject), data=formant_data_5, REML=FALSE)

stargazer(f2phon_acco.model5, f2phon_acco.model15, f2phon_acco.model25, f2phon_acco.model35, f2phon_acco.model45, 
          order = c("Constant","Gendermale","Vowel\vt","Contexte","Tasks"),out = 'stargazer_F2_final.tex')


# ANOVAs:
af25 <- anova(phon_acco.model5,null.model5)
af215 <- anova(phon_acco.model15,null.model15)
af225 <- anova(phon_acco.model25,null.model25)
af235 <- anova(phon_acco.model35,null.model35)
af245 <- anova(phon_acco.model45,null.model45)

stargazer(af25,af215,af225,af235,af245)






# par(mfrow = c(1, 1))
# with(bengali_phon_acco_raw, plotVowels(normF1, normF2, plot.tokens = TRUE, pch.tokens = Vowel, 
#                     plot.means = TRUE, pch.means = Context, 
#                       ellipse.line = TRUE, pretty = TRUE))
# boxplot(normF1~context*vowel, data=bengali_phon_acco_raw)

# vowel_vt <- bengali_phon_acco_raw %>%
#   filter(Vowel == "ʌ")
# task_p <- subset(bengali_phon_acco_raw, (vowel=="ʌ"))
# p <- ggplot(bengali_phon_acco_raw, aes(x=Context, y=normF1)) + 
#   geom_boxplot() + xlab("Language context: B (Bengali) and E (English) and Tasks: S (Sentence) and P (Picture naming)") + ylab("Lobanov Normalized F1 at 5% of the vowel")
# p + facet_grid(Vowel~Task)
# 
# p <- ggplot(bengali_phon_acco_raw, aes(x=Context, y=Vowel_Duration)) + 
#   geom_boxplot() + xlab("Language context: B (Bengali) and E (English) and Tasks: S (Sentence) and P (Picture naming)") + ylab("Vowel duration (in ms)")
# p + facet_grid(Vowel~Task)
# 
# ggplot(bengali_phon_acco_raw, aes(x=context, y=normF1, fill=vowel))
# geom_boxplot()


