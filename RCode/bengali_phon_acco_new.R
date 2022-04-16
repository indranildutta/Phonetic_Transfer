# analysis code for phonetic_transfer
# file paths relative to R project environment (root = phonetic_transfer)

library(tidyverse)
library(phonR)
library(lme4)
library(lmerTest)
library(stargazer)


formant_data_5 <- read.csv("RCode/formant_data_5.csv", check.names = TRUE)%>%
  mutate_if(is.character,as.factor)
str(formant_data_5)

attach(formant_data_5)



# pool measures from all 5 points in the vowel, then from that-- calculate the average F1 for each speaker, and normalize all data points from that speaker wrt that.  
normF1 <- with(formant_data_5,normLobanov(cbind(F1_5,F1_15,F1_25,F1_35,F1_45)),group=Subject)

normF2 <- with(formant_data_5,normLobanov(cbind(F2_5,F2_15,F2_25,F2_35,F2_45)),group=Subject)


df_lobanov <- data.frame(Subject,Gender,Task,Word,Context,Vowel,normF1,normF2)


detach(formant_data_5)
# with(formant_data_5, plotVowels(normF1$F1_15, normF2$F2_15, Vowel, group = Context, plot.tokens = FALSE, 
#               plot.means = TRUE , var.sty.by = Vowel,var.col.by = Context, pch.means = Vowel, cex.tokens = 0.8,
#               cex.means = 0.8, alpha.means = 0.3,alpha.tokens = 0.3,ellipse.line = TRUE,  ellipse.fill = TRUE,
#               fill.opacity = 0.1, pretty = TRUE))



## bengali baseline data
bengali <- read.csv("RCode/baseline_bengali.csv", stringsAsFactors = TRUE, header = TRUE)
attach(bengali)
detach(bengali)
bengaliAE <- subset (bengali, V1=="A"|V1=="E")
attach(bengaliAE)
# detach(bengaliAE)



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
remapping <- c(`\\ae` = "æ", `\\vt` = "ʌ")
contexte$unicodevowel <- remapping[as.character(contexte$Vowel)]
ggplot(contexte, aes(x = Subject, y = enormF1$F1_25, fill = unicodevowel)) + ylab("Normalized F1") +scale_fill_discrete(name="Vowel")+ geom_boxplot()
detach(contexte)


# vowels: unilingual English, mixed, and baseline Bengali
xlim <- c(2.5,-2.5)
ylim = c(3.5,-2.5)

remapping <- c(A = "a:", E = "æ")
bengaliAE$unicodevowel <- remapping[as.character(bengaliAE$V1)]
with(bengaliAE,plotVowels(F1_V1_T55,F2_V1_T55, unicodevowel, plot.tokens = F, plot.means = TRUE,
                        pch.tokens = V1, pch.means = unicodevowel, cex.tokens = 0.8, var.col.by = unicodevowel,
                        cex.means = 1, alpha.means = 0.8,alpha.tokens = 0.1,ellipse.line = F,  
                        ellipse.fill = T, pretty = TRUE, xlim = xlim, ylim = ylim, legend.kwd="bottomleft", legend.args = list(legend=c("baseline æ","baseline a:")), axes=FALSE))
par(new = TRUE)
remapping <- c(`\\ae` = "æ", `\\vt` = "ʌ")
formant_data_5$unicodevowel <- remapping[as.character(formant_data_5$Vowel)]
with(formant_data_5, plotVowels(normF1$F1_45, normF2$F2_45, Vowel, group = Context, plot.tokens = FALSE, 
                                plot.means = TRUE , var.sty.by = Context, pch.means = unicodevowel, cex.tokens = 0.8,
                                cex.means = 1, alpha.means = 0.5,alpha.tokens = 0.3,ellipse.line = T,  ellipse.fill = T,
                                fill.opacity = 0.1, pretty = TRUE, xlim = xlim, ylim = ylim, legend.kwd = "bottomright", legend.args = list(legend=c("unilingual","mixed"))))




## main model

# mean-centered and scaled data: 

df_lobanov.CS <- df_lobanov %>% mutate_if(is.numeric,scale)


phon_acco.model = lmer(F1_25 ~ Gender + Context*Vowel + Task + (1+Task+Context|Word) + 
                         (1+Task+Context*Vowel|Subject), data=df_lobanov.CS, REML=FALSE)

null.model = lmer(F1_25 ~ Gender + Context*Vowel + Task + (1+Task+Context|Word) + 
                    (1+Task+Context+Vowel|Subject), data=df_lobanov.CS, REML=FALSE)
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


