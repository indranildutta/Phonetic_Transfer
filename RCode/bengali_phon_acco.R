attach(bengali_phon_acco_raw)
library(lattice)
library(plyr)
library(phonR)
library(lme4)
library(ggplot2)
library(dplyr)

normF1 <- with(bengali_phon_acco_raw, normLobanov(F1_5), group=Subject)
normF2 <- with(bengali_phon_acco_raw, normLobanov(F2_5), group=Subject)
normF1mid <- with(bengali_phon_acco_raw, normLobanov(F1_mid), group=Subject)
normF2mid <- with(bengali_phon_acco_raw, normLobanov(F2_mid), group=Subject)

with(bengali_phon_acco_raw, plotVowels(normF1, normF2, group=Vowel, pch.tokens = Vowel))
par(mfrow = c(1, 1))
with(bengali_phon_acco_raw, plotVowels(normF1, normF2, plot.tokens = TRUE, pch.tokens = Vowel, 
                    plot.means = TRUE, pch.means = Context, 
                      ellipse.line = TRUE, pretty = TRUE))
boxplot(normF1~Context*Vowel, data=bengali_phon_acco_raw)

vowel_vt <- bengali_phon_acco_raw %>%
  filter(Vowel == "ʌ")
task_p <- subset(bengali_phon_acco_raw, (vowel=="ʌ"))
p <- ggplot(bengali_phon_acco_raw, aes(x=Context, y=normF1)) + 
  geom_boxplot() + xlab("Language context: B (Bengali) and E (English) and Tasks: S (Sentence) and P (Picture naming)") + ylab("Lobanov Normalized F1 at 5% of the vowel")
p + facet_grid(Vowel~Task)

p <- ggplot(bengali_phon_acco_raw, aes(x=Context, y=normF1)) + 
  geom_boxplot() + xlab("Language context: B (Bengali) and E (English) and Tasks: S (Sentence) and P (Picture naming)") + ylab("Normalized F1")
p + facet_grid(Vowel~Task)

ggplot(bengali_phon_acco_raw, aes(x=context, y=normF1, fill=vowel))
geom_boxplot()

phon_acco.model = lmer(normF1mid ~ Vowel + Task + Context + Gender + (1+Context|Word), data=bengali_phon_acco_raw, REML=FALSE)
null.model = lmer(normF1mid ~ Vowel + Task  + Gender + (1+Context|Word), data=bengali_phon_acco_raw, REML=FALSE)
summary(phon_acco.model)
anova(phon_acco.model,null.model) 

phon_acco.model = lmer(Vowel_Duration ~ Vowel + Task + Context + Gender + (1+Context|Word), data=bengali_phon_acco_raw, REML=FALSE)
null.model = lmer(Vowel_Duration  ~  Vowel + Task + Gender + (1+Context|Word), data=bengali_phon_acco_raw, REML=FALSE)
summary(phon_acco.model)
anova(phon_acco.model,null.model) 

vt_p <- subset(phon_accomm_data_female, (Vowel== "ʌ" & Task=="p"))

# #Summary stats for AF1 for V1
# normAF1_V1 <- data.frame(normAF1V1, lary_cont)
# normAF1_V1 %>%
#   group_by(lary_cont) %>%
#   summarize(mean(normAF1V1, na.rm = TRUE), sd(normAF1V1, na.rm = TRUE))
# 
# #Summary stats for AF1 for V2
# normAF1_V2 <- data.frame(normAF1V2, lary_cont)
# normAF1_V2 %>%
#   group_by(lary_cont) %>%
#   summarize(mean(normAF1V2, na.rm = TRUE), sd(normAF1V2, na.rm = TRUE))
# 
# normF1_V1_T10 <- with(bangla_geminates_formant_values_consolidated, normLobanov(V1_F1_95), group=subject)
# normF2_V1_T10 <- with(bangla_geminates_formant_values_consolidated, normLobanov(V1_F2_95), group=subject)
# normF1_V2_T1 <- with(bangla_geminates_formant_values_consolidated, normLobanov(V2_F1_5), group=subject)
# normF2_V2_T1 <- with(bangla_geminates_formant_values_consolidated, normLobanov(V2_F2_5), group=subject)
# 
# 
# #p <- ggplot(raw_specint_measures, aes(normf0, normOQ)) + geom_point(aes(colour = factor(lary_con))) + geom_smooth(method=lm)
# #p + facet_grid(lary_con ~ gender)
# require(gridExtra)
# library(grid)
# 
# plot1 <- qplot(1)
# plot2 <- qplot(1)
# library(ggsignif)
# 
# #V A1-A2 Plots
# #V1
# f1<-ggplot(bangla_geminates_formant_values_consolidated, aes(x=lary_cont, y=normAF1V1)) + geom_boxplot(aes(fill = factor(lary_cont))) + ylab("Anti-symmetric Fourier Coefficent - a1")  + ylim(-2,2.5) + 
#   theme(axis.text.x = element_text(size=18, face="bold"),axis.text.y = element_text(face="bold", size=18),
#         axis.title.x=element_blank(), axis.title.y = element_text(size=20, face="bold"))
# f1=f1 + theme(panel.background = element_rect(fill = "white",colour = NA), legend.position="none")
# f2<-ggplot(bangla_geminates_formant_values_consolidated, aes(x=lary_cont, y=normAF2V1)) + geom_boxplot(aes(fill = factor(lary_cont)))  + ylab("a2") + ylim(-2,2.5) +
#   theme(axis.text.x = element_text(size=18, face="bold"),axis.text.y = element_text(face="bold", size=18), 
#         axis.title.x=element_blank(), axis.title.y = element_text(size=22, face="bold"))
# f2=f2 + theme(panel.background = element_rect(fill = "white",colour = NA), legend.position="none")
# grid.arrange(f1, f2, ncol=2, top = textGrob("V1",gp=gpar(fontsize=24,font=2)))
# #V2
# f1<-ggplot(bangla_geminates_formant_values_consolidated, aes(x=lary_cont, y=normAF1V2)) + geom_boxplot(aes(fill = factor(lary_cont))) + ylab("Anti-symmetric Fourier Coefficent - a1")  + ylim(-2,2) +
#   theme(axis.text.x = element_text(size=18, face="bold"),axis.text.y = element_text(face="bold", size=18),
#         axis.title.x=element_blank(), axis.title.y = element_text(size=20, face="bold"))
# f1= f1 + theme(panel.background = element_rect(fill = "white",colour = NA), legend.position="none")
# 
# f2<-ggplot(bangla_geminates_formant_values_consolidated, aes(x=lary_cont, y=normAF2V2)) + geom_boxplot(aes(fill = factor(lary_cont))) + ylab("a2") + ylim(-2,2) +
#   theme(axis.text.x = element_text(size=18, face="bold"),axis.text.y = element_text(face="bold", size=18),
#         axis.title.x=element_blank(), axis.title.y = element_text(size=22, face="bold"))
# f2= f2 + theme(panel.background = element_rect(fill = "white",colour = NA), legend.position="none")
# grid.arrange(f1, f2, ncol=2, top = textGrob("V2",gp=gpar(fontsize=24,font=2)))
# 
# 
# p + theme_bw()
# #lmer
# bangla_geminates_formant_values_consolidated$V1 <- factor(bangla_geminates_formant_values_consolidated$V1, c("i", "a", "ɔ", "o", "u"))
# bangla_geminates_formant_values_consolidated$V2 <- factor(bangla_geminates_formant_values_consolidated$V2, c("i", "a", "e", "ɔ", "o", "u"))
# 
# par(mfrow = c(1, 2))
# xlim=c(3,-2.5)
# ylim=c(3,-3)
# ggplot(bangla_geminates_formant_values_consolidated, aes(x=V1, y=normAF1V1)) + ylim(-3,3) + ylab("Coefficient of the 1st anti-symmetric Fourier component") + xlab("Vowel 1")+ geom_boxplot(aes(fill = factor(lary_cont))) + scale_fill_discrete(name="Laryngeal \nsetting")
# 
# ggplot(bangla_geminates_formant_values_consolidated, aes(x=V2, y=normAF2V1)) + ylim(-3,3) + ylab("Coefficient of the 1st anti-symmetric Fourier component") + xlab("Vowel 2")+ geom_boxplot(aes(fill = factor(lary_cont))) + scale_fill_discrete(name="Laryngeal \nsetting")
# 
# 
# #F1V1
# bangla_AF.model = lmer(normAF1V1 ~ lary_cont + poa + V1 + (1+lary_cont|item) + (1+lary_cont|iterations), data=bangla_geminates_formant_values_consolidated,REML=FALSE)
# null.model = lmer(normAF1V1 ~ poa+ V1 + (1+lary_cont|item) + (1+lary_cont|iterations), data=bangla_geminates_formant_values_consolidated,REML=FALSE)
# summary(bangla_AF.model)
# anova(bangla_AF.model,null.model)
# 
# #F2V1
# bangla_AF.model = lmer(normAF2V1 ~ lary_cont + V1 + poa + (1+lary_cont|subject) + (1+lary_cont|item) + (1+lary_cont|iterations), data=bangla_geminates_formant_values_consolidated,REML=FALSE)
# null.model = lmer(normAF2V1 ~  V1 + poa + (1+lary_cont|subject) + (1+lary_cont|item) + (1+lary_cont|iterations), data=bangla_geminates_formant_values_consolidated,REML=FALSE)
# summary(bangla_AF.model)
# anova(bangla_AF.model,null.model)
# 
# #F1V2
# bangla_AF.model = lmer(normAF1V2 ~ lary_cont  + V2 + poa + (1+lary_cont|subject) + (1+lary_cont|item) + (1+lary_cont|iterations), data=bangla_geminates_formant_values_consolidated,REML=FALSE)
# null.model = lmer(normAF1V2 ~  V2+ poa  + (1+lary_cont|subject) + (1+lary_cont|item) + (1+lary_cont|iterations), data=bangla_geminates_formant_values_consolidated,REML=FALSE)
# summary(bangla_AF.model)
# anova(bangla_AF.model,null.model)
# 
# #F2V2
# bangla_AF.model = lmer(normAF2V2 ~ lary_cont + V2 + poa  + (1+lary_cont|subject) + (1+lary_cont|item) + (1+lary_cont|iterations), data=bangla_geminates_formant_values_consolidated,REML=FALSE)
# null.model = lmer(normAF2V2 ~  V2 + poa + (1+lary_cont|subject) + (1+lary_cont|item) + (1+lary_cont|iterations), data=bangla_geminates_formant_values_consolidated,REML=FALSE)
# summary(bangla_AF.model)
# anova(bangla_AF.model,null.model)
# 
# # poly.area.V1T10 <- with(bangla_geminates_formant_values_consolidated, vowelMeansPolygonArea(normF1_V1_T10, normF2_V1_T10, V1,
# #                                                           poly.order = c("i", "A", "O", "oh", "u")
# #                                                           ,group = lary_cont) )
# # poly.area.V2T1 <- with(bangla_geminates_formant_values_consolidated, vowelMeansPolygonArea(normF1_V2_T1, normF2_V2_T1, V2,
# #                                                          poly.order = c("i", "e", "A", "oh", "u"),
# #                                                          group = lary_cont) )
# #rbind(poly.area.V1T10, poly.area.V2T1)
# 
# par(mfrow = c(1, 2))
# xlim=c(1.5,-1.5)
# ylim=c(1.25,-1)
# with(bangla_geminates_formant_values_consolidated, plotVowels(normF1_V1_T10, normF2_V1_T10, V1, 
#                                                               group = lary_cont, plot.tokens = FALSE, plot.means = TRUE, pch.means = V1, cex.means = 2, xlim=xlim, ylim=ylim,
#                                                               var.col.by = lary_cont, legend.kwd="bottomright", ellipse.fill = FALSE, poly.fill=TRUE, poly.line = FALSE, poly.order = c("i", "a", "ɔ", "o", "u"), pretty = FALSE))
# 
# with(bangla_geminates_formant_values_consolidated, plotVowels(normF1_V2_T1, normF2_V2_T1, V2, 
#                                                               group = lary_cont,plot.tokens = FALSE, plot.means = TRUE, pch.means = V2, cex.means = 2, xlim=xlim, ylim=ylim,
#                                                               var.col.by = lary_cont, legend.kwd="bottomright", ellipse.fill = FALSE, poly.fill=TRUE, poly.line = FALSE, poly.order = c("i", "e", "a", "o", "u"), pretty = FALSE))
# 
# 
# #######################closure durations####################################
# attach(compiled_closure_durations)
# vel <- subset(compiled_closure_durations, (poa== "vel" & lary_cont == "Voiced"))
# ret <- subset(compiled_closure_durations, (poa== "ret" & lary_cont == "Voiced"))
# den <- subset(compiled_closure_durations, (poa== "den" & lary_cont == "Voiced"))
# vel_m <- mean(vel$cd)
# ret_m <- mean(ret$cd)
# den_m <- mean(den$cd)
# 
# 
# 
# 
# 
# bangla_cd.model = lmer(cd ~ lary_cont*poa*sex  + (1+lary_cont|subject) + (1+lary_cont|item) + (1+lary_cont|iteration), data=compiled_closure_durations,REML=FALSE)
# null.model = lmer(cd ~ poa*sex + (1+lary_cont|subject) + (1+lary_cont|item) + (1+lary_cont|iteration), data=compiled_closure_durations,REML=FALSE)
# summary(bangla_cd.model)
# anova(bangla_cd.model,null.model)
# ggplot(compiled_closure_durations, aes(x=lary_cont, y=cd)) + geom_boxplot(aes(fill = factor(lary_cont))) + facet_grid(poa~sex) + xlab("Laryngeal context")
# 
# # calculate an F2-locus equation for initial [d] 
# # preceding lax vowels produced by female speaker "68".
# # the onset is taken at the vowel onset; the
# # vowel target is taken at the vowel's temporal midpoint.
# library(emuR)
# # # identify initial "d" of speaker "68"
# # temp <- vowlax.left == "d" & vowlax.spkr == "68"
# # # get the F2 value at the vowel's temporal midpoint
# # targ <- dcut(vowlax.fdat[temp,2], .5, prop=TRUE)
# # # F2 value at the vowel's acoustic onset.
# # on <- dcut(vowlax.fdat[temp,2], 0, prop=TRUE)
# attach(compiled_F2_data)
# library(dplyr)
# select(compiled_F2_data, .data$subject)
# # locus equation plot
# result <- locus(V2F2_50, V2F2_5, compiled_F2_data)
# # statistical diagnostics of the regression line (locus equation)
# summary(result)
# # intercept and slope
# result$coeff
# # best estimate of the locus frequency, i.e. the
# # point of bisection of on = TRUEarg with the regression line
# result$locus
# 
