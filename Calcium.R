#########ANOVA
#Response variable (CI) 
#Two categorical variable; gender with two labels (male and female) & Bone with 3 labels (OO, OA AND ND)

Calcium <- read.table(file.choose(), header=TRUE)
names(Calcium)
str(Calcium)
attach(Calcium)


######### DESCRIPTIVE STATISTICS
#The mean it is obvious that the daily intake of calcium is most effective in adult male with normal bone density 
#and the least effective is in adult female with osteoporosis
#I can assume adult female with normal bone density has a low variation from the mean, while adult female with 
#osteoporosis has a large variation from the mean which could be due to select observations with high value as a 
#result of other effect aside calcium intake
with(data = Calcium, expr = tapply(CI, list(Bone, Gender), mean))
with(data = Calcium, expr = tapply(CI, list(Bone, Gender), sd))
library(ggplot2)
library(gridExtra)
p1 <- ggplot(data = Calcium, mapping = aes(x = Bone, y = CI)) + geom_boxplot() + 
  theme_bw()
p2 <- ggplot(data = Calcium, mapping = aes(x = Gender, y = CI)) + geom_boxplot() + 
  theme_bw()
p3 <- ggplot(data = Calcium, mapping = aes(x = Bone, y = CI, colour = Gender)) + 
  geom_boxplot() + theme_bw()

grid.arrange(p1, p2, ncol = 2)

p3

#Calcium intake with gender - calcium intake in male seem higher than in female
#Calcium intake in bone - calcium intake in male seem higher than in female


boxplot(CI~Gender, ylab = "CI (mg/day)", xlab = "Gender", main = "Calcium intake with gender")
boxplot(CI~Bone, ylab = "CI (mg/day)", xlab = "Bone", main = "Calcium intake in bone")


###########Analysis of Variance
#There is significance different with calcium intake and bone also with gender because the P-value> 0.05. 
#But no interaction between the two variable (gender& bone) and I simplified the model from multiplicative to
#a more pasimonic model which is the additive effect.

CI_Bone_Gender<-lm(CI~Bone*Gender)          ### (*) multiplicative model with interaction
anova(CI_Bone_Gender)


CI_Bone_Gender<-lm(CI~Bone+Gender)     ### (+) Additive model without interaction
anova(CI_Bone_Gender)
summary(CI_Bone_Gender)


######### INTERACTION PLOT
#The calcium intake effect is highest in adult with normal bone density and in general the effect of calcium intake is highest
#in male than in female for the three different bone. The pattern are the same 
#The pattern of the effect of calcium intake is the same for male and female which is that as the calcium intake increases the 
#effect deceases from normal bone to low bone density(OA) and finally to ostereoporosis(OO).

ggplot(data = Calcium, aes(x = Gender, y = CI, colour =Bone, group = Bone)) + ggtitle("Effect of Calcium in gender(male & female) with different Bone type") +
  stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, 
                                                            geom = "line") + labs(y = "mean (CI mg/day)") + theme_bw()

ggplot(data = Calcium, aes(x = Bone, y = CI, colour = Gender, group = Gender)) + ggtitle("Effect of Calcium in different Bone based on Gender") +
  stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, 
                                                            geom = "line") + labs(y = "mean (CI mg/day)") + theme_bw()



######### ASSUMPTIONS OF MODEL
#The normality and heteroscedasticity looks acceptable from the plot

par(mfcol=c(2,2))
plot(CI_Bone_Gender)


#Shapiro test (test for normality in residuals "the variation among sampling units within each sample): the normality is greater than 0.05

leveneTest(CI_Bone_Gender) ###test heteroscedasticity
shapiro.test(residuals(CI_Bone_Gender)) ###test normality in residuals "the variation amongst sampling units within each sample"


