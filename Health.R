Health <- read.table(file.choose(),sep=",",header=TRUE)
str(Health)
summary(Health) #displays the mean, median, min and max for each variable
attach(Health)
dim(Health) #displays the no of rows and columns


########## To find the age category that has the highest frequency of hospital visit

hist(Health$AGE, main = "Histogram of AGE", xlab ="AGE")
AGE <- as.factor(AGE)
max(table(AGE))
#From the graph, infants has the maximum frequency of hospital visit, going above 300. After converting
#the age from numeric to factor. There are 307 entries for those in the range of 0-1 year.


########## Maximum expenditure for the age group who frequently visited the hospital
AGE_EXP <- aggregate(TOTCHG ~ AGE, data = Health, sum)
max(AGE_EXP)


########## To find the diagnosis related group that has maximum hospitalization and expenditure
APRDRG_FACTOR<-as.factor(APRDRG)
APRDRG_TABLE <- table(APRDRG_FACTOR)
which.max(APRDRG_TABLE)
APRDRG_TOTCHG<-aggregate(TOTCHG~APRDRG,FUN = sum,data=Health)
APRDRG_TOTCHG
APRDRG_TOTCHG[which.max(APRDRG_TOTCHG$TOTCHG),]


########## To analyze if the race of the patient is related to the hospitalization costs.
RACE <- as.factor(RACE)
table(RACE)
lm_Health <- lm((TOTCHG)~RACE)
anova(lm_Health)
summary(lm_Health)

#The result of anova shows there is no significance relationship between RACE and TOTCHG
#The result of summary shows there is no significance difference between the different race except for RACE 1


########## To analyze the severity of the hospital costs by age and gender for proper allocation of resources.
FEMALE <- as.factor(FEMALE)
table(FEMALE)
lm_health2 <- lm(TOTCHG ~ AGE*FEMALE)
anova(lm_health2)
summary(lm_health2)
#The feature "Female" has no interaction with the hospitalization cost(TOTCHG) but AGE is related to (TOTCHG)
#There is interaction between (AGE & FEMALE)
#summary show the features and the interactions between the different labels of the factors


########## To find if the length of stay can be predicted from age, gender, and race.
lm_health3 <- lm(LOS ~ AGE+FEMALE+RACE)
anova(lm_health3)
summary(lm_health3)
#There is no interaction between LOS, AGE, FEMALE & RACE (probability value > 0.05)


########## To find the variable that mainly affects the hospital costs
lm_health4 <- lm(TOTCHG ~ AGE+FEMALE+RACE+LOS+APRDRG)
anova(lm_health4)
summary(lm_health4)
#There is significance relationship between TOTCHG and (AGE,LOS,APRDRG)

