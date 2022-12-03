#-------------------------
# Econ 408 Final Project 
# Version 01_sharon
#-------------------------

#--------------------
# 1a: Set Directory 
#--------------------
setwd("~/Downloads")
library(haven) #to import data 
library(dplyr) #to filter 

#------------------
# 1b: Import Data & Clean
#------------------
survey<-read.csv("survey.csv",encoding="UTF-8")

#Remove extraneous columns and rows
survey <-  survey[ -c(1:17) ]
survey<- survey[-2,]

survey_w_question <- survey
survey <-survey[-1,]
str(survey)
#factorize variables

survey$Q7 <- as.factor(survey$Q7)
survey$Q8 <- as.factor(survey$Q8)



#-------------------
# Part 1c: Subsets 
#-------------------
# Create the treatment group (the group of first-gen students who were primed):
# Those who said yes to q20.1
# AND those who answered q16
primed_first_gen<-filter(survey,Q20.1=="Yes" & (Q16=="A lot of Support"|
                                                   Q16=="Little Support"|
                                                   Q16=="Moderate Support"|
                                                   Q16=="None"))

# Create the control group (the group of first-gen students who were NOT primed):
# Those who said yes to q20.1
# AND those who answered q18
control_first_gen<-filter(survey,Q20.1=="Yes" & (Q19=="Duderstadt"|Q19=="Hatcher"|
                                                      Q19=="Law"|Q19=="Shapiro"))

# Check: total obs of groups above approx add up to total obs for following: 
first_gen<-filter(survey,Q20.1=="Yes")



# Create the primed non-first-gen group: 
# Those who said no to q20.1
# AND those who answered q16
primed_non_first_gen<-filter(survey,Q20.1=="No" & (Q16=="A lot of Support"|
                                                  Q16=="Little Support"|
                                                  Q16=="Moderate Support"|
                                                  Q16=="None"))

# Create the non-primed non-first-gen group: 
# Those who said no to q20.1
# AND those who answered q18
control_non_first_gen<-filter(survey,Q20.1=="No" & (Q19=="Duderstadt"|Q19=="Hatcher"|
                                                   Q19=="Law"|Q19=="Shapiro"))

# Check: total obs of groups above approx add up to total obs for following:
non_first_gen<-filter(survey,Q20.1=="No")

#-----------------
# Patience Vrs 1 
#-----------------

# what is the proportion of primed first gen kids who choose a over b (impatient)

prop.table(table(primed_first_gen$Q21.1_1)) # 74% of primed first-gen are impatient, 26% patient 
prop.table(table(control_first_gen$Q21.1_1)) # 75% of control first-gen are impatient, 25% patient 
                                            # no sig diff b/w primed and control first-gen

prop.table(table(primed_first_gen$Q21.1_2)) # 61% impatient, 39% patient
prop.table(table(control_first_gen$Q21.1_2)) # 60% impatient, 40% patient 
                                            # no sig diff 

prop.table(table(primed_first_gen$Q21.1_3)) # 48% impatient, 52% patient 
prop.table(table(control_first_gen$Q21.1_3)) # 40% impatient, 60% patient
                                            # sig diff 

prop.table(table(primed_first_gen$Q21.1_4)) # 26% impatient, 74% patient
prop.table(table(control_first_gen$Q21.1_4)) # 30% impatient, 70% patient
                                            # sig diff

prop.table(table(primed_first_gen$Q21.1_5)) # 13% impatient, 87% patient
prop.table(table(control_first_gen$Q21.1_5)) # 10% impatient, 90% patient
                                            # sig diff

prop.table(table(primed_first_gen$Q21.1_6)) # 9% impatient, 91% patient
prop.table(table(control_first_gen$Q21.1_6)) # 10% impatient, 90% patient 
                                            # no sig diff 

prop.table(table(primed_first_gen$Q21.1_7)) # 4% impatient, 96% patient
prop.table(table(control_first_gen$Q21.1_7)) #10% impatient, 90% patient
                                            # sig diff
# # # oh no there are NULL answers here 
prop.table(table(primed_first_gen$Q21.1_8)) # 4% impatient, 91% patient
prop.table(table(control_first_gen$Q21.1_8)) # 5% impatient, 95% patient 
                                            # no sig diff but need to fix ! ! !



prop.table(table(primed_non_first_gen$Q21.1_1)) # 53% of primed non-first-gen are impatient, 41% patient 
prop.table(table(control_non_first_gen$Q21.1_1)) # 68% of control non-first-gen are impatient, 30% patient 
                                                # sig diff

# # # oh no there are NULL answers here
prop.table(table(primed_non_first_gen$Q21.1_2)) # 42% impatient, 52% patient
prop.table(table(control_non_first_gen$Q21.1_2)) # 45% impatient, 53% patient
                                                # sig diff 
# # # also NULL here 
prop.table(table(primed_non_first_gen$Q21.1_3)) # 26% impatient, 67% patient
prop.table(table(control_non_first_gen$Q21.1_3)) # 31% impatient, 68% patient 
                                                # sig diff
# # # also NULL here
prop.table(table(primed_non_first_gen$Q21.1_4)) # 17% impatient, 77% patient
prop.table(table(control_non_first_gen$Q21.1_4)) # 20% impatient, 80% patient 
                                                # sig diff

# # # also NULL here
prop.table(table(primed_non_first_gen$Q21.1_5)) # 7% impatient, 88% patient 
prop.table(table(control_non_first_gen$Q21.1_5)) # 11% impatient, 86% patient 
                                                # sig diff 
# # # also NULL here 
prop.table(table(primed_non_first_gen$Q21.1_6)) # 6% impatient, 89% patient
prop.table(table(control_non_first_gen$Q21.1_6)) # 5% impatient, 93% patient
                                                # no sig diff
# # # also NULL here 
prop.table(table(primed_non_first_gen$Q21.1_7))  # 5% impatient, 90% patient
prop.table(table(control_non_first_gen$Q21.1_7)) # 5% impatient, 93% patient
                                                # no sig diff 
# # # also NULL in the second one
prop.table(table(primed_non_first_gen$Q21.1_8)) # 3% impatient, 97% patient
prop.table(table(control_non_first_gen$Q21.1_8)) # 2% impatient, 95% patient
                                                # no sig diff 

#-----------------
# Patience Vrs 2 
#-----------------










#----------------------------
# Part 3a: Risk Pref Game #1
#----------------------------
prop.table(table(primed_first_gen$Q28.1_1)) # 83% of primed first-gen are risk averse, 13% risky
prop.table(table(control_first_gen$Q28.1_1)) # 85% of control first-gen are risk averse, 5% risky  
                                            # no sig diff b/w primed and control first-gen

prop.table(table(primed_non_first_gen$Q28.1_1)) # 91% of primed non-first-gen are risk averse, 6% risky 
prop.table(table(control_non_first_gen$Q28.1_1)) # 95% of control non-first-gen are risk averse, 1% risky
# is this a sig diff? 


#----------------------------
# Part 3b: Risk Pref Game #2
#----------------------------
# Create variable for risk:
# # # I NEED A NEW DATA SET WITH THE NUMBERS ONLY CUZ I CAN'T ENTER THE FRACTIONS



#----------------------------
# Part 3c: Risk Pref Game #3
#----------------------------
# Create another variable for risk: 
survey$risk2<-0
survey$risk2[survey$Q32=="Salary of $40,000 with 0% probability of being laid-off"]<-1
survey$risk2[survey$Q32=="Salary of $75,000 with 10% probability of being laid-off"]<-2
survey$risk2[survey$Q32=="Salary of $100,000 with 20% probability of being laid-off"]<-3
survey$risk2[survey$Q32=="Salary of $200,000 with 30% probability of being laid-off"]<-4

# Create variable for primed first gen: 
survey$r<-0
survey$risk2[survey$Q32=="Salary of $40,000 with 0% probability of being laid-off"]<-1
survey$risk2[survey$Q32=="Salary of $75,000 with 10% probability of being laid-off"]<-2
survey$risk2[survey$Q32=="Salary of $100,000 with 20% probability of being laid-off"]<-3
survey$risk2[survey$Q32=="Salary of $200,000 with 30% probability of being laid-off"]<-4


reg1<-lm(risk2~primed_first_gen,data=survey)


#----------------------------
# Part 5: Questionnaire Data
#----------------------------


#reverse scoring

str(survey)
survey %>% group_by(Q20.1) %>% summarize(college_risk=mean(Q26_1))
#----------------------------
# Part 5: Exploratory Analysis
#----------------------------


