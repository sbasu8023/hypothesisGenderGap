# Load packages


library(ggplot2)
library(dplyr)
library(gridBase)
library(gridExtra)
library(statsr) # For verification only


#Load data

load("gss.Rdata")

# Total female subjects
t_respF <- gss %>% filter(sex=="Female") %>% select(sex)
t_respF <- nrow(t_respF)

# Creating tables with female having education Bachelors or above
degree_tblF_cnt <- gss %>% filter(sex=="Female", degree=="Bachelor" | degree=="Graduate") %>% select(degree) %>% group_by(degree) %>% tally()

# Adding a column in above table showing perecentage

degree_tblF_cnt <- degree_tblF_cnt %>% mutate(percent=n*100/ t_respF)

# Total male subjects
t_respM <- gss %>% filter(sex=="Male") %>% select(sex) 
t_respM <- nrow(t_respM)

# Creating tables with male having education Bachelors or above
degree_tblM_cnt <- gss %>% filter(sex=="Male", degree=="Bachelor" | degree=="Graduate") %>% select(degree) %>% group_by(degree) %>% tally()
# Adding a column in above table showing perecentage

degree_tblM_cnt <- degree_tblM_cnt %>% mutate(percent=n*100/ t_respM)

# Snaphot of female subjects with education, bachelors or above

degree_tblF_cnt

#  Snaphot of male subjects with education, bachelors or above

degree_tblM_cnt

# Visual comparison of  higher education among female and male in the sample

f_hEd <- ggplot(degree_tblF_cnt, aes(x=degree, y=percent)) + geom_bar(stat = "identity", color="black",fill="orange")
m_hEd <- ggplot(degree_tblM_cnt, aes(x=degree, y=percent)) + geom_bar(stat = "identity", color="black",fill="deepskyblue")
f_hEd <- f_hEd + scale_y_continuous(limits=c(0, 20)) + ggtitle("Female Higher Education") 
m_hEd <- m_hEd + scale_y_continuous(limits=c(0, 20)) + ggtitle("Male Higher Education") 
grid.arrange(f_hEd, m_hEd, ncol=2)


# Creating table of sex of respondants and their correponding education degree
ed_mf <- gss %>% filter(!is.na(sex), !is.na(degree)) %>% select(sex, degree)
# Classifying degree into level Bachelors and above Higher Education and everything else as Lower
ed_mf <- ed_mf %>% mutate(lev=ifelse(degree=="Bachelor" | degree=="Graduate", "High", "Low"))
# Number of samples with High and Low Education for females
f_level <- ed_mf %>% filter(sex=="Female") %>% group_by(lev) %>% tally()
# Number of samples with High and Low Education for males
m_level <- ed_mf %>% filter(sex=="Male") %>% group_by(lev) %>% tally()
# Total number of subjects by sex
total <- ed_mf %>% group_by(sex) %>% tally() 
# Extracting data from above tables for comparision tables 
f_level_H <- f_level %>% filter(lev=="High") %>% select(n)
m_level_H <- m_level %>% filter(lev=="High") %>% select(n)

f_tot <- total %>% filter(sex=="Female") %>% select(n)
m_tot <- total %>% filter(sex=="Male") %>% select(n)
# Force R not to use exponential notation 
options(scipen=10000)


male <- c(m_level_H$n , m_tot$n - m_level_H$n, m_tot$n, m_level_H$n/m_tot$n )
female <- c(f_level_H$n , f_tot$n - f_level_H$n, f_tot$n, f_level_H$n/f_tot$n)

#  Creating Comparision table
comp_table <- data.frame(male, female)

# For easy readabilty
rownames(comp_table) <- c("Higher Education", "No Higher education", "Total (n)", "p_hat")

comp_table

# For males
m_tot$n*(m_level_H$n/m_tot$n) # Success
m_tot$n*(1-(m_level_H$n/m_tot$n)) #Failure

# For females
f_tot$n*(f_level_H$n/f_tot$n) # Success
f_tot$n*(1-(f_level_H$n/f_tot$n)) #Failure

p_hat_male <- (m_level_H$n/m_tot$n) # Sample proportion of male success (Higher Education)
p_hat_female <- (f_level_H$n/f_tot$n) # Sample proportion of female success (Higher Education)

# Determining point estimate
pe <-p_hat_male-p_hat_female
pe

# Determining Standard error
se <- sqrt(((p_hat_male*(1-p_hat_male))/m_tot$n)+((p_hat_female*(1-p_hat_female))/f_tot$n))
se 

# Determining confidence interval
ci <- c(pe-(1.96*se), pe+(1.96*se))
ci

#statsr is not standard R library but a companion package for the Coursera Statistics with R specialization developed by Prof. Dr Mine Cetinkaya-Rundel, Duke University Department of Statistical Science.

inference(y = lev, x=sex, data = ed_mf,  statistic = "proportion", type = "ci",  method = "theoretical", success = "High")

#Hypothesis Testing

p_hat_pool<-(m_level_H$n+f_level_H$n)/(m_tot$n+f_tot$n)
p_hat_pool

# For males
m_tot$n*p_hat_pool # Success
m_tot$n*(1-p_hat_pool) #Failure

# For females
f_tot$n*p_hat_pool # Success
f_tot$n*(1-p_hat_pool) #Failure

#Calculating standard Error

# Calculating p(1-p)/n1  and p(1-p)/n2
ml_s <- (p_hat_pool*(1-p_hat_pool))/m_tot$n
fl_s <- (p_hat_pool*(1-p_hat_pool))/f_tot$n
# Calculating Standard Error
se <- sqrt(ml_s + fl_s)
se

# Determining point estimate

prop_diff <- (m_level_H$n/m_tot$n ) -(f_level_H$n/f_tot$n )
prop_diff

# Estimating Z Score

z<-(prop_diff-0)/se
z

# statsr is not standard R library but a companion package for the Coursera Statistics with R specialization developed by Prof. Dr Mine Cetinkaya-Rundel, Duke University Department of Statistical Science.

inference(y = lev, x=sex, data = ed_mf,  statistic = "proportion", type = "ht", alternative="twosided", null=0, method = "theoretical", success = "High")

#Conclusion 
# Proportion of male and female with higher education in US are not equal. Probability of observed or more extreme outcome given null hypothesis is true is almost 0. In other word probability of difference in population proportion simply by chance is almost 0%.



