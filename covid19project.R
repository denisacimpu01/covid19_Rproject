library(Hmisc)

data <- read.csv("C:/Users/Denisa/Desktop/covid19-project/COVID19_line_list_data.csv")
describe(data)

#cleaning up the death column
data$death_dummy <- as.integer(data$death!=0) #if the death column is not
#equal to 0, then the person died

unique(data$death_dummy) #the only values are now 0 and 1

sum(data$death_dummy)/nrow(data) #the death rate

#age
#claim: people who die are older
dead=subset(data,death_dummy == 1) #63 people died
alive=subset(data, death_dummy == 0) #1022 people survived
mean(dead$age, na.rm = TRUE) #68.5 years
mean(alive$age, na.rm = TRUE) #48 years

#is this statiscally significant?
t.test(alive$age, dead$age, alternative="two.sided", conf.level= 0.99)
#our t-test returns the p-value 2.2e-16, which is less than 0.05
#we reject the null hypothesis
#so, the people who die from covid-19 are older than the people who survive

#gender

male=subset(data,gender == 'male') 
female=subset(data, gender == 'female') 
mean(male$death_dummy, na.rm = TRUE) #8.4%
mean(female$death_dummy, na.rm = TRUE) #3.6%

t.test(male$death_dummy, female$death_dummy, alternative="two.sided", conf.level= 0.99)
#the p-value is 0.002105, so we reject the null hypothesis
#men have from 0.8% to 8.8% higher chances of dying from covid-19
