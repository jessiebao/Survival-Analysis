# title: log-rank test of left-trucated data 
# date: 1/8/2017
# author: Jessie Bao
# description: using method described on Chapter 7.2 page 212 of survival analysis
# techniques for censored and truncated data

##left-truncated data
data(channing)
##to obtain the number of people at risk at each time
new.data.f <- c()
new.data.m <- c()
for(i in channing$obs){
  if(channing[i,"gender"]==2){
    new.data.f <- c(new.data.f,seq(channing[i,"ageentry"], channing[i,"age"]))
  }else{
    new.data.m <- c(new.data.m, seq(channing[i, "ageentry"], channing[i, "age"]))
  }
}
as.data.frame(table(new.data.f)) -> data.f
data.f <- data.frame(age = as.numeric(levels(data.f$new.data.f)), risk.female = data.f$Freq)
as.data.frame(table(new.data.m)) -> data.m
data.m <- data.frame(age = as.numeric(levels(data.m$new.data.m)), risk.male = data.m$Freq)
plot(data.f$risk.female~data.f$age, xlab="Age in months", ylab = "The number of people at risk", col =1)
lines(y = data.m$risk.male, x = data.m$age, lty = 3, col=2)
legend("topright", c("Female", "Male"), text.col =  1:2)
library(dplyr)
#event times ti and the number of deaths at each time for female and male 
death.female <- channing %>% filter(gender == 2) %>%  #only select females
  group_by(age) %>%  #summarize data by death times
  summarize(d.female = sum(death)) %>%
  filter(d.female >0 ) #remove time points with no death
death.male <- channing %>% filter(gender == 1) %>%  #only select males
  group_by(age) %>%  #summarize data by death times
  summarize(d.male = sum(death)) %>%
  filter(d.male >0 )  #remove time points with no death
answer <- death.female %>% full_join(death.male)  #outer join two table
answer[is.na(answer$d.female),"d.female"]=0  #change non-matches NA to 0
answer[is.na(answer$d.male),"d.male"]=0

answer<- answer %>% left_join(data.f)  #add column of number at risk for female
answer<- answer %>% left_join(data.m)  #add column of number at risk for male
answer[is.na(answer$risk.male),"risk.male"]=0
answer$d.total <- answer$d.male + answer$d.female
answer$risk.total <- answer$risk.male + answer$risk.female
answer$expected.d.feamle <- answer$d.total*answer$risk.female/answer$risk.total
answer$E.minus.O.d.female <- answer$d.female - answer$expected.d.feamle
answer$part = answer$risk.female*answer$risk.male*(answer$risk.total-answer$d.total)*answer$d.total/(answer$risk.total^2*(answer$risk.total-1))

#calculate log-rank statistics with weight of 1
z = sum(answer$E.minus.O.d.female)/sqrt(sum(answer$part))
z
#p-value
qnorm(0.05, lower.tail = TRUE )
#conclusion: we reject Ho that female and male have the same hazard rate with significant level of 0.05.
#and favor the Ha that female has smaller hazard rate
