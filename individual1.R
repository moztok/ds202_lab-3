install.packages('readxl')
dat <- readxl::read_xls('GSS.xls')
View(dat)
library(classdata)
library(ggplot2)
library(dplyr)
library(tidyverse)


##Rename
dat <- dat %>% rename(year = `Gss year for this respondent`)
dat <- dat %>% rename(wrkslf = `R self-emp or works for somebody`)
dat <- dat %>% rename(wrkgovt = `Govt or private employee`)
dat <- dat %>% rename(marital = `Marital status`)
dat <- dat %>% rename(educ = `Highest year of school completed`)
dat <- dat %>% rename(sex = `Respondents sex`)
dat <- dat %>% rename(rincome = `Respondents income`)
dat <- dat %>% rename(partyid = `Political party affiliation`)
dat <- dat %>% rename(happy = `General happiness`)
View(dat)

##1
dat1 <- dat[- grep("Don't know", dat$educ),]
dat2 <- dat1[- grep("No answer", dat1$educ),]
dat3 <- dat2[- grep("No answer", dat2$marital),]
dat3$educ <- as.numeric(as.character(dat3$educ))
dat3 %>%                                      
  group_by(marital) %>%                         
  summarise_at(vars(educ),              
               list(name = mean)) %>%
  arrange(desc(name))
## We found that people who were never married have the highest years of school experience
## We also found that widowed individuals have the lowest years of school experience.


##2
better_educated <- dat3 %>% group_by(marital) %>% filter(educ > mean(educ, na.rm = TRUE))
str(better_educated)


##3
dat4 <- dat3
happiness <- dat4[dat4$happy %in% c('Very happy' , 'Pretty happy'), ]

myTable = table(happiness$marital, happiness$happy)
myTable
myFrame <- as.data.frame(myTable) 


a <- ggplot(myFrame, aes(x = Var1, y = Freq, group = Var2)) +
  geom_bar(aes(fill = Var2), stat = "identity", position = "dodge") + 
  labs(fill = "Happiness status") + labs(x= "Marital Status", y="Count")
a
  






##4
happiness1 <- happiness[- grep("Don't know", happiness$partyid),]
happiness2 <- happiness1[- grep("No answer", happiness1$partyid),]
myTable2 = table(happiness2$marital, happiness2$happy, happiness2$partyid)
myTable2
myFrame2 <- as.data.frame(myTable2)
View(myFrame2)
ggplot(myFrame2, aes(x=Var1, weight= Freq)) + geom_bar() + facet_grid(Var3 ~ Var2 ) + labs(x= "Marital Status")



##5
dat5 <- dat3
myTable3 = table(dat5$marital, dat5$educ)
myTable3

myFrame3 <- as.data.frame(myTable3) 
View(myFrame3)

z <- ggplot(myFrame3, aes(x = Var2, y = Freq, group = Var1)) +
  geom_bar(aes(fill = Var1), stat = "identity", position = "dodge") + 
  labs(fill = "Happiness status") + labs(x= "Years of school", y="Count") + coord_flip()
z

##6
# Lets explore the happiness status of people depend on if they work for the government or not


sector <- happiness
sector_h <- sector[sector$wrkgovt %in% c('Government' , 'Private'), ]

View(sector_h)

myTable6 = table(sector_h$wrkgovt, sector_h$happy)
myTable6
myFrame6 <- as.data.frame(myTable6) 
View(myFrame6)


a <- ggplot(myFrame6, aes(x = Var1, y = Freq, group = Var2)) +
  geom_bar(aes(fill = Var2), stat = "identity", position = "dodge") + 
  labs(fill = "Happiness status") + labs(x= "Government or Private employee", y="Count")
a

##7
# Lets explore the income levels and works for self
dat7 <- dat
wrk_self <- dat7[dat7$wrkslf %in% c('Self-employed' , 'Someone else'), ]
income_comp <- wrk_self[wrk_self$rincome %in% c('Lt $1000' , '$1000 to 2999',  '$3000 to 3999',  '$4000 to 4999', '$5000 to 5999', '$6000 to 6999', '$7000 to 7999', '$8000 to 9999', 
                                              '$10000 - 14999', '$15000 - 19999', '$20000 - 24999', '$25000 or more'), ]

myTable7 = table(income_comp$wrkslf, income_comp$rincome)
myTable7
myFrame7 <- as.data.frame(myTable7) 
View(myFrame7)


o <- ggplot(myFrame7, aes(x = Var1, y = Freq, group = Var2)) +
  geom_bar(aes(fill = Var2), stat = "identity", position = "dodge") + 
  labs(fill = "Self-Employed or Work for Someone Else") + labs(x= "Income Leves", y="Count")
o





