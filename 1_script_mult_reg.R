library(tidyverse)
library(apaTables)
library(haven)
library(predictionInterval)

#load spss file
my.data <- read_spss("regression_example_data.sav")
my.data
str(data1)

##1 - single multiple regression
#Correlation table
apa.cor.table(my.data)


#make sure data isn't curvilinear, so regression makes sense
psych::pairs.panels(as.data.frame(my.data))


#do single regression
gma.reg <- lm(jobperf ~ gma, data=my.data)
summary(gma.reg)

# get the regression in APA style, with more helpful info (ie. b, CIs, beta, r, r2)
apa.reg.table(gma.reg)


#do multiple regressions
my.regression2 <- lm(jobperf ~ gma + con,data=my.data)
apa.reg.table(my.regression2, filename = "myRegressionTable.doc")

my.regression3 <- lm(jobperf ~ gma + ac,data=my.data)
apa.reg.table(my.regression3, filename = "myRegressionTable2.doc")

my.regression4 <- lm(jobperf ~ gma + graph,data=my.data)
apa.reg.table(my.regression4, filename = "myRegressionTable3.doc")

#gets the b-weights 
my.regression

#"Estimate" column is b-weight
summary(my.regression)

#Get b -weights, beta-weights, sr2, r, and R2
apa.reg.table(my.regression)


##2 - block regressions
#GMA + CON
block1 = lm(jobperf~gma,data=my.data)

block2 = lm(jobperf~gma+con,data=my.data)

apa.reg.table(block1, block2)

#GMA + AC
block1 = lm(jobperf~gma,data=my.data)

block3 = lm(jobperf~gma+ac,data=my.data)

apa.reg.table(block1, block3)

#GMA + graph
block1 = lm(jobperf~gma,data=my.data)

block4 = lm(jobperf~gma+graph,data=my.data)

apa.reg.table(block1, block4)



## 3 - CI at means
#find means
mean(my.data$gma)
mean(my.data$con)

#create GMA+CON reg
reg.2 <- lm(jobperf ~ gma + con, data = my.data)

#find CI
x_axis_range <- data.frame(gma = c(100), con=c(120))
x_axis_range
CI_data <- predict(reg.2, newdata = x_axis_range, interval = "confidence", level=0.95)
CI_data <- as.data.frame(cbind(x_axis_range, CI_data))
CI_data


#find PI
PI_data <- predict(reg.2, newdata = x_axis_range, interval = "prediction", level=0.95)
PI_data <- as.data.frame(cbind(x_axis_range, PI_data))
PI_data