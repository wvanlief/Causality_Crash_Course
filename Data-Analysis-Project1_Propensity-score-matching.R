# Libraries
library(tableone)
library(Matching)
library(MatchIt)

#Loading Data
data(lalonde)
raw <- lalonde

View(raw)

mydata<-data.frame(raw)

#covariates 
xvars<-c("age","educ","black","hispan","married","nodegree","re74","re75")

#look at a table 1
table1<- CreateTableOne(vars=xvars,strata="treat", data=mydata, test=FALSE)
## include standardized mean difference (SMD)
print(table1,smd=TRUE)

#mean difference pre treatment 
mean(mydata$re78[mydata$treat==1]) - mean(mydata$re78[mydata$treat==0])

#Creating the logistic regression model
set.seed(931139)
psmodel<-glm(treat~age+educ+black+hispan+married+nodegree+re74+re75,
             family=binomial(),data=mydata)

#show coefficients etc
summary(psmodel)

#create propensity score
pscore<-psmodel$fitted.values

#Match on pscore with seed and caliper and using the regression model 
psmatch<-Match(Tr=mydata$treat,M=1,X=pscore,replace=FALSE, caliper=0.1)

matched<-mydata[unlist(psmatch[c("index.treated","index.control")]), ]

#get standardized differences
matchedtab1<-CreateTableOne(vars=xvars, strata ="treat", 
                            data=matched, test = FALSE)
print(matchedtab1, smd = TRUE)

#outcome analysis and t testing 
mean(matched$re78[matched$treat==1]) - mean(matched$re78[matched$treat==0])

y_trt<-matched$re78[matched$treat==1]
y_con<-matched$re78[matched$treat==0]

#pairwise difference
diffy<-y_trt-y_con

#paired t-test
t.test(diffy)
