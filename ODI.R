#Data has been extracted from ESPNCricinfo
#Dataset consists of ODI matches summary which includes the two playing teams,
#and their respective overs played, wickets down and runs per inning.
odi<-read.csv("/Users/khyati_soni/Downloads/matches.csv")
head(odi)

#Extracting all the matches played by India
crik_india <- odi[odi$team1=="INDIA" | odi$team2 == "INDIA",]
head(crik_india)

#Extracting first innings runs of India:
x<-crik_india[crik_india$team1=="INDIA",5]
innings1<-x[1:50]

#Density plot of first innings runs
plot(density(x),main="Comparing Density Plot of runs scored \nin the first inning by 
     India over all the matches \nwith fitted Normal Distribution",xlab="Runs")

#Finding parameters mean and sigma to fit a distribution 
#using MLE (Maximum Likelihood Estimate)
f<-function(params){
  lnL<-dnorm(x,params[1],params[2],log = TRUE) #log likelihood function
  sum(-lnL) #sum of log likelihood function
}

p<-c(mean(x),sd(x))
nlm(f,p)$estimate

mu<-nlm(f,p)$estimate[1]
sigma<-nlm(f,p)$estimate[2]

x1<-seq(0,500)
lines(x1,dnorm(x1,mu,sigma),col="blue")
legend("topright",legend=c("Actual Data","Fitted Data"),col=c("black","blue"),lty=c(1,1))
#Therefore, we can conclude that the given estimate provides a good fit to our model.
#Knowing the parameters, we can perform various analysis such as calculation of
#confidence interval and hypothesis testing.

#Similarly for the second innings:
y<-crik_india[crik_india$team2=="INDIA",5]
innings2<-y[1:50]

#Density plot of second innings
plot(density(y),main="Comparing Density Plot of runs scored \nin the first inning 
     by India over all the matches \nwith fitted Normal Distribution",xlab="Runs")
#Finding parameters mean and sigma to fit a distribution using 
#MLE (Maximum Likelihood Estimate)
f<-function(params){
  lnL<-dnorm(y,params[1],params[2],log = TRUE)
  sum(-lnL)
}

p<-c(mean(x),sd(x))
nlm(f,p)$estimate

mu<-nlm(f,p)$estimate[1]
sigma<-nlm(f,p)$estimate[2]

y1<-seq(0,500)
lines(y1,dnorm(y1,mu,sigma),col="blue")
legend("topright",legend=c("Actual Data","Fitted Data"),col=c("black","blue"),lty=c(1,1))
#Therefore, we can conclude that the given estimate provides a good fit to our model.
#Knowing the parameters, we can perform various analysis such as calculation of
#confidence interval and hypothesis testing.




#Effect of the second innings adjustment:
#Does India play better in the first inning or in the second innning?
#Comparing Density Plots of First and Second Innings played by India

plot(density(x),col="blue",ylim=c(0,0.008),main="Comparing Density Plots of the 
     two innings",xlab="Number of runs")
lines(density(y),col="red")
legend("topright",legend=c("First Innings","Second Innings"),
       col=c("blue","red"),lty=c(1,1))
#Conclusion: India plays better in the Second Innings
#Therefore, they should choose to bowl first if the coin flip 
#turns out to be in their favour.




#Extracting number of wickets down if India chooses to bat in the first 
#inning over all the matches:
wik1<-crik_india[crik_india$team1=="INDIA",4]
#Total number of wickets taken in the first inning:
tot_wik1<-sum(wik1)

#Extracting number of wickets down if India chooses to bat in the first 
#inning over all the matches:
wik2<-crik_india[crik_india$team2=="INDIA",7]
#Total number of wickets taken in the first inning:
tot_wik2<-sum(wik2)

#Thus, calculating the percentage of wickets down in the 
#first vs second innings for India:
tot_wik1/(length(wik1)*10)*100
tot_wik2/(length(wik2)*10)*100

#We observe that the percentage of wickets down over all the matches if India plays in the
#first innings is higher than that if India plays in the second innings.
#Therefore, we again conclude that India should choose to bowl first if the coin 
#flip turns out to be in their favour.




#Generating Time Series of the first inning and plotting
TS1<-ts(innings1,start=1,end=length(innings1),frequency=1)
ts.plot(TS1,col="blue",ylab="Runs per inning",main="TIME SERIES (Number of runs): 
        \nInning-1 vs Inning-2")

#Generating Time Series of the second inning and plotting to compare 
#it to the first inning
TS2<-ts(innings2,start=1,end=length(innings2),frequency=1)
lines(TS2,col="red")
legend("top",legend=c("Innings 1","Innings 2"),col=c("blue","red"),lty=c(1,1))


#Fitting time series process to our model:
#The following function calculates the aic of all the possible combinations for
#'p', 'd', 'q' for an ARIMA-p,d,q process.
ans<-numeric(4)
for(p in 0:2){
  for(d in 0:2){
    for(q in 0:2){
      aic<-arima(TS1,order=c(p,d,q))$aic #Extracting akaike's information criterion
      row<-c(p,d,q,aic)
      ans<-rbind(ans,row)
    }
  }
}

head(ans)
ans<-ans[-1,]
head(ans)

#Now, we extract the minimum aic and the respective p,d and q values of our ARIMA model.
which(ans[,4]==min(ans[,4])) #Extracts the row number of the lowest aic
ans[9,]

fit<-arima(TS1,order=c(0,2,2))
fit
#beta1 = -1.8699
#beta2 = 0.8803
plot(fit$residuals,main="Residuals Plot",ylab="Residuals")
#The residuals plot is patternless
#There are almost equal number of positive and negative values
#Therefore, the ARIMA(0,2,2) process provides a good fit our model.



#MODELLING:
tsdiag(fit)
#Thus, for Standardized residuals:
#1. It is patternless
#2. There are equal number of positive and negative values
#3. Standardized Residuals lie between the range (-2,2) (since 96% of the values should
#lie between -2 and 2)
#ACF gets cut off for k>=1
#Thus, fit is very good




#qqplot between innings1 and innings 2 of India
qqplot(innings1,innings2,main="QQPlot: Innings1 vs Innings2",
       xlab="Innings 1",ylab="Innings 2")
abline(0,1,col="red",lty=3,lwd=2)




#QQ plot corresponding to first innings runs for Australia batting against India
Ind<-crik_india[crik_india$team1=="INDIA",5]
Aus<-crik_india[crik_india$team2=="AUSTRALIA",5]
qqplot(Ind,Aus,main="QQ plot corresponding to first innings runs for Australia 
       \nbatting against India.",
       xlab="India",ylab="Australia")
abline(0,1,col="red",lty=2,lwd=2)




#COMPARING- Number of overs played in the First innings by Australia and by India:
Aus_data<-odi[odi$team1=="AUSTRALIA",]
head(Aus_data)
n1<-length(Aus_data)
c1=0
for(i in 1:n1){
  if(Aus_data$innings1_overs[i]==50){
    c1=c1+1
  }
}

c1

Ind_data<-odi[odi$team1=="INDIA",]
n2<-length(Ind_data)
c2=0
for(i in 1:n2){
  if(Ind_data$innings1_overs[i]==50){
    c2=c2+1
  }
}

c2


#Thus, % of the time Australia uses all its overs in innings 1:
c1/n1*100

#Thus, % of the time India uses all its overs in innings 1:
c2/n2*100

#This suggests that there is merit in our modification of aggressiveness in first innings
#batting by Australia as compared to India.
#This supports our previous claim as well where had concluded that India plays better 
#if they bat in the second innings.




#Analysing Sachin Tendulkkar as a cricketer:
#Data has been extracted from ESPNCricinfo
#Dataset consists summary statistics of Sachin Tendulkar as a batsman from 1989 to 2013.
data<-read.csv("/Users/khyati_soni/Documents/Khyati/Internship/cricketr/cricketr/data/tendulkar.csv")
head(data)

#Exctracting runs
runs<-data[,2]
head(runs)

#Creating Time Series from runs data:
ts<-ts(runs,start=1989,end=2013)

#The following function calculates the aic of all the possible combinations for
#'p', 'd', 'q' for an ARIMA-p,d,q process.
ans<-numeric(4)
for(p in 0:2){
  for(d in 0:2){
    for(q in 0:2){
      aic<-arima(ts,order=c(p,d,q))$aic
      row<-c(p,d,q,aic)
      ans<-rbind(ans,row)
    }
  }
}
head(ans)
ans<-ans[-1,]
head(ans)

#Now, we extract the minimum aic and the respective p,d and q values of our ARIMA model.

which(ans[,4]==min(ans[,4]))
ans[9,]

#Therefore, the model follows an ARIMA(0,2,2) process

fit<-arima(ts,order=c(0,2,2))
fit

#Extracting the residuals of the time series
et<-fit$residuals
plot(et,main="Residuals Plot",ylab="Residuals",col="red")
#Plot of Residuals is patternless
#There are almost equal number of positive and negative values
#Thus, we can conclude that ARIMA(0,2,2) process provied a good fit to our model
#We can therefore, predict future values using this model.

acf(et,main="Auto Correlation Function of Residuals plot")

#Predicting future 5 years time series model:
pd<-predict(fit,n.ahead = 5)$pred
ts.plot(ts,xlim=c(1990,2020),main="PREDICTING MODEL FOR THE NEXT FIVE YEAR",ylab="Runs")
lines(pd,col="dark red")
legend("topright",legend="Predicting line",col="dark red",lty=1)


#Differencing Data for Stationarity:
ds<-diff(ts)
dds<-diff(ds)

par(mfrow=c(3,1))
acf(ts)
acf(ds)
acf(dds)
par(mfrow=c(1,1))

#Variance Test: Should be least
var(ts)
var(ds)
var(dds)
#Since variance of ts is least and we can also observe from the diagram above,
#the model does not need to be differenced at all for the process to attain
#stationarity.


#In this article, I have used performed simulation and used Time Series Analysis
#for modelling and predicting the model.
#With respect to India's performance in ODI matches, I have conluded that India plays 
#better if given a chance to bowl first and bat second in terms of runs and wickets down.

#For prediction of Sachin Tendulkar's performance, I have fitted an accurate time series
#process based on his performances over the years. Accuracy is certain since the residuals
#plot was patternless and there were equal number of negative and positive values.
#Using this fit, I have predicted his performance over the next 5 years. 


#BIBLIOGRAPHY:
#https://stats.espncricinfo.com/ci/engine/records/index.html
