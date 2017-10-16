#Exploring the run size data at Albion test fishery to see if anything gives us better estimates of expansion/run size,
#so we could potentially modify the priors used in the in-season model
library(ggplot2)
library(changepoint)

setwd("C:/DFO-MPO/Modelling/Chum Model and Graphs/Water Quality vs Catch/")

#/////////////////////////////////////////////////////////////////////////
#Read in data and format
#/////////////////////////////////////////////////////////////////////////

run_data<-read.csv("Albion_CM_run_details_1980-2016.csv",header=T) #Data formatted by year

n_years=max(run_data$year)-1980+1

run_data$q<-1/run_data$expansion

#/////////////////////////////////////////////////////////////////////////
#Exploration
#/////////////////////////////////////////////////////////////////////////

plot(run_size~year,data=run_data, main="Annual Run Size", bty="l")

plot(expansion~year,data=run_data, main="Annual Expansion", bty="l")

plot(peak~year,data=run_data, main="Annual Peak", bty="l")

plot(spread~year,data=run_data, main="Annual Spread", bty="l")

plot(q~year,data=run_data, main="Annual Catchability", bty="l")



pairs(~run_size+expansion+peak+spread+q,data=run_data)

#From this, it looks like there might be a relationship between RS and exp, peak and spread

plot(expansion~run_size,data=run_data, main="Expansion vs Run Size", bty="l")

#Should also explore this:
plot(q~run_size,data=run_data, main="Catchability vs Run Size", bty="l")

#How much variation in expansion is explained by run size?

fit<-lm(expansion~run_size,data=run_data)
summary(fit)

#About 36%
#Maybe not a straight linear relationship?
plot(log(expansion)~log(run_size),data=run_data, main="log Expansion vs log Run Size", bty="l")

fit<-lm(log(expansion)~log(run_size),data=run_data)
summary(fit)

#This is a better fit - 44%; but, the intercept is not significant - what does that mean?
#"If the intercept is not significant you usually would not want to remove it from the model 
#because by doing this you are creating a model that says that the response function must be 
#zero when the predictors are all zero. If the nature of what you are modeling is such that 
#you want to assume this, then you might want to remove the intercept. This can usually be done by adding a NOINT option.
#From: http://support.sas.com/kb/23/136.html
#So, since it's logical that if the run size is zero, the expansion is zero, then we should leave the model as-is.


#Ok so let's look at the deviates from the model and see what is left:

plot(log(expansion)~log(run_size),data=run_data, main="log Expansion vs log Run Size", bty="l")
abline(fit,col="red")

run_n_exp<-data.frame(log(run_data$run_size),log(run_data$expansion))
colnames(run_n_exp)<-c("logRS","logExp")

fit.res<-resid(fit)
#Residuals = observed - fitted

run_n_exp$residuals<-fit.res

run_n_exp$year<-run_data$year


plot(run_n_exp$logRS, fit.res, 
    ylab="Residuals", xlab="logRS", 
    main="Residuals of lm between logExp and logRS") 
abline(0, 0) 

hist(fit.res, breaks=20, nclass="FD", main="Histogram of residuals")
#Not normally or maybe even lognormally distributed....

plot(residuals~year,data=run_n_exp, main="Annual Residuals",bty="l")
abline(0,0)

#This also shows something similar
acf(fit.res)

#See https://www.otexts.org/fpp/2/6
#Formal test for autocorrelation
# lag=h and fitdf=K
Box.test(fit.res, lag=4, fitdf=1)
#Tried lag at 2,5, and 10, did not get a significant result. So I guess that's good.

#Very interesting.... it looks as though the residuals have been mostly negative since about the year 2000,
#indicating the expansion has been lower than we would expect given the run size, which means the catch has
#been higher than expected given the run size

RS_tseries <- ts(run_n_exp$residuals, start = c(1980, 1), frequency = 1)
print(RS_tseries)
plot(RS_tseries,main="Annual Residuals",bty="l")

mvalue = cpt.mean(RS_tseries, method="PELT") #mean changepoints using PELT
cpts(mvalue)

mvalue = cpt.mean(RS_tseries, method="BinSeg")
cpts(mvalue)

#This didn't really result in anything - wondering if it works very well with negative values?

#Going back to the change in residuals - would like to look at the water quality data, but need to get the data
#prior to 2008 together and then get some annual metrics (such as annual max/min/means)
#e.g. function from wq_exploration file: aggregate(temp~year,data=WQ_data,FUN=max)

#Explore this later
plot(spread~peak,data=run_data, main="Spread vs Peak", bty="l")
