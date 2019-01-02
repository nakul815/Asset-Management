suppressMessages(library(data.table))
suppressMessages(library(zoo))
suppressMessages(library(moments))
suppressMessages(library(lubridate))

#Question 1
CRSP_Bonds<-fread("C:/Nakul/UCLA Coursework/Spring 2018/QAM/PS_2/4f31512f6a62c031.csv") #Loading downloaded data without edits
#head(CRSP_Bonds)

PS2_Q1 <- function(CRSP_Bonds){
data <- CRSP_Bonds
#head(data)
#head(output)
#Clean up, change -99.0 to NA
data <- data[TMRETNUA == -99.0,TMRETNUA:=NA]

#Sort, get Month, Year, Lag of Outstanding
data$MCALDT<-lubridate::mdy(as.vector(data$MCALDT)) #Get Dates in right format
data[,c("Month","Year"):=.(month(MCALDT),year(MCALDT))]
setorder(data,Year,Month) #Sort on dates
data[,c("LagOut"):=.(shift(TMTOTOUT,1)),by=KYCRSPID] #Lagging market cap

#Calculate Metrics
#Not removing NA lagged outstanding, as we need it for equal weighted
#TMTOTOUT is already in millions
output <- data[,.(Bond_lag_MV = sum(LagOut,na.rm = TRUE),
                  Bond_Ew_Ret = sum(TMRETNUA,na.rm = TRUE)/length(!is.na(TMRETNUA)),
                  Bond_Vw_Ret = sum(TMRETNUA*LagOut, na.rm = TRUE)/sum(LagOut,na.rm = TRUE))
               , by = .(Year,Month)]

output<-output[-1,]
output[Year>=1926 & Year<=2017]
return(output)
}

CRSP_Bonds<-fread("C:/Nakul/UCLA Coursework/Spring 2018/QAM/PS_2/4f31512f6a62c031.csv") #Loading downloaded data without edits

Monthly_CRSP_Bonds<-PS2_Q1(CRSP_Bonds = CRSP_Bonds) #Running Function for output
#head(Monthly_CRSP_Bonds)
#tail(Monthly_CRSP_Bonds)

#Question 2

Monthly_CRSP_Riskless<-fread("C:/Nakul/UCLA Coursework/Spring 2018/QAM/PS_2/e0fe73e8af88ad4d.csv")
#head(Monthly_CRSP_Riskless)
Monthly_CRSP_Stocks<-PS1_Q1(CRSP_Stocks = CRSP_Stocks) #Running Previous Assignment Function

PS2_Q2 <- function(Monthly_CRSP_Stocks,Monthly_CRSP_Bonds,Monthly_CRSP_Riskless){

#Riskfree rate

#Getting right format for dates
dates=as.character(Monthly_CRSP_Riskless$caldt)
years=substr(dates,1,4) 
months=substr(dates,5,6)
days=rep("01",length(dates))
dates_final=paste(years,months,days,sep = "-")
dates=as.Date(dates_final,format = "%Y-%m-%d")

Monthly_CRSP_Riskless$caldt<-dates
Monthly_CRSP_Riskless[,c("Month","Year"):=.(month(caldt),year(caldt))]

#Sorting Dates
setorder(Monthly_CRSP_Riskless,Year,Month)

#Merging all 3 data sets

universe <- merge(Monthly_CRSP_Stocks,Monthly_CRSP_Riskless,by=c("Year","Month"))
universe <- merge(universe,Monthly_CRSP_Bonds,by=c("Year","Month"))
head(universe)

universe<-universe[,.(Year,Month,Stock_lag_MV,Stock_Excess_Vw_Ret=(Stock_Vw_Ret-t30ret),Bond_lag_MV,Bond_Excess_Vw_Ret=(Bond_Vw_Ret-t30ret))]
return(universe[Year>=1926 & Year<=2017])
}

#Running Function for output
Monthly_CRSP_Universe<-PS2_Q2(Monthly_CRSP_Bonds = Monthly_CRSP_Bonds,Monthly_CRSP_Stocks = Monthly_CRSP_Stocks,Monthly_CRSP_Riskless = Monthly_CRSP_Riskless) #Running Function for output
#head(Monthly_CRSP_Universe)

#Question 3

PS2_Q3<-function(Monthly_CRSP_Universe){
CRSP_combined <- Monthly_CRSP_Universe
setorder(CRSP_combined,Year,Month)
#tail(Monthly_CRSP_Universe)

#Calculate value weighted returns of bond and stock
CRSP_combined[,Vw_weight := Stock_lag_MV/(Stock_lag_MV + Bond_lag_MV)]
CRSP_combined[,Excess_Vw_Ret := Vw_weight * Stock_Excess_Vw_Ret + (1-Vw_weight)*Bond_Excess_Vw_Ret]

#Calculate 60-40 portfolio of bond and stock
CRSP_combined[,Excess_60_40_Ret := 0.6 * Stock_Excess_Vw_Ret + 0.4 * Bond_Excess_Vw_Ret]

#Caluculating Stock Inverse Sigma hat and Bond Inverse Sigma hat

CRSP_combined[,c("Stock_inverse_sigma_hat","Bond_inverse_sigma_hat"):=.(1/shift(rollapply(CRSP_combined$Stock_Excess_Vw_Ret,36,sd,fill=NA,align='right')),1/shift(rollapply(CRSP_combined$Bond_Excess_Vw_Ret,36,sd,fill=NA,align='right')))]

#Find k = 1/(stock sig)^-1 + (bond sig)^-1
CRSP_combined[,Unlevered_k := 1/(Stock_inverse_sigma_hat + Bond_inverse_sigma_hat)] 

#Find unlevered beta portfolio returns
CRSP_combined[,Excess_Unlevered_RP_Ret := Unlevered_k*Stock_inverse_sigma_hat*Stock_Excess_Vw_Ret+
            Unlevered_k*Bond_inverse_sigma_hat*Bond_Excess_Vw_Ret] 

#Calculating Levered K

sd_vw<-sd(CRSP_combined[Year>=1929 & (Year<2010 | (Year==2010 & Month<=6))]$Excess_Vw_Ret)
s1<-CRSP_combined[Year>=1929 & (Year<2010 | (Year==2010 & Month<=6))]$Stock_inverse_sigma_hat*CRSP_combined[Year>=1929 & (Year<2010 | (Year==2010 & Month<=6))]$Stock_Excess_Vw_Ret
b1<-CRSP_combined[Year>=1929 & (Year<2010 | (Year==2010 & Month<=6))]$Bond_inverse_sigma_hat*CRSP_combined[Year>=1929 & (Year<2010 | (Year==2010 & Month<=6))]$Bond_Excess_Vw_Ret
sd_unlever<-sd(s1+b1,na.rm = TRUE)

K_lever<-sd_vw/sd_unlever

#Find Levered K portfolio returns
CRSP_combined[,Levered_k:=rep(K_lever,nrow(CRSP_combined))]
CRSP_combined[,Excess_levered_RP_Ret := K_lever*Stock_inverse_sigma_hat*Stock_Excess_Vw_Ret+
                K_lever*Bond_inverse_sigma_hat*Bond_Excess_Vw_Ret] 
#Keep reqiured columns
CRSP_combined[,c("Stock_lag_MV","Bond_lag_MV","Vw_weight"):=NULL]
return(CRSP_combined)
}

#Running Function for output
Port_Rets<-PS2_Q3(Monthly_CRSP_Universe = Monthly_CRSP_Universe)

#Question 4

PS2_Q4 <- function(Port_Rets){
  
  #Restrict data to data range in question
  Port_Rets <- Port_Rets[Year>=1929 & (Year<2010 | (Year==2010 & Month<=6))]
  
  answers <- matrix(ncol=6, nrow=6)
  row.names(answers) <- c("CRSP Stocks","CRSP Bonds","Value-weighted portfolio","60/40 portfolio","unlevered RP","levered RP")
  colnames(answers) <- c("Annualized Mean","t-stat of Annualized Mean","Annualized Standard Deviation","Annualized Sharpe Ratio",
                         "Skewness","Excess Kurtosis")
  answers["CRSP Stocks","Annualized Mean"] <- mean(Port_Rets$Stock_Excess_Vw_Ret,na.rm = TRUE)*12
  answers["CRSP Stocks","Annualized Standard Deviation"] <- sd(Port_Rets$Stock_Excess_Vw_Ret,na.rm = TRUE)*sqrt(12)
  answers["CRSP Stocks","Annualized Sharpe Ratio"] <- answers["CRSP Stocks","Annualized Mean"]/answers["CRSP Stocks","Annualized Standard Deviation"]
  answers["CRSP Stocks","Skewness"] <- skewness(Port_Rets$Stock_Excess_Vw_Ret,na.rm = TRUE)
  answers["CRSP Stocks","Excess Kurtosis"] <- kurtosis(Port_Rets$Stock_Excess_Vw_Ret,na.rm = TRUE)-3
  answers["CRSP Stocks","t-stat of Annualized Mean"] <- t.test(Port_Rets$Stock_Excess_Vw_Ret)$statistic
  
  answers["CRSP Bonds","Annualized Mean"] <- mean(Port_Rets$Bond_Excess_Vw_Ret,na.rm = TRUE)*12
  answers["CRSP Bonds","Annualized Standard Deviation"] <- sd(Port_Rets$Bond_Excess_Vw_Ret,na.rm = TRUE)*sqrt(12)
  answers["CRSP Bonds","Annualized Sharpe Ratio"] <- answers["CRSP Bonds","Annualized Mean"]/answers["CRSP Bonds","Annualized Standard Deviation"]
  answers["CRSP Bonds","Skewness"] <- skewness(Port_Rets$Bond_Excess_Vw_Ret,na.rm = TRUE)
  answers["CRSP Bonds","Excess Kurtosis"] <- kurtosis(Port_Rets$Bond_Excess_Vw_Ret,na.rm = TRUE)-3
  answers["CRSP Bonds","t-stat of Annualized Mean"] <- t.test(Port_Rets$Bond_Excess_Vw_Ret)$statistic
  
  answers["Value-weighted portfolio","Annualized Mean"] <- mean(Port_Rets$Excess_Vw_Ret,na.rm = TRUE)*12
  answers["Value-weighted portfolio","Annualized Standard Deviation"] <- sd(Port_Rets$Excess_Vw_Ret,na.rm = TRUE)*sqrt(12)
  answers["Value-weighted portfolio","Annualized Sharpe Ratio"] <- 
  answers["Value-weighted portfolio","Annualized Mean"]/answers["Value-weighted portfolio","Annualized Standard Deviation"]
  answers["Value-weighted portfolio","Skewness"] <- skewness(Port_Rets$Excess_Vw_Ret,na.rm = TRUE)
  answers["Value-weighted portfolio","Excess Kurtosis"] <- kurtosis(Port_Rets$Excess_Vw_Ret,na.rm = TRUE)-3
  answers["Value-weighted portfolio","t-stat of Annualized Mean"] <- t.test(Port_Rets$Excess_Vw_Ret)$statistic
  
  answers["60/40 portfolio","Annualized Mean"] <- mean(Port_Rets$Excess_60_40_Ret,na.rm = TRUE)*12
  answers["60/40 portfolio","Annualized Standard Deviation"] <- sd(Port_Rets$Excess_60_40_Ret,na.rm = TRUE)*sqrt(12)
  answers["60/40 portfolio","Annualized Sharpe Ratio"] <- answers["60/40 portfolio","Annualized Mean"]/answers["60/40 portfolio","Annualized Standard Deviation"]
  answers["60/40 portfolio","Skewness"] <- skewness(Port_Rets$Excess_60_40_Ret,na.rm = TRUE)
  answers["60/40 portfolio","Excess Kurtosis"] <- kurtosis(Port_Rets$Excess_60_40_Ret,na.rm = TRUE)-3
  answers["60/40 portfolio","t-stat of Annualized Mean"] <- t.test(Port_Rets$Excess_60_40_Ret)$statistic
  
  answers["unlevered RP","Annualized Mean"] <- mean(Port_Rets$Excess_Unlevered_RP_Ret,na.rm = TRUE)*12
  answers["unlevered RP","Annualized Standard Deviation"] <- sd(Port_Rets$Excess_Unlevered_RP_Ret,na.rm = TRUE)*sqrt(12)
  answers["unlevered RP","Annualized Sharpe Ratio"] <- answers["unlevered RP","Annualized Mean"]/answers["unlevered RP","Annualized Standard Deviation"]
  answers["unlevered RP","Skewness"] <- skewness(Port_Rets$Excess_Unlevered_RP_Ret,na.rm = TRUE)
  answers["unlevered RP","Excess Kurtosis"] <- kurtosis(Port_Rets$Excess_Unlevered_RP_Ret,na.rm = TRUE)-3
  answers["unlevered RP","t-stat of Annualized Mean"] <- t.test(Port_Rets$Excess_Unlevered_RP_Ret)$statistic
  
  answers["levered RP","Annualized Mean"] <- mean(Port_Rets$Excess_levered_RP_Ret,na.rm = TRUE)*12
  answers["levered RP","Annualized Standard Deviation"] <-sd(Port_Rets$Excess_levered_RP_Ret,na.rm = TRUE)*sqrt(12)
  answers["levered RP","Annualized Sharpe Ratio"]<-mean(Port_Rets$Excess_levered_RP_Ret,na.rm = TRUE)*1.2/sd(Port_Rets$Excess_levered_RP_Ret,na.rm = TRUE)*sqrt(12)
  answers["levered RP","Skewness"]<-skewness(Port_Rets$Excess_levered_RP_Ret,na.rm = TRUE)
  answers["levered RP","Excess Kurtosis"]<-kurtosis(Port_Rets$Excess_levered_RP_Ret,na.rm = TRUE)-3
  answers["levered RP","t-stat of Annualized Mean"]<-t.test(Port_Rets$Excess_levered_RP_Ret)$statistic
  
  #Formatting
  #mean,sd to % and 2 decimals
  answers[,"Annualized Mean"] <- round(answers[,"Annualized Mean"],4)*100
  answers[,"Annualized Standard Deviation"] <- round(answers[,"Annualized Standard Deviation"],4)*100
  
  answers[,c("Annualized Sharpe Ratio","Skewness","Excess Kurtosis","t-stat of Annualized Mean")] <- 
    round(answers[,c("Annualized Sharpe Ratio","Skewness","Excess Kurtosis","t-stat of Annualized Mean")],2)
  
  return(answers)
}

Final_PS2_Output<-PS2_Q4(Port_Rets = Port_Rets)
Final_PS2_Output
