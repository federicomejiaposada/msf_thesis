#Federico Mejía-Posada - Diana Constanza Restrepo-Ochoa
#Algorithm used in the research "Do investors care about terrorist attacks and peace in Colombia?"
#MSc in Finance Thesis for Federico Mejía-Posada

library(rugarch)
library(stats)
library(readxl)
library(dplyr)

#####
#Set working directory
WorkingDirectory = "C:/Users/Earendil/Google Drive/01_MSc_Finance/001_Thesis_proposal/Programs/"
setwd(WorkingDirectory)

#####
#Clear Global Environment and clean console
rm(list=ls()) # Clean GlobalEnviroment
cat("\014") #Clean console

#####
#Define Global variables

#Following Mnasri & Nechi 2016, p189
ESTIMATION_WINDOW_LENGTH = 500
#[1, 20], [1, 5], [1, 3], [1, 2], [1, 1], [1, 10],
#[11, 20], [-10,-1], [-1,-1]
#event window lenght from 0 to 20 / offset number from -10 to 10
# When 0, it means exactly before the event, thus [1,x] in the event window lenght
EVENT_WINDOW_LENGTH = 5
EVENT_WINDOW_OFFSET = 0

######
#Function to compute and fit GARCH(1,1) model with mean model as the market model
#Following Mnasri & Nechi 2016, p189, equations (1) and (2)
# data: list with the data to be fitted.
# externalRegressor: list with external regressor data.
computeGarch = function(data, externalRegressor) {
  spec = ugarchspec(mean.model = list(armaOrder=c(0,0), archpow=2, include.mean = TRUE,
                                      external.regressors=as.matrix(externalRegressor)))
  
  fit = ugarchfit(spec, data, solver="hybrid")
  
  alpha = fit@fit[["coef"]][["mu"]]
  beta =  fit@fit[["coef"]][["mxreg1"]]
  gamma0 = fit@fit[["coef"]][["omega"]]
  gamma1 = fit@fit[["coef"]][["beta1"]]
  gamma2 = fit@fit[["coef"]][["alpha1"]]
  residuals = fit@fit[["residuals"]]
  lastResidual = tail(residuals, n=1)
  conditionalVariances = fit@fit[["sigma"]]
  lastConditionalVariance = tail(conditionalVariances, n=1)
  matcoef = fit@fit[["matcoef"]]
  infocriteria = infocriteria(fit)
  loglikelihood = fit@fit[["LLH"]]
  
  return(list(garchfit=fit, alpha=alpha, beta=beta,
              gamma0=gamma0, gamma1=gamma1, gamma2=gamma2,
              residuals=residuals, lastResidual=lastResidual,
              conditionalVariances=conditionalVariances, lastConditionalVariance=lastConditionalVariance,
              matcoef=matcoef, infocriteria=infocriteria, loglikelihood=loglikelihood))
}

#####
#Function estimateEvent to run computeGARCH on it with the addequate estimation window and event window
#Following Mnasri & Nechi 2016, p189
estimateEvent = function(returnsData, eventDate) {
  for (i in 1:nrow(returnsData)) {
    if (returnsData[[i, "DATE"]] == eventDate) {
      print(paste("Found event", eventDate, "on row i =", i))
      
      #Definition of event window
      eventWindowFrom = i + EVENT_WINDOW_OFFSET
      eventWindowTo = eventWindowFrom + EVENT_WINDOW_LENGTH - 1
      
      #Definition of estimation window
      estimationWindowTo = eventWindowFrom - 1
      estimationWindowFrom = estimationWindowTo - ESTIMATION_WINDOW_LENGTH + 1
      
      #Data to use in computeGarch with the appropiate estimation window respect to the event i
      garchData = returnsData[estimationWindowFrom:estimationWindowTo,]
      
      if (nrow(garchData) != ESTIMATION_WINDOW_LENGTH) {
        stop("unexpected size of estimation window data")
      }
      
      #print(garchData)
      #Separate data to input in computeGarch function
      fitData = as.data.frame(garchData[,2])
      regressorData = as.data.frame(garchData[,3])
      
      #Garch results after fiting GARCH with the function computeGarch
      #Following Mnasri & Nechi 2016, p189, equations (1) and (2)
      garchResult = computeGarch(fitData, regressorData)
      
      #####
      # assign(paste("garchResult", i, sep = ""), garchResult$garchfit, envir = .GlobalEnv)
      # print("GARCH fit results")
      # print(garchResult$garchfit)
      print("Coeficient matrix")
      print(garchResult$matcoef)
      print("Information criteria")
      print(garchResult$infocriteria)
      print("LogLikelihood")
      print(garchResult$loglikelihood)
      
      #Create vector abnormalReturns to fill it
      abnormalReturns = double()
      
      #Compute the abnormal returns
      #Following MacKinlay 1997, p20, eq (7)
      for (k in eventWindowFrom:eventWindowTo) {
        # returnsData[[k, 2]] <= term Y
        # returnsData[[k, 3]] <= term X
        abnormalReturn = returnsData[[k, 2]] - garchResult$alpha - garchResult$beta * returnsData[[k, 3]]
        
        abnormalReturns = c(abnormalReturns, abnormalReturn)
      }
      
      return(list(abnormalReturns=abnormalReturns,
                  gamma0=garchResult$gamma0, gamma1=garchResult$gamma1, gamma2=garchResult$gamma2,
                  lastResidual=garchResult$lastResidual, sigmaResiduals=garchResult$sigmaResidual,
                  lastConditionalVariance=garchResult$lastConditionalVariance))
    }
  }
  stop(paste("failed to find event", eventDate))
}

#####
# Returns a range to iterate over covering the interval [from, to] both inclusive.
# If from > to, returns an empty range.
safeRange = function(from, to) {
  if (from > to) return(c())
  return(from:to)
}

#####
##Function to compute the Expected Conditional Volatility
#Mnasri & Nechi 2016, p189 equation (3), Essaddam & Mnasri 2015, p210, eq (3), Bialkowski 2008, p1942, eq (3)
computeExpectedConditionalVolatility = function(k, gamma0, gamma1, gamma2, lastConditionalVariance, lastResidual) {
  if (k == 1){
    COMPUTEDExpectedConditionalVolatility = gamma0 + gamma1 * lastConditionalVariance + gamma2 * lastResidual ** 2
  }
  else {
    sum = 0.0
    for (j in safeRange(1, k-1)) {
      sum = sum + ((gamma1 + gamma2) ** j)
    }
    
    term1 = gamma0 * sum
    term2 = (gamma1 + gamma2) ** (k - 1) * gamma1 * lastConditionalVariance
    term3 = (gamma1 + gamma2) ** (k - 1) * gamma2 * lastResidual ** 2
    
    COMPUTEDExpectedConditionalVolatility = term1 + term2 + term3
  }
  return(COMPUTEDExpectedConditionalVolatility)
}

#####
#Function to compute the Multiplicative Effect on Volatility
#Following Mnasri & Nechi 2016, p190, eq (5), Essaddam & Mnasri 2015, p210, eq (5),
#Bialkowski et al 2008, p1942, eq (5), Hilliard & Savickas 2002, p450, eq (3)
computeMultiplicativeEffectOnVolatility = function(N, k, abnormalReturns, expectedConditionalVolatility) {
  outer_sum = 0.0
  for (i in safeRange(1,N)) { # big sum
    top_sum = 0
    for (j in safeRange(1, N)){
      top_sum = top_sum + abnormalReturns[k, j]
    }
    numerator = (N * abnormalReturns[k, i] - top_sum) ** 2
    
    bottom_sum = 0
    for (j in safeRange(1, N)){
      bottom_sum = bottom_sum + expectedConditionalVolatility[k, j]
    }
    denominator = N * (N - 2) * expectedConditionalVolatility[k, i] + bottom_sum
    
    outer_sum = outer_sum + numerator / denominator
  }
  return(1 + (outer_sum / as.double(N - 1))) #Add +1 as it is thought that this Multiplicative Effect on Volatility is the difference between 1 and the real Mt
}

#####
#Function to obtain CARs and CAV of an event
# returnsFile: string with path to the file with returns data.
# eventFile: string with path to the file that contains the events.
# termY: string with the left-side term (e.g. "COLCAP")
# termX: string with the right-side term (e.g "IBOV")
analyzeEvents = function(returnsFile, eventsFile, termY, termX) {
  #Load data as dataset 
  returnsData <<- read_excel(returnsFile)[,c("DATE", termY, termX)]
  eventDates = read_excel(eventsFile)[[1]]
  
  N = length(eventDates) # Number of events
  print(paste("Number of events:", N))
  #Create matrix full of zeros to assign ARs (rows=day in event window, cols=event )
  abnormalReturns = matrix(0, EVENT_WINDOW_LENGTH, N)
  #Create vector of Average AR 
  averageAbnormalReturns = double(EVENT_WINDOW_LENGTH)
  expectedConditionalVolatility = matrix(0, EVENT_WINDOW_LENGTH, N)
  MultiplicativeEffectOnVolatility = double(EVENT_WINDOW_LENGTH)
  
  
  for (i in 1:N) {
    date = eventDates[i]
    print(paste("Estimating event", date))
    
    ##Calls function estimateEvent which returns AR's and estimators of the GARCH fit process for each i event
    eventResult = estimateEvent(returnsData, date)
    
    if (length(eventResult$abnormalReturns) != EVENT_WINDOW_LENGTH) {
      stop(paste("unexpected length of abnormal returns for event", date))
    }
    # Compute contribution from this event to global average Abnormal Returns for each event window day k.
    #Following MacKinlay 1997, p24, eq (13)
    for (k in 1:EVENT_WINDOW_LENGTH) {
      abnormalReturns[k, i] = eventResult$abnormalReturns[k]
      averageAbnormalReturns[k] = averageAbnormalReturns[k] + eventResult$abnormalReturns[k] / as.double(length(eventDates))
    }
    
    #Calls function to compute expected conditional volatility for the i event.
    for (k in 1:EVENT_WINDOW_LENGTH) {
      expectedConditionalVolatility[k, i] = computeExpectedConditionalVolatility(
        k,
        gamma0=eventResult$gamma0,
        gamma1=eventResult$gamma1,
        gamma2=eventResult$gamma2,
        lastConditionalVariance=eventResult$lastConditionalVariance,
        lastResidual=eventResult$lastResidual)
    }
  }
  #####
  # print("Expected conditional volatility (rows=day in event window, cols=event)")
  # print(expectedConditionalVolatility)
  expectedConditionalVolatility <<- expectedConditionalVolatility
  
  # print("Abnormal returns (rows=day in event window, cols=event):")
  # print(abnormalReturns)
  abnormalReturns <<- as.data.frame(abnormalReturns)
  
  # print("Average abnormal returns:")
  # print(averageAbnormalReturns)
  averageAbnormalReturns <<- as.data.frame(averageAbnormalReturns)
  
  #####
  # Compute AVERAGE cumulative abnormal return.
  #Following MacKinlay 1997, p24, eq (15)
  cumulativeAbnormalReturn = 0.0
  for (k in 1:EVENT_WINDOW_LENGTH) {
    cumulativeAbnormalReturn = cumulativeAbnormalReturn + averageAbnormalReturns[k] #look AVERAGE because averageAbnormalReturns
  }
  print("Cumulative abnormal return:")
  print(cumulativeAbnormalReturn)
  cumulativeAbnormalReturn <<- as.data.frame(cumulativeAbnormalReturn)
  
  #####
  # Compute variance of cumulative abnormal return
  #Following MacKinlay 1997, p28, eq (21)
  sum = 0
  for (i in 1:N) {
    leftinnersum = 0
    for (k in 1:EVENT_WINDOW_LENGTH) {
      leftinnersum = leftinnersum + abnormalReturns[k,i]
    }
    sum = sum + (leftinnersum - cumulativeAbnormalReturn) ** 2
  }
  varianceCAR = sum / (N ** 2)
  print("Variance of cumulative abnormal return:")
  print(varianceCAR)
  
  #Compute theta statistic for abnormal returns
  #Following MacKinlay 1997, p24, eq (20)
  theta1 = cumulativeAbnormalReturn / (varianceCAR ** 0.5)
  print("Theta1:")
  print(theta1)
  
  #Compute p-value assuming normal distribution for Cumulative Abnormal Return (CAR)
  if (theta1 < 0) {
    pvalCAR = pnorm(theta1)
  }
  else {
    pvalCAR = pnorm(theta1, lower.tail = FALSE)
  }
  pvalCAR = pvalCAR * 2
  print("P Value of theta1 ~N(0,1)")
  print(pvalCAR)
  pvalCAR <<- as.data.frame(pvalCAR)
  
  #Compute the Multiplicative Effect on Volatility of the events during the event window lenght
  #Following same authors described in the function computeMultiplicativeEffectOnVolatility
  sumM = 0
  for (k in 1:EVENT_WINDOW_LENGTH) {
    
    MultiplicativeEffectOnVolatility[k] =
      computeMultiplicativeEffectOnVolatility(N, k, abnormalReturns, expectedConditionalVolatility)
    
    sumM = sumM + MultiplicativeEffectOnVolatility[k]
  }
  
  print("Multiplicative effect on volatility")
  print(MultiplicativeEffectOnVolatility)
  MultiplicativeEffectOnVolatility <<- MultiplicativeEffectOnVolatility
  
  print("Sum Mt")
  print(sumM)
  
  #Compute the Cumulative Abnormal Volatility of all events during the whole event window
  #Following Mnasri & Nechi 2016, p190, eq (6) and same mentioned authors
  cumulativeAbnormalVolatility = sumM - EVENT_WINDOW_LENGTH
  print("cumulative abnormal volatility:")
  print(cumulativeAbnormalVolatility)
  cumulativeAbnormalVolatility <<- as.data.frame(cumulativeAbnormalVolatility)
  
  #Compute phi statistic for Cumulative Abnormal Volatility (CAV)
  #Following Mnasri & Nechi 2016, p190, eq (9)
  phi = (N - 1) * sumM
  print("Phi:")
  print(phi)
  phi <<- phi
  
  #Compute p-value assuming chi squared distribution for Cumulative Abnormal Volatility
  #Following Mnasri & Nechi 2016, p190, eq (9)
  pvalCAV = pchisq(phi,((N-1)*EVENT_WINDOW_LENGTH), lower.tail = FALSE)
  print("P Value of phi ~ Chi Sq with (N-1)*(Event window lenght) degrees of freedom")
  print(pvalCAV)
  pvalCAV <<- as.data.frame(pvalCAV)
}

#####
#Function to export the main results needed for the article
#DataToExport is any R object
#AssignedTermY, AssignedTermX, BaseEventsFileName (e.g. "COLCAP", "IBOV", "bogota")
#EVENT_WINDOW_OFFSET, EVENT_WINDOW_LENGTH are values (e.g. 0, 10)
ExportResults = function(DataToExport, AssignedTermY, AssignedTermX, BaseEventsFileName,
                         EVENT_WINDOW_OFFSET, EVENT_WINDOW_LENGTH, ExportingDirectory) {
  setwd(ExportingDirectory)
  
  #Add a new column with the description of the specific model
  type=as.data.frame(paste(AssignedTermY,"~",AssignedTermX,"_",BaseEventsFileName,"_",
                           "(",EVENT_WINDOW_OFFSET,",", EVENT_WINDOW_OFFSET + EVENT_WINDOW_LENGTH,")", sep = ""))
  colnames(type) <- "type"
  
  #Bind data to export with description in one data frame
  ResultsToExport <<- bind_cols(DataToExport, type)
  
  #Export .txt with the information
  write.table(ResultsToExport,
              file = paste(AssignedTermY,"~",AssignedTermX,"_",BaseEventsFileName,"_",
                           "(",EVENT_WINDOW_OFFSET,",", EVENT_WINDOW_OFFSET + EVENT_WINDOW_LENGTH,").txt", sep=""),
              quote = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE)

  # write.csv(ResultsToExport,
  #                 file = paste(AssignedTermY,"-",AssignedTermX,"_",BaseEventsFileName,"_",
  #                              "(",EVENT_WINDOW_OFFSET,"-", EVENT_WINDOW_OFFSET + EVENT_WINDOW_LENGTH,").txt", sep=""),
  #                 row.names = FALSE)
  
  #Print notification of succes
  print(paste("Archive named ", AssignedTermY, "~", AssignedTermX, "_", BaseEventsFileName, "_",
              "(", EVENT_WINDOW_OFFSET, ",", EVENT_WINDOW_OFFSET + EVENT_WINDOW_LENGTH, 
              ").txt has been created in the directory ", ExportingDirectory, sep=""))
}

#####
#Define the data directory and the data returns file
DataDirectory = "C:/Users/Earendil/Google Drive/01_MSc_Finance/001_Thesis_proposal/Programs/Data_for_R/"
returnsFileName = "data_returns.xlsx"
returnsFile = paste(DataDirectory, returnsFileName, sep="")
#Set Exporting Directory
PrintDirectory = "C:/Users/Earendil/Google Drive/01_MSc_Finance/001_Thesis_proposal/Programs/ExportedResults/"

#####
## **CHANGE THE FOLLOWING TO EVALUATE DIFFERENT EVENTS AND MARKET MODELS**

#Change this to evaluate different type of events
#Possible BaseEventsFileNames:  peace, bogota, money, oil,
#electricity, bigcities, international, ecopetrol, isa
BaseEventsFileName = "elec2"

eventsFileName = paste(BaseEventsFileName, ".xlsx", sep = "")
eventsFile = paste(DataDirectory, eventsFileName, sep = "")

#Change these two variables to evaluate different market models
#Possible TermY's: colcap, ecopetl, gruposur,
#isa, celsia, grupoarg, etb  
#Posible TermX's: spx, ibov, colcap 
AssignedTermY="celsia"
AssignedTermX="colcap"

#####
#Print the specific model and window event
cat("\014") #Clean console
print("Specific model and window event")
print(paste(AssignedTermY, "~", AssignedTermX, "_", BaseEventsFileName, "_",
"(", EVENT_WINDOW_OFFSET, ",", EVENT_WINDOW_OFFSET + EVENT_WINDOW_LENGTH, 
")", sep=""))

#Run the functions to analyze the events
analyzeEvents(returnsFile, eventsFile, AssignedTermY, AssignedTermX)

#Export results - ALWAYS keep commented unless printing needed

#Combine relevant results to be exported in one dataframe
ResultsToExport=bind_cols(cumulativeAbnormalReturn, pvalCAR, cumulativeAbnormalVolatility, pvalCAV)

#Export results in .txt
ExportResults(ResultsToExport, AssignedTermY, AssignedTermX, BaseEventsFileName,
              EVENT_WINDOW_OFFSET, EVENT_WINDOW_LENGTH, PrintDirectory)
