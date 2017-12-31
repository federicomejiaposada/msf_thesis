#Federico Mejía-Posada - Diana Constanza Restrepo-Ochoa
#Algorithm used in the research "Do investors care about terrorist attacks and peace in Colombia?"
#MSc in Finance Thesis for Federico Mejía-Posada

library(rugarch)
library(stats)
library(readxl)

ESTIMATION_WINDOW_LENGTH = 500
EVENT_WINDOW_LENGTH = 3
EVENT_WINDOW_OFFSET = 0

# data: list with the data to be fitted.
# externalRegressor: list with external regressor data.
computeGarch = function(data, externalRegressor) {
  spec = ugarchspec(mean.model = list(armaOrder=c(0,0), archpow=2, include.mean = TRUE,
                                      external.regressors=as.matrix(externalRegressor)))
  
  fit = ugarchfit(spec, data, solver="hybrid")
  #show(fit)
  
  alpha = fit@fit[["coef"]][["mu"]]
  beta =  fit@fit[["coef"]][["mxreg1"]]
  gamma0 = fit@fit[["coef"]][["omega"]]
  gamma1 = fit@fit[["coef"]][["beta1"]]
  gamma2 = fit@fit[["coef"]][["alpha1"]]
  residuals = fit@fit[["residuals"]]
  conditionalVariances = fit@fit[["sigma"]]
  matcoef= fit@fit[["matcoef"]]

  return(list(alpha=alpha, beta=beta, gamma0=gamma0, gamma1=gamma1,
              gamma2=gamma2, residuals=residuals, conditionalVariances=conditionalVariances,
              matcoef=matcoef))
}

estimateEvent = function(returnsData, eventDate) {
  for (i in 1:nrow(returnsData)) {
    if (returnsData[[i, "DATE"]] == eventDate) {
      print(paste("Found event", eventDate, "on row i =", i))
      
      eventWindowFrom = i + EVENT_WINDOW_OFFSET
      eventWindowTo = eventWindowFrom + EVENT_WINDOW_LENGTH - 1
      
      estimationWindowTo = eventWindowFrom - 1
      estimationWindowFrom = estimationWindowTo - ESTIMATION_WINDOW_LENGTH + 1
      
      garchData = returnsData[estimationWindowFrom:estimationWindowTo,]
      if (nrow(garchData) != ESTIMATION_WINDOW_LENGTH) stop("unexpected size of estimation window data")
  
      #print(garchData)
      fitData = as.data.frame(garchData[,2])
      regressorData = as.data.frame(garchData[,3])
      garchResult = computeGarch(fitData, regressorData)
      #print(garchResult)
      
      abnormalReturns = double()
      for (k in eventWindowFrom:eventWindowTo) {
        abnormalReturn = returnsData[[k, 2]] - garchResult$alpha - garchResult$beta * returnsData[[k, 3]]
        
        abnormalReturns = c(abnormalReturns, abnormalReturn)
      }
      
      return(list(abnormalReturns=abnormalReturns))
    }
  }
  stop(paste("failed to find event", eventDate))
}


# returnsFile: string with path to the file with return data.
# eventFile: string with path to the file that contains the events.
# termY: string with the left-side term (e.g. "COLCAP")
# termX: string with the right-side term (e.g "IBOV")
analyzeEvents = function(returnsFile, eventsFile, termY, termX) {
  #Load data as dataset 
  returnsData = read_excel(returnsFile)[,c("DATE", termY, termX)]
  eventDates = read_excel(eventsFile)[[1]]
  
  averageAbnormalReturns = double(EVENT_WINDOW_LENGTH)
  
  for (i in 1:length(eventDates)) {
    date = eventDates[i]
    print(paste("Estimating event", date))
    eventResult = estimateEvent(returnsData, date)
    if (length(eventResult$abnormalReturns) != EVENT_WINDOW_LENGTH) {
      stop(paste("unexpected length of abnormal returns for event", date))
    }
    
    for (i in 1:EVENT_WINDOW_LENGTH) {
      averageAbnormalReturns[i] = averageAbnormalReturns[i] + eventResult$abnormalReturns[i] / as.double(length(eventDates))
    }
  }
  
  print("Average abnormal returns:")
  print(averageAbnormalReturns)
  
  cumulativeAbnormalReturn = 0.0
  for (i in 1:EVENT_WINDOW_LENGTH) {
    cumulativeAbnormalReturn = cumulativeAbnormalReturn + averageAbnormalReturns[i]
  }
  
  print("Cumulative abnormal return:")
  print(cumulativeAbnormalReturn)

}

directory = "C:/Users/Earendil/Google Drive/01_MSc_Finance/001_Thesis_proposal/Programs/Data_for_R/"
returnsFile = paste(directory, "data_returns.xlsx", sep="")
eventsFile = paste(directory, "bogota.xlsx", sep="")

analyzeEvents(returnsFile, eventsFile, "COLCAP", "IBOV")

