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
  lastResidual = tail(residuals, n=1)
  sigmaResiduals = var(residuals)
  conditionalVariances = fit@fit[["sigma"]]
  lastConditionalVariance = tail(conditionalVariances, n=1)
  matcoef = fit@fit[["matcoef"]]

  return(list(alpha=alpha, beta=beta,
              gamma0=gamma0, gamma1=gamma1, gamma2=gamma2,
              residuals=residuals, lastResidual=lastResidual, sigmaResiduals=sigmaResiduals,
              conditionalVariances=conditionalVariances, lastConditionalVariance=lastConditionalVariance,
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
      if (nrow(garchData) != ESTIMATION_WINDOW_LENGTH) {
        stop("unexpected size of estimation window data")
      }
        
      #print(garchData)
      fitData = as.data.frame(garchData[,2])
      regressorData = as.data.frame(garchData[,3])
      garchResult = computeGarch(fitData, regressorData)
      #print(garchResult)
      
      abnormalReturns = double()
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

# Returns a range to iterate over covering the interval [from, to] both inclusive.
# If from > to, returns an empty range.
safeRange = function(from, to) {
  if (from > to) return(c())
  return(from:to)
}

computeExpectedConditionalVolatility = function(k, gamma0, gamma1, gamma2, lastConditionalVariance, lastResidual) {
  sum = 0.0
  for (j in safeRange(1, k-1)) {
    sum = sum + ((gamma1 + gamma2) ** j)  
  }
  
  term1 = gamma0 * sum
  term2 = (gamma1 + gamma2) ** (k - 1) * gamma1 * lastConditionalVariance
  term3 = (gamma1 + gamma2) ** (k - 1) * gamma2 * lastResidual ** 2
  return(term1 + term2 + term3)
}

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
  return(outer_sum / as.double(N - 1))
  
}

# returnsFile: string with path to the file with return data.
# eventFile: string with path to the file that contains the events.
# termY: string with the left-side term (e.g. "COLCAP")
# termX: string with the right-side term (e.g "IBOV")
analyzeEvents = function(returnsFile, eventsFile, termY, termX) {
  #Load data as dataset 
  returnsData = read_excel(returnsFile)[,c("DATE", termY, termX)]
  eventDates = read_excel(eventsFile)[[1]]
  
  N = length(eventDates) # Number of events
  averageAbnormalReturns = double(EVENT_WINDOW_LENGTH)
  expectedConditionalVolatility = matrix(0, EVENT_WINDOW_LENGTH, N)
  abnormalReturns = matrix(0, EVENT_WINDOW_LENGTH, N)
  
  sigmasResiduals = double(N)
  
  for (i in 1:N) {
    date = eventDates[i]
    print(paste("Estimating event", date))
    eventResult = estimateEvent(returnsData, date)
    
    sigmasResiduals[i] = eventResult$sigmaResiduals
    
    if (length(eventResult$abnormalReturns) != EVENT_WINDOW_LENGTH) {
      stop(paste("unexpected length of abnormal returns for event", date))
    }
    # Compute contribution from this event to global average abnormal returns for each event window day.
    for (k in 1:EVENT_WINDOW_LENGTH) {
      abnormalReturns[k, i] = eventResult$abnormalReturns[k]
      averageAbnormalReturns[k] = averageAbnormalReturns[k] + eventResult$abnormalReturns[k] / as.double(length(eventDates))
    }
    
    # Compute expected conditional volatility.
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
  

  #print("Expected conditional volatility (rows=day in event window, cols=event)")
  #print(expectedConditionalVolatility)
   
  #print("Sigmas residuals:")
  #print(sigmasResiduals)

  #print("Abnormal returns (rows=day in event window, cols=event):")
  #print(abnormalReturns)
  
  print("Average abnormal returns:")
  print(averageAbnormalReturns)
  
  # Compute cumulative abnormal return.
  cumulativeAbnormalReturn = 0.0
  for (i in 1:EVENT_WINDOW_LENGTH) {
    cumulativeAbnormalReturn = cumulativeAbnormalReturn + averageAbnormalReturns[i]
  }
  print("Cumulative abnormal return:")
  print(cumulativeAbnormalReturn)
  
  # Compute variance of cumulative abnormal return.
  sum = 0
  for (k in 1:EVENT_WINDOW_LENGTH) {
    for (i in 1:N) {
      sum = sum + expectedConditionalVolatility[k, i]
    }
  }
  varianceCAR = sum / (N ** 2)
  print("Variance of cumulative abnormal return:")
  print(varianceCAR)
  
  theta1 = cumulativeAbnormalReturn / (varianceCAR ** 0.5)
  print("Theta1:")
  print(theta1)
  
  sumM = 0
  for (k in 1:EVENT_WINDOW_LENGTH) {
    sumM = sumM + computeMultiplicativeEffectOnVolatility(N, k, abnormalReturns, expectedConditionalVolatility)
  }
  cumulativeAbnormalVolatility = sumM - EVENT_WINDOW_LENGTH
  print("cumulative abnormal volatility:")
  print(cumulativeAbnormalVolatility)

  phi = (N - 1) * sumM
  print("Phi:")
  print(phi)
}

directory = "C:/Users/Earendil/Google Drive/01_MSc_Finance/001_Thesis_proposal/Programs/Data_for_R/"
returnsFile = paste(directory, "data_returns.xlsx", sep="")
eventsFile = paste(directory, "money.xlsx", sep="")


analyzeEvents(returnsFile, eventsFile, "COLCAP", "IBOV")

