package main


import (
  "fmt"
  "log"
  "strconv"
)

const (
  COLCAP = 1
  IBOV = 2

  ESTIMATION_WINDOW_LENGTH = 3 // 500 in real model
  EVENT_WINDOW_LENGTH = 4 // 2 to 10 in real model
  EVENT_WINDOW_OFFSET = 0 // should be in range [-5, 0]. Event window will start on date of the event plus this offset.
)

var data = [][]string{
  {"Date",     "COLCAP", "IBOV", "SPX", "ECOPETROL", "SURA", "ARGOS", "ISA", "CELSIA", "ETB"},
  {"21-07-05",    "0.1",  "0.2", "0.3",       "0.4",  "0.5",   "0.6", "0.7",    "0.8", "0.9"},
  {"22-07-05",    "1.1",  "1.2", "0.3",       "0.4",  "0.5",   "0.6", "0.7",    "0.8", "0.9"},
  {"24-07-05",    "2.1",  "2.2", "0.3",       "0.4",  "0.5",   "0.6", "0.7",    "0.8", "0.9"},
  {"25-07-05",    "3.1",  "3.2", "0.3",       "0.4",  "0.5",   "0.6", "0.7",    "0.8", "0.9"},
  {"26-07-05",    "4.1",  "4.2", "0.3",       "0.4",  "0.5",   "0.6", "0.7",    "0.8", "0.9"},
  {"27-07-05",    "5.1",  "5.2", "0.3",       "0.4",  "0.5",   "0.6", "0.7",    "0.8", "0.9"},
  {"28-07-05",    "6.1",  "6.2", "0.3",       "0.4",  "0.5",   "0.6", "0.7",    "0.8", "0.9"},
  {"29-07-05",    "7.1",  "7.2", "0.3",       "0.4",  "0.5",   "0.6", "0.7",    "0.8", "0.9"},
  {"30-07-05",    "8.1",  "8.2", "0.3",       "0.4",  "0.5",   "0.6", "0.7",    "0.8", "0.9"},
  {"01-08-05",    "9.1",  "9.2", "0.3",       "0.4",  "0.5",   "0.6", "0.7",    "0.8", "0.9"},
  {"02-08-05",    "0.1",  "0.2", "0.3",       "0.4",  "0.5",   "0.6", "0.7",    "0.8", "0.9"},
}

var eventDates = []string{
  "27-07-05",
  "28-07-05",
  "29-07-05",
}

func garch(garchData [][]float64) (alpha, beta, gamma0, gamma1, gamma2 float64, residuals, conditionalVariances []float64) {
  alpha = 1
  beta = 2
  gamma0 = 3
  gamma1 = 4
  gamma2 = 5
  residuals = make([]float64, len(garchData))
  conditionalVariances = make([]float64, len(garchData))
  return alpha, beta, gamma0, gamma1, gamma2, residuals, conditionalVariances
}

func stringToFloat64(s string) float64 {
  if f, err := strconv.ParseFloat(s, 64); err == nil {
    return f
  }
  log.Fatalf("failed to parse %s as float64\n", s)
  return 0.0 // unreachable
}

func estimateEvent(eventDate string) (gamma0, gamma1, gamma2 float64, residuals, abnormalReturns, conditionalVariances []float64) {
  var i int
  for i = 0+1; i < len(data); i++ {
    if (data[i][0] == eventDate) { // data is a global var.
      // Event started on index i.

      eventWindowFrom := i + EVENT_WINDOW_OFFSET
      eventWindowTo := eventWindowFrom + EVENT_WINDOW_LENGTH - 1 // -1 to make it inclusive

      estimationWindowTo := eventWindowFrom - 1
      estimationWindowFrom := estimationWindowTo - ESTIMATION_WINDOW_LENGTH + 1

      fmt.Printf("Estimation window for event %s: ", eventDate)
      fmt.Printf("%s to %s\n", data[estimationWindowFrom][0], data[estimationWindowTo][0])

      fmt.Printf("Event window for event %s: ", eventDate)
      fmt.Printf("%s to %s\n", data[eventWindowFrom][0], data[eventWindowTo][0])


      var garchData [][]float64 // as many columns as defined in the mean model
      for k := estimationWindowFrom; k <= estimationWindowTo; k++ {
        row := []float64{
          stringToFloat64(data[k][COLCAP]), // 1 = COLCAP
          stringToFloat64(data[k][IBOV]),  // 2 = IBOV
        }
        garchData = append(garchData, row)
      }

      alpha, beta, gamma0, gamma1, gamma2, residuals, conditionalVariances := garch(garchData)
      fmt.Printf("alpha = %f\nbeta = %f\ngamma0 = %f\ngamma1 = %f\ngamma2 = %f\nresiduals = %q\n", alpha, beta, gamma0, gamma1, gamma2, residuals)

      var abnormalReturns []float64
      for k := eventWindowFrom; k <= eventWindowTo; k++ {
        abnormalReturn := stringToFloat64(data[k][COLCAP]) - alpha - beta * stringToFloat64(data[k][IBOV])
        abnormalReturns = append(abnormalReturns, abnormalReturn)
      }

      fmt.Printf("\n")
      return gamma0, gamma1, gamma2, residuals, abnormalReturns, conditionalVariances // exits function
    }
  }

  log.Fatalf("failed to find event %s\n", eventDate)
  return
}

func main() {
  averageAbnormalReturns := make([]float64, EVENT_WINDOW_LENGTH)
  for e, eventDate := range eventDates {
    fmt.Printf("Processing event %d on day %s\n", e, eventDate)
    gamma0, gamma1, gamma2, residuals, abnormalReturns, conditionalVariances := estimateEvent(eventDate)

    if (len(abnormalReturns) != EVENT_WINDOW_LENGTH) {
      log.Fatalf("abnormalReturns for event %s have %d elements; want EVENT_WINDOW_LENGTH = %d\n", eventDate, len(abnormalReturns), EVENT_WINDOW_LENGTH)
    }
    for i := 0; i < EVENT_WINDOW_LENGTH; i++ {
      averageAbnormalReturns[i] += abnormalReturns[i] / float64(len(eventDates))
    }

    fmt.Printf("gamma0 = %f\ngamma1 = %f\ngamma2 = %f\nresiduals = %q\nconditionalVariances = %q\n", gamma0, gamma1, gamma2, residuals, conditionalVariances)
  }

  fmt.Printf("\n")
  fmt.Printf("Average Abnormal Returns:\n")
  for i := 0; i < EVENT_WINDOW_LENGTH; i++ {
    fmt.Printf("%f ", averageAbnormalReturns[i])
  }

  cumulativeAbnormalReturn := float64(0.0)
  for i := 0; i < EVENT_WINDOW_LENGTH; i++ {
    cumulativeAbnormalReturn += averageAbnormalReturns[i]
  }
  fmt.Printf("\n")
  fmt.Printf("Cumulative Abnormal Return:\n%f\n", cumulativeAbnormalReturn)

}
