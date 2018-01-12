#Federico Mejía-Posada - Diana Constanza Restrepo-Ochoa
#Algorithm used in the research "Do investors care about terrorist attacks and peace in Colombia?"
#MSc in Finance Thesis for Federico Mejía-Posada
#Algorithm to export results

#####
#Export results

#Combine relevant results to be exported in one dataframe
ResultsToExport=bind_cols(cumulativeAbnormalReturn, pvalCAR, cumulativeAbnormalVolatility, pvalCAV)

#Export results in .txt
ExportResults(ResultsToExport, AssignedTermY, AssignedTermX, BaseEventsFileName,
              EVENT_WINDOW_OFFSET, EVENT_WINDOW_LENGTH, PrintDirectory)
