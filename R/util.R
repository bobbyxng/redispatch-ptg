### Helper functions

### Read result files and append to result dataframe from previous period.
### Useful for model runs that are split over multiple periods.

multiCSVtoDF <- function(csvFile, dir = session_dir) {
  
  csvDataFrame <- data.frame()
  
  for (s in 1:length(dir)) {
    temp <- fread(file = paste(dir[s], csvFile, sep = ""))
    csvDataFrame <- rbind(csvDataFrame, temp)  
  }
  
  return(csvDataFrame)
  
}

multiCSVtoDFtype <- function(csvFile, dir = session_dir) {
  
  csvDataFrame <- data.frame()
  
  for (s in 1:length(dir)) {
    temp <- fread(file = paste(dir[s], csvFile, sep = ""))
    temp$case <- dir[s]
    csvDataFrame <- rbind(csvDataFrame, temp)  
  }
  
  return(csvDataFrame)
  
}

saveWidgetFix <- function(widget, file, ...) {
  ## A wrapper to saveWidget which compensates for arguable BUG in
  ## saveWidget which requires `file` to be in current working
  ## directory.
  wd <- getwd()
  on.exit(setwd(wd))
  outDir <- dirname(file)
  file <- basename(file)
  setwd(outDir);
  saveWidget(widget, file = file,...)
}
