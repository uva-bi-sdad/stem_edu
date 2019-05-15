cleanme <- function(dataname){

  #SAVE THE ORIGINAL FILE
  oldfile <- write.csv(dataname, file = "oldfile.csv", row.names = FALSE, na = "")

  #CLEAN THE FILE. SAVE THE CLEAN. IMPORT THE CLEAN FILE. CHANGE THE TO A DATAFRAME.
  cleandata <- dataname[complete.cases(dataname),]
  cleanfile <- write.csv(cleandata, file = "cleanfile.csv", row.names = FALSE, na = "")
  cleanfileread <- read.csv(file = "cleanfile.csv")
  cleanfiledata <- as.data.frame(cleanfileread)

  #SUBSETTING THE DATA TO TYPES
  logicmeint <- cleanfiledata[,sapply(cleanfiledata,is.integer)]
  logicmedouble <- cleanfiledata[,sapply(cleanfiledata,is.double)]
  logicmefactor <- cleanfiledata[,sapply(cleanfiledata,is.factor)]
  logicmenum <- cleanfiledata[,sapply(cleanfiledata,is.numeric)]
  mainlogicmefactors <- cleanfiledata[,sapply(cleanfiledata,is.factor) | sapply(cleanfiledata,is.numeric)]

  #VIEW ALL FILES
  View(cleanfiledata)
  View(logicmeint)
  View(logicmedouble)
  View(logicmefactor)
  View(logicmenum)
  View(mainlogicmefactors)

  #describeFast(mainlogicmefactors)

  #ANALYTICS OF THE MAIN DATAFRAME
  cleansum <- summary(cleanfiledata)
  print(cleansum)
  cleandec <- describe(cleanfiledata)
  print(cleandec)

  #ANALYTICS OF THE FACTOR DATAFRAME
  factorsum <- summary(logicmefactor)
  print(factorsum)
  factordec <- describe(logicmefactor)
  print(factordec)

  #ANALYTICS OF THE NUMBER DATAFRAME
  numbersum <- summary(logicmenum)
  print(numbersum)

  numberdec <- describe(logicmefactor)
  print(numberdec)

  mainlogicmefactorsdec <- describe(mainlogicmefactors)
  print(mainlogicmefactorsdec)

  mainlogicmefactorssum <- describe(mainlogicmefactors)
  print(mainlogicmefactorssum)

  #savemenow <- saveRDS("cleanmework.rds")
  #readnow <- readRDS(savemenow)

  #HISTOGRAM PLOTS OF ALL TYPES
  hist(cleanfiledata)
  hist(logicmeint)
  hist(logicmedouble)
  hist(logicmefactor)
  hist(logicmenum)
  #plot(mainlogicmefactors)

  save(cleanfiledata, logicmeint, mainlogicmefactors, logicmedouble, logicmefactor, logicmenum, numberdec, numbersum, factordec, factorsum, cleandec, oldfile, cleandata, cleanfile, cleanfileread,   file = "cleanmework.RData")
}

