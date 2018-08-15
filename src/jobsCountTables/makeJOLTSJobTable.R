# Conclusion: use fipscounty. 3271 fipscounties, 410 CBSAs. To compare to Datawork we aggregate JOLTS data to CBSAs.

# Data aggregations needed: for JOLTS, Datawork, and Openjobs, a table with CBSA/County/Month-Yr/#jobs/#stem jobs
library(data.table)
library(stringr)
library(dplyr)
joltsData = fread("./data/stem_edu/original/JOLTS/joltsJobOpenings.txt")

# The first column is a series_id which encodes all the information about the values for the period
# We must parse this column.

parseSeriesId = function(id){
  id = unlist(id)
  seasonal = str_extract(id, "(?<=.{2})(.{1})")
  industry = str_extract(id, "(?<=.{3})(.{6})")
  region = str_extract(id, "(?<=.{9})(.{2})")
  dataType = str_extract(id, "(?<=.{11})(.{2})")
  rateOrLevel = str_extract(id, "(?<=.{13})(.{1})")
  out = cbind(seasonal = seasonal, industry = industry, region = region, dataType = dataType, rateOrLevel = rateOrLevel)
  return(out)
}

parsedJolts = data.table(parseSeriesId(joltsData[,1]), joltsData[,2:4])

filterJolts = function(seas, ind, Region, rate = FALSE, startyear, endyear, startmonth, endmonth){
  rateCode = ifelse(rate, "R", "L")
  activeData = dplyr::filter(parsedJolts, seasonal == as.character(seas) & industry == as.character(ind) & region == as.character(Region) & rateOrLevel == as.character(rateCode))

  startmonth = ifelse(nchar(startmonth) == 1, paste0("M0", startmonth), paste0("M", startmonth))
  endmonth = ifelse(nchar(endmonth) == 1, paste0("M0", endmonth), paste0("M", endmonth))

  startRow = which(activeData$year == startyear & activeData$period == startmonth)
  endRow = which(activeData$year == endyear & activeData$period == endmonth)

  activeData = activeData[startRow:endRow,]

  return(activeData)
}

# Reference: https://download.bls.gov/pub/time.series/jt/jt.txt
# USE THIS FUNCTION

allRegionAllInd = filterJolts(seas = "U", ind = "000000", Region = "00", rate = FALSE, startyear = 2006, endyear = 2013, startmonth = 1, endmonth = 12)
allRegionAllIndSeas = filterJolts(seas = "S", ind = "000000", Region = "00", rate = FALSE, startyear = 2006, endyear = 2013, startmonth = 1, endmonth = 12)
plot(allRegionAllInd$value/1000, ylim = c(0, 6), type = 'l', xlab = 'Time', ylab = "Job Openings (Mil)")
lines(allRegionAllIndSeas$value/1000, xlab = 'Time', col = 'red')
legend("topright", bty = 'n', legend = c("Seasonally Adjusted", "Raw Data"), fill = c("red", "black"))
