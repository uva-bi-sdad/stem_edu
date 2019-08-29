library(data.table)
library(sdalr)
library(DBI)
library(magrittr) # pipes
library(readr) #read_tsv

## CREATE FOLDER PATHS
data.dir = "./data/stem_edu/original/Burning_Glass_Data/"
folderNames = c("Certs", "CIP", "Main", "Skill", "Degree", "Major")
year = as.character(c(2007, 2010:2017))

paths = expand.grid(data.dir, folderNames, "/", year)
paths <- paths %>%
  dplyr::mutate(path = stringr::str_c(Var1, Var2, Var3, Var4),
                path2 = stringr::str_c(path, "/"),
                table = Var2) %>%
  dplyr::select(path,path2, table)

testthat::expect_equal(nrow(paths), 54)

## CREATE FILE NAMES
zip_files <- purrr::map(paths$path, function(x){
  zip_files = list.files(x, full.names = TRUE)
  zip_files
})  %>%
  unlist() %>%
  tibble::tibble(zip_p = .) %>%
  dplyr::mutate(save_p = stringr::str_extract(zip_p, '.+(?=/.*\\.zip)/')) %>%
  dplyr::left_join(paths, by = c('save_p' = "path2"))

nrow(zip_files)
testthat::expect_true(nrow(zip_files) == 649)

## SPLIT ZIP FILES BY TABLE NAME
zip_split <- split(zip_files$zip_p, zip_files$table)
zip_main <- unlist(zip_split$Main)

zip_mains <- split(zip_main, rep(1:9,each=12))

# Connect to database
con <- con_db(dbname = "burning_glass", host = "127.0.0.1", port = 5433, user = "dtn2ep", pass = "dtn2ep")

# Data Structure

datatypes <- c(
  bgtjobid = "character(30)",
  jobid = "character(40)",
  jobdate = "date",
  cleantitle = "text",
  canontitle = "text",
  occfam = "integer",
  occfamname = "text",
  soc = "character(7)",
  socname = "text",
  onet = "character(10)",
  onetname = "text",
  bgtocc = "character(10)",
  bgtoccname = "text",
  bgtoccgroupname = "text",
  bgtoccgroupname2 = "text",
  bgtcareerareaname  = "text",
  bgtcareerareaname2 = "text",
  employer = "text",
  sector = "character(7)",
  sectorname = "text",
  naics3 = "integer",
  naics4 = "integer",
  naics5 = "integer",
  naics6 = "integer",
  city = "text",
  state = "character(30)",
  county = "text",
  fipsstate  =  "integer",
  fipscounty = "integer",
  fips = "integer",
  lat = "numeric",
  lon = "numeric",
  bestfitmsa = "integer",
  bestfitmsaname = "text",
  bestfitmsatype = "character(50)",
  msa = "integer",
  msaname = "text",
  edu = "integer",
  maxedu = "integer",
  degree = "character(15)",
  maxdegree = "character(15)",
  exp = "numeric",
  maxexp = "numeric",
  minsalary = "numeric",
  maxsalary = "numeric",
  minhrlysalary = "numeric",
  maxhrlysalary = "numeric",
  payfrequency  = "character(15)",
  salarytype  = "character(20)",
  jobhours  = "character(20)",
  taxterm  = "character(15)",
  internship = "integer",
  subocc = "text"
)

########### FOR LOOPS ##############

### ZIP MAIN
datalist = list()

# for (f in zip_mains$`9`) {
#   print(paste("Reading", f))
#   data <- fread(cmd = paste("zcat", f))
#   data[CleanTitle == "na", CleanTitle := NA]
#   data[CanonTitle == "na", CanonTitle := NA]
#   data[OccFam == "na", OccFam := NA]
#   data[OccFamName == "na", OccFamName := NA]
#   data[CanonTitle == "na", CanonTitle := NA]
#   data[SOC == "na", SOC := NA]
#   data[SOCName == "na", SOCName := NA]
#   data[ONET == "na", ONET := NA]
#   data[ONETName == "na", ONETName := NA]
#   data[BGTOcc == "na", BGTOcc := NA]
#   data[BGTOccName == "na", BGTOccName := NA]
#   data[BGTOccGroupName == "na", BGTOccGroupName := NA]
#   data[BGTOccGroupName2 == "na", BGTOccGroupName2 := NA]
#   data[BGTCareerAreaName == "na", BGTCareerAreaName := NA]
#   data[BGTCareerAreaName2 == "na", BGTCareerAreaName2 := NA]
#   data[Employer == "na", Employer := NA]
#   data[Sector == "na", Sector := NA]
#   data[SectorName == "na", SectorName := NA]
#   data[NAICS3 == "-999", NAICS3 := NA]
#   data[NAICS4 == "-999", NAICS4 := NA]
#   data[NAICS5 == "-999", NAICS5 := NA]
#   data[NAICS6 == "-999", NAICS6 := NA]
#   data[City == "na", City := NA]
#   data[Lat == "-999", Lat := NA]
#   data[Lon == "-999", Lon := NA]
#   data[MSA == "-999", MSA := NA]
#   data[MSAName == "na", MSAName := NA]
#   data[Edu == "-999", Edu := NA]
#   data[MaxEdu == "-999", MaxEdu := NA]
#   data[Degree == "na", Degree := NA]
#   data[MaxDegree == "na", MaxDegree := NA]
#   data[Exp == "-999", Exp := NA]
#   data[MaxExp == "-999", MaxExp := NA]
#   data[MinSalary == "-999", MinSalary := NA]
#   data[MaxSalary == "-999", MaxSalary := NA]
#   data[MinHrlySalary == "-999", MinHrlySalary := NA]
#   data[MaxHrlySalary == "-999", MaxHrlySalary := NA]
#   data[PayFrequency == "na", PayFrequency := NA]
#   data[SalaryType == "na", SalaryType := NA]
#   data[JobHours == "na", JobHours := NA]
#   data[TaxTerm == "na", TaxTerm := NA]
#
#   colnames(data) <- tolower(colnames(data))
#   print(sum(nrow(data)))
#   x <- list()
#   x$f <- nrow(data)
#   datalist[f] <- x
#   print(paste("Writing", f))
#   dbWriteTable(con, "ads_main_2017" , data, row.names = F, append = TRUE,
#                field.types = datatypes)
# }
# y = do.call(rbind, datalist)
# sum(y[,1])
# saveRDS(y, "~/git/stem_edu/data/stem_edu/working/BGexplorevalidate/sizechecks/check2/2017mainsize.RDS")
#
#
#
#  for (f in zip_mains$`1`) {
#    print(paste("Reading", f))
#    data <- fread(cmd = paste("zcat", f))
#    data[CleanTitle == "na", CleanTitle := NA]
#    data[CanonTitle == "na", CanonTitle := NA]
#    data[OccFam == "na", OccFam := NA]
#    data[OccFamName == "na", OccFamName := NA]
#    data[CanonTitle == "na", CanonTitle := NA]
#    data[SOC == "na", SOC := NA]
#    data[SOCName == "na", SOCName := NA]
#    data[ONET == "na", ONET := NA]
#    data[ONETName == "na", ONETName := NA]
#    data[BGTOcc == "na", BGTOcc := NA]
#    data[BGTOccName == "na", BGTOccName := NA]
#    data[BGTOccGroupName == "na", BGTOccGroupName := NA]
#    data[BGTOccGroupName2 == "na", BGTOccGroupName2 := NA]
#    data[BGTCareerAreaName == "na", BGTCareerAreaName := NA]
#    data[BGTCareerAreaName2 == "na", BGTCareerAreaName2 := NA]
#    data[Employer == "na", Employer := NA]
#    data[Sector == "na", Sector := NA]
#    data[SectorName == "na", SectorName := NA]
#    data[NAICS3 == "-999", NAICS3 := NA]
#    data[NAICS4 == "-999", NAICS4 := NA]
#    data[NAICS5 == "-999", NAICS5 := NA]
#    data[NAICS6 == "-999", NAICS6 := NA]
#    data[City == "na", City := NA]
#    data[Lat == "-999", Lat := NA]
#    data[Lon == "-999", Lon := NA]
#    data[MSA == "-999", MSA := NA]
#    data[MSAName == "na", MSAName := NA]
#    data[Edu == "-999", Edu := NA]
#    data[MaxEdu == "-999", MaxEdu := NA]
#    data[Degree == "na", Degree := NA]
#    data[MaxDegree == "na", MaxDegree := NA]
#    data[Exp == "-999", Exp := NA]
#    data[MaxExp == "-999", MaxExp := NA]
#    data[MinSalary == "-999", MinSalary := NA]
#    data[MaxSalary == "-999", MaxSalary := NA]
#    data[MinHrlySalary == "-999", MinHrlySalary := NA]
#    data[MaxHrlySalary == "-999", MaxHrlySalary := NA]
#    data[PayFrequency == "na", PayFrequency := NA]
#    data[SalaryType == "na", SalaryType := NA]
#    data[JobHours == "na", JobHours := NA]
#    data[TaxTerm == "na", TaxTerm := NA]
#
#    colnames(data) <- tolower(colnames(data))
#    print(sum(nrow(data)))
#    x <- list()
#    x$f <- nrow(data)
#    datalist[f] <- x
#    print(paste("Writing", f))
#    dbWriteTable(con, "ads_main_2007" , data, row.names = F, append = TRUE,
#                 field.types = datatypes)
#  }
#  y = do.call(rbind, datalist)
#  sum(y[,1])
#  saveRDS(y, file = "~/git/stem_edu/data/stem_edu/working/BGexplorevalidate/sizechecks/check2/2007mainsize.RDS")
#
#  for (f in zip_mains$`2`) {
#    print(paste("Reading", f))
#    data <- fread(cmd = paste("zcat", f))
#    data[CleanTitle == "na", CleanTitle := NA]
#    data[CanonTitle == "na", CanonTitle := NA]
#    data[OccFam == "na", OccFam := NA]
#    data[OccFamName == "na", OccFamName := NA]
#    data[CanonTitle == "na", CanonTitle := NA]
#    data[SOC == "na", SOC := NA]
#    data[SOCName == "na", SOCName := NA]
#    data[ONET == "na", ONET := NA]
#    data[ONETName == "na", ONETName := NA]
#    data[BGTOcc == "na", BGTOcc := NA]
#    data[BGTOccName == "na", BGTOccName := NA]
#    data[BGTOccGroupName == "na", BGTOccGroupName := NA]
#    data[BGTOccGroupName2 == "na", BGTOccGroupName2 := NA]
#    data[BGTCareerAreaName == "na", BGTCareerAreaName := NA]
#    data[BGTCareerAreaName2 == "na", BGTCareerAreaName2 := NA]
#    data[Employer == "na", Employer := NA]
#    data[Sector == "na", Sector := NA]
#    data[SectorName == "na", SectorName := NA]
#    data[NAICS3 == "-999", NAICS3 := NA]
#    data[NAICS4 == "-999", NAICS4 := NA]
#    data[NAICS5 == "-999", NAICS5 := NA]
#    data[NAICS6 == "-999", NAICS6 := NA]
#    data[City == "na", City := NA]
#    data[Lat == "-999", Lat := NA]
#    data[Lon == "-999", Lon := NA]
#    data[MSA == "-999", MSA := NA]
#    data[MSAName == "na", MSAName := NA]
#    data[Edu == "-999", Edu := NA]
#    data[MaxEdu == "-999", MaxEdu := NA]
#    data[Degree == "na", Degree := NA]
#    data[MaxDegree == "na", MaxDegree := NA]
#    data[Exp == "-999", Exp := NA]
#    data[MaxExp == "-999", MaxExp := NA]
#    data[MinSalary == "-999", MinSalary := NA]
#    data[MaxSalary == "-999", MaxSalary := NA]
#    data[MinHrlySalary == "-999", MinHrlySalary := NA]
#    data[MaxHrlySalary == "-999", MaxHrlySalary := NA]
#    data[PayFrequency == "na", PayFrequency := NA]
#    data[SalaryType == "na", SalaryType := NA]
#    data[JobHours == "na", JobHours := NA]
#    data[TaxTerm == "na", TaxTerm := NA]
#    colnames(data) <- tolower(colnames(data))
#    print(sum(nrow(data)))
#    x <- list()
#    x$f <- nrow(data)
#    datalist[f] <- x
#    print(paste("Writing", f))
#    dbWriteTable(con, "ads_main_2010" , data, row.names = F, append = TRUE,
#                 field.types = datatypes)
#  }
#  y = do.call(rbind, datalist)
#  sum(y[,1])
#  saveRDS(y, file = "~/git/stem_edu/data/stem_edu/working/BGexplorevalidate/sizechecks/check2/2010mainsize.RDS")

 for (f in zip_mains$`3`) {
   print(paste("Reading", f))
   data <- fread(cmd = paste("zcat", f))
   data[CleanTitle == "na", CleanTitle := NA]
   data[CanonTitle == "na", CanonTitle := NA]
   data[OccFam == "na", OccFam := NA]
   data[OccFamName == "na", OccFamName := NA]
   data[CanonTitle == "na", CanonTitle := NA]
   data[SOC == "na", SOC := NA]
   data[SOCName == "na", SOCName := NA]
   data[ONET == "na", ONET := NA]
   data[ONETName == "na", ONETName := NA]
   data[BGTOcc == "na", BGTOcc := NA]
   data[BGTOccName == "na", BGTOccName := NA]
   data[BGTOccGroupName == "na", BGTOccGroupName := NA]
   data[BGTOccGroupName2 == "na", BGTOccGroupName2 := NA]
   data[BGTCareerAreaName == "na", BGTCareerAreaName := NA]
   data[BGTCareerAreaName2 == "na", BGTCareerAreaName2 := NA]
   data[Employer == "na", Employer := NA]
   data[Sector == "na", Sector := NA]
   data[SectorName == "na", SectorName := NA]
   data[NAICS3 == "-999", NAICS3 := NA]
   data[NAICS4 == "-999", NAICS4 := NA]
   data[NAICS5 == "-999", NAICS5 := NA]
   data[NAICS6 == "-999", NAICS6 := NA]
   data[City == "na", City := NA]
   data[Lat == "-999", Lat := NA]
   data[Lon == "-999", Lon := NA]
   data[MSA == "-999", MSA := NA]
   data[MSAName == "na", MSAName := NA]
   data[Edu == "-999", Edu := NA]
   data[MaxEdu == "-999", MaxEdu := NA]
   data[Degree == "na", Degree := NA]
   data[MaxDegree == "na", MaxDegree := NA]
   data[Exp == "-999", Exp := NA]
   data[MaxExp == "-999", MaxExp := NA]
   data[MinSalary == "-999", MinSalary := NA]
   data[MaxSalary == "-999", MaxSalary := NA]
   data[MinHrlySalary == "-999", MinHrlySalary := NA]
   data[MaxHrlySalary == "-999", MaxHrlySalary := NA]
   data[PayFrequency == "na", PayFrequency := NA]
   data[SalaryType == "na", SalaryType := NA]
   data[JobHours == "na", JobHours := NA]
   data[TaxTerm == "na", TaxTerm := NA]
   colnames(data) <- tolower(colnames(data))
   print(sum(nrow(data)))
   x <- list()
   x$f <- nrow(data)
   datalist[f] <- x
   print(paste("Writing", f))
   dbWriteTable(con, "ads_main_2011" , data, row.names = F, append = TRUE,
                field.types = datatypes)
 }
 y = do.call(rbind, datalist)
 sum(y[,1])
 saveRDS(y, file = "~/git/stem_edu/data/stem_edu/working/BGexplorevalidate/sizechecks/check2/2011mainsize.RDS")

datalist = list()
 for (f in zip_mains$`4`) {
   print(paste("Reading", f))
   data <- fread(cmd = paste("zcat", f))
   data[CleanTitle == "na", CleanTitle := NA]
   data[CanonTitle == "na", CanonTitle := NA]
   data[OccFam == "na", OccFam := NA]
   data[OccFamName == "na", OccFamName := NA]
   data[CanonTitle == "na", CanonTitle := NA]
   data[SOC == "na", SOC := NA]
   data[SOCName == "na", SOCName := NA]
   data[ONET == "na", ONET := NA]
   data[ONETName == "na", ONETName := NA]
   data[BGTOcc == "na", BGTOcc := NA]
   data[BGTOccName == "na", BGTOccName := NA]
   data[BGTOccGroupName == "na", BGTOccGroupName := NA]
   data[BGTOccGroupName2 == "na", BGTOccGroupName2 := NA]
   data[BGTCareerAreaName == "na", BGTCareerAreaName := NA]
   data[BGTCareerAreaName2 == "na", BGTCareerAreaName2 := NA]
   data[Employer == "na", Employer := NA]
   data[Sector == "na", Sector := NA]
   data[SectorName == "na", SectorName := NA]
   data[NAICS3 == "-999", NAICS3 := NA]
   data[NAICS4 == "-999", NAICS4 := NA]
   data[NAICS5 == "-999", NAICS5 := NA]
   data[NAICS6 == "-999", NAICS6 := NA]
   data[City == "na", City := NA]
   data[Lat == "-999", Lat := NA]
   data[Lon == "-999", Lon := NA]
   data[MSA == "-999", MSA := NA]
   data[MSAName == "na", MSAName := NA]
   data[Edu == "-999", Edu := NA]
   data[MaxEdu == "-999", MaxEdu := NA]
   data[Degree == "na", Degree := NA]
   data[MaxDegree == "na", MaxDegree := NA]
   data[Exp == "-999", Exp := NA]
   data[MaxExp == "-999", MaxExp := NA]
   data[MinSalary == "-999", MinSalary := NA]
   data[MaxSalary == "-999", MaxSalary := NA]
   data[MinHrlySalary == "-999", MinHrlySalary := NA]
   data[MaxHrlySalary == "-999", MaxHrlySalary := NA]
   data[PayFrequency == "na", PayFrequency := NA]
   data[SalaryType == "na", SalaryType := NA]
   data[JobHours == "na", JobHours := NA]
   data[TaxTerm == "na", TaxTerm := NA]
   colnames(data) <- tolower(colnames(data))
   print(sum(nrow(data)))
   x <- list()
   x$f <- nrow(data)
   datalist[f] <- x
   print(paste("Writing", f))
   dbWriteTable(con, "ads_main_2012" , data, row.names = F, append = TRUE,
                field.types = datatypes)
 }
 y = do.call(rbind, datalist)
 sum(y[,1])
 saveRDS(y, file = "~/git/stem_edu/data/stem_edu/working/BGexplorevalidate/sizechecks/check2/2012mainsize.RDS")

 for (f in zip_mains$`5`) {
   print(paste("Reading", f))
   data <- fread(cmd = paste("zcat", f))
   data[CleanTitle == "na", CleanTitle := NA]
   data[CanonTitle == "na", CanonTitle := NA]
   data[OccFam == "na", OccFam := NA]
   data[OccFamName == "na", OccFamName := NA]
   data[CanonTitle == "na", CanonTitle := NA]
   data[SOC == "na", SOC := NA]
   data[SOCName == "na", SOCName := NA]
   data[ONET == "na", ONET := NA]
   data[ONETName == "na", ONETName := NA]
   data[BGTOcc == "na", BGTOcc := NA]
   data[BGTOccName == "na", BGTOccName := NA]
   data[BGTOccGroupName == "na", BGTOccGroupName := NA]
   data[BGTOccGroupName2 == "na", BGTOccGroupName2 := NA]
   data[BGTCareerAreaName == "na", BGTCareerAreaName := NA]
   data[BGTCareerAreaName2 == "na", BGTCareerAreaName2 := NA]
   data[Employer == "na", Employer := NA]
   data[Sector == "na", Sector := NA]
   data[SectorName == "na", SectorName := NA]
   data[NAICS3 == "-999", NAICS3 := NA]
   data[NAICS4 == "-999", NAICS4 := NA]
   data[NAICS5 == "-999", NAICS5 := NA]
   data[NAICS6 == "-999", NAICS6 := NA]
   data[City == "na", City := NA]
   data[Lat == "-999", Lat := NA]
   data[Lon == "-999", Lon := NA]
   data[MSA == "-999", MSA := NA]
   data[MSAName == "na", MSAName := NA]
   data[Edu == "-999", Edu := NA]
   data[MaxEdu == "-999", MaxEdu := NA]
   data[Degree == "na", Degree := NA]
   data[MaxDegree == "na", MaxDegree := NA]
   data[Exp == "-999", Exp := NA]
   data[MaxExp == "-999", MaxExp := NA]
   data[MinSalary == "-999", MinSalary := NA]
   data[MaxSalary == "-999", MaxSalary := NA]
   data[MinHrlySalary == "-999", MinHrlySalary := NA]
   data[MaxHrlySalary == "-999", MaxHrlySalary := NA]
   data[PayFrequency == "na", PayFrequency := NA]
   data[SalaryType == "na", SalaryType := NA]
   data[JobHours == "na", JobHours := NA]
   data[TaxTerm == "na", TaxTerm := NA]
   colnames(data) <- tolower(colnames(data))
   print(sum(nrow(data)))
   x <- list()
   x$f <- nrow(data)
   datalist[f] <- x
   print(paste("Writing", f))
   dbWriteTable(con, "ads_main_2013" , data, row.names = F, append = TRUE,
                field.types = datatypes)
 }
 y = do.call(rbind, datalist)
 sum(y[,1])
 saveRDS(y, file = "~/git/stem_edu/data/stem_edu/working/BGexplorevalidate/sizechecks/check2/2013mainsize.RDS")

 for (f in zip_mains$`6`) {
   print(paste("Reading", f))
   data <- fread(cmd = paste("zcat", f))
   data[CleanTitle == "na", CleanTitle := NA]
   data[CanonTitle == "na", CanonTitle := NA]
   data[OccFam == "na", OccFam := NA]
   data[OccFamName == "na", OccFamName := NA]
   data[CanonTitle == "na", CanonTitle := NA]
   data[SOC == "na", SOC := NA]
   data[SOCName == "na", SOCName := NA]
   data[ONET == "na", ONET := NA]
   data[ONETName == "na", ONETName := NA]
   data[BGTOcc == "na", BGTOcc := NA]
   data[BGTOccName == "na", BGTOccName := NA]
   data[BGTOccGroupName == "na", BGTOccGroupName := NA]
   data[BGTOccGroupName2 == "na", BGTOccGroupName2 := NA]
   data[BGTCareerAreaName == "na", BGTCareerAreaName := NA]
   data[BGTCareerAreaName2 == "na", BGTCareerAreaName2 := NA]
   data[Employer == "na", Employer := NA]
   data[Sector == "na", Sector := NA]
   data[SectorName == "na", SectorName := NA]
   data[NAICS3 == "-999", NAICS3 := NA]
   data[NAICS4 == "-999", NAICS4 := NA]
   data[NAICS5 == "-999", NAICS5 := NA]
   data[NAICS6 == "-999", NAICS6 := NA]
   data[City == "na", City := NA]
   data[Lat == "-999", Lat := NA]
   data[Lon == "-999", Lon := NA]
   data[MSA == "-999", MSA := NA]
   data[MSAName == "na", MSAName := NA]
   data[Edu == "-999", Edu := NA]
   data[MaxEdu == "-999", MaxEdu := NA]
   data[Degree == "na", Degree := NA]
   data[MaxDegree == "na", MaxDegree := NA]
   data[Exp == "-999", Exp := NA]
   data[MaxExp == "-999", MaxExp := NA]
   data[MinSalary == "-999", MinSalary := NA]
   data[MaxSalary == "-999", MaxSalary := NA]
   data[MinHrlySalary == "-999", MinHrlySalary := NA]
   data[MaxHrlySalary == "-999", MaxHrlySalary := NA]
   data[PayFrequency == "na", PayFrequency := NA]
   data[SalaryType == "na", SalaryType := NA]
   data[JobHours == "na", JobHours := NA]
   data[TaxTerm == "na", TaxTerm := NA]
   colnames(data) <- tolower(colnames(data))
   print(sum(nrow(data)))
   x <- list()
   x$f <- nrow(data)
   datalist[f] <- x
   print(paste("Writing", f))
   dbWriteTable(con, "ads_main_2014" , data, row.names = F, append = TRUE,
                field.types = datatypes)
 }
 y = do.call(rbind, datalist)
 sum(y[,1])
 saveRDS(y, file = "~/git/stem_edu/data/stem_edu/working/BGexplorevalidate/sizechecks/check2/2014mainsize.RDS")


 for (f in zip_mains$`7`) {
   print(paste("Reading", f))
   data <- fread(cmd = paste("zcat", f))
   data[CleanTitle == "na", CleanTitle := NA]
   data[CanonTitle == "na", CanonTitle := NA]
   data[OccFam == "na", OccFam := NA]
   data[OccFamName == "na", OccFamName := NA]
   data[CanonTitle == "na", CanonTitle := NA]
   data[SOC == "na", SOC := NA]
   data[SOCName == "na", SOCName := NA]
   data[ONET == "na", ONET := NA]
   data[ONETName == "na", ONETName := NA]
   data[BGTOcc == "na", BGTOcc := NA]
   data[BGTOccName == "na", BGTOccName := NA]
   data[BGTOccGroupName == "na", BGTOccGroupName := NA]
   data[BGTOccGroupName2 == "na", BGTOccGroupName2 := NA]
   data[BGTCareerAreaName == "na", BGTCareerAreaName := NA]
   data[BGTCareerAreaName2 == "na", BGTCareerAreaName2 := NA]
   data[Employer == "na", Employer := NA]
   data[Sector == "na", Sector := NA]
   data[SectorName == "na", SectorName := NA]
   data[NAICS3 == "-999", NAICS3 := NA]
   data[NAICS4 == "-999", NAICS4 := NA]
   data[NAICS5 == "-999", NAICS5 := NA]
   data[NAICS6 == "-999", NAICS6 := NA]
   data[City == "na", City := NA]
   data[Lat == "-999", Lat := NA]
   data[Lon == "-999", Lon := NA]
   data[MSA == "-999", MSA := NA]
   data[MSAName == "na", MSAName := NA]
   data[Edu == "-999", Edu := NA]
   data[MaxEdu == "-999", MaxEdu := NA]
   data[Degree == "na", Degree := NA]
   data[MaxDegree == "na", MaxDegree := NA]
   data[Exp == "-999", Exp := NA]
   data[MaxExp == "-999", MaxExp := NA]
   data[MinSalary == "-999", MinSalary := NA]
   data[MaxSalary == "-999", MaxSalary := NA]
   data[MinHrlySalary == "-999", MinHrlySalary := NA]
   data[MaxHrlySalary == "-999", MaxHrlySalary := NA]
   data[PayFrequency == "na", PayFrequency := NA]
   data[SalaryType == "na", SalaryType := NA]
   data[JobHours == "na", JobHours := NA]
   data[TaxTerm == "na", TaxTerm := NA]
   colnames(data) <- tolower(colnames(data))
   print(sum(nrow(data)))
   x <- list()
   x$f <- nrow(data)
   datalist[f] <- x
   print(paste("Writing", f))
   dbWriteTable(con, "ads_main_2015" , data, row.names = F, append = TRUE,
                field.types = datatypes)
 }
 y = do.call(rbind, datalist)
 sum(y[,1])
 saveRDS(y, file = "~/git/stem_edu/data/stem_edu/working/BGexplorevalidate/sizechecks/check2/2015mainsize.RDS")


 for (f in zip_mains$`8`) {
   print(paste("Reading", f))
   data <- fread(cmd = paste("zcat", f))
   data[CleanTitle == "na", CleanTitle := NA]
   data[CanonTitle == "na", CanonTitle := NA]
   data[OccFam == "na", OccFam := NA]
   data[OccFamName == "na", OccFamName := NA]
   data[CanonTitle == "na", CanonTitle := NA]
   data[SOC == "na", SOC := NA]
   data[SOCName == "na", SOCName := NA]
   data[ONET == "na", ONET := NA]
   data[ONETName == "na", ONETName := NA]
   data[BGTOcc == "na", BGTOcc := NA]
   data[BGTOccName == "na", BGTOccName := NA]
   data[BGTOccGroupName == "na", BGTOccGroupName := NA]
   data[BGTOccGroupName2 == "na", BGTOccGroupName2 := NA]
   data[BGTCareerAreaName == "na", BGTCareerAreaName := NA]
   data[BGTCareerAreaName2 == "na", BGTCareerAreaName2 := NA]
   data[Employer == "na", Employer := NA]
   data[Sector == "na", Sector := NA]
   data[SectorName == "na", SectorName := NA]
   data[NAICS3 == "-999", NAICS3 := NA]
   data[NAICS4 == "-999", NAICS4 := NA]
   data[NAICS5 == "-999", NAICS5 := NA]
   data[NAICS6 == "-999", NAICS6 := NA]
   data[City == "na", City := NA]
   data[Lat == "-999", Lat := NA]
   data[Lon == "-999", Lon := NA]
   data[MSA == "-999", MSA := NA]
   data[MSAName == "na", MSAName := NA]
   data[Edu == "-999", Edu := NA]
   data[MaxEdu == "-999", MaxEdu := NA]
   data[Degree == "na", Degree := NA]
   data[MaxDegree == "na", MaxDegree := NA]
   data[Exp == "-999", Exp := NA]
   data[MaxExp == "-999", MaxExp := NA]
   data[MinSalary == "-999", MinSalary := NA]
   data[MaxSalary == "-999", MaxSalary := NA]
   data[MinHrlySalary == "-999", MinHrlySalary := NA]
   data[MaxHrlySalary == "-999", MaxHrlySalary := NA]
   data[PayFrequency == "na", PayFrequency := NA]
   data[SalaryType == "na", SalaryType := NA]
   data[JobHours == "na", JobHours := NA]
   data[TaxTerm == "na", TaxTerm := NA]
   colnames(data) <- tolower(colnames(data))
   print(sum(nrow(data)))
   x <- list()
   x$f <- nrow(data)
   datalist[f] <- x
   print(paste("Writing", f))
   dbWriteTable(con, "ads_main_2016" , data, row.names = F, append = TRUE,
                field.types = datatypes)
 }
 y = do.call(rbind, datalist)
 sum(y[,1])
 saveRDS(y, file = "~/git/stem_edu/data/stem_edu/working/BGexplorevalidate/sizechecks/check2/2016mainsize.RDS")







