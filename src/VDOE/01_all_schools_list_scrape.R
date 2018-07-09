# load(file = "~/git/dashboard/data/VDOE/school_list_final.RData")
#
schools_22 <- c("Bland County High", "Council High","Grundy High","Hurley High","Twin Valley High","Powhatan High",
                "Armstrong High","Franklin Military Academy","George Wythe High","Huguenot High","John Marshall High",
                "Open High","Richmond Community High","Thomas Jefferson High","Patrick Henry High","William Fleming High",
                "Cave Spring High", "Glenvar High","Hidden Valley High","Northside High", "William Byrd High", "Sussex Central High")

districts_22 <- c("Bland County Public Schools","Buchanan County Public Schools","Buchanan County Public Schools","Buchanan County Public Schools","Buchanan County Public Schools",
                  "Powhatan County Public Schools","Richmond City Public Schools","Richmond City Public Schools","Richmond City Public Schools","Richmond City Public Schools","Richmond City Public Schools","Richmond City Public Schools","Richmond City Public Schools","Richmond City Public Schools",
                  "Roanoke City Public Schools","Roanoke City Public Schools","Roanoke County Public Schools","Roanoke County Public Schools","Roanoke County Public Schools","Roanoke County Public Schools","Roanoke County Public Schools", "Sussex County Public Schools")

school_list_22 <- data.frame(schools_22,districts_22)

colnames(school_list_22) <- c("SchoolName","Division")

# library(dplyr)
# school_list_22 <- left_join(school_list_22,school_list_final,by=c("SchoolName","Division"))

write.csv(school_list_22, file = "~/stem_edu/data/stem_edu/final/VDOE/22_schools_list.csv")


# DO WE NEED DISTRICTS????
library(RCurl)
library(XML)
library(rvest)

schools_list <- setNames(data.frame(matrix(ncol = 2, nrow = 1)), c("SchoolName", "Division"))

for (page_num in 1:61)
{
  Sys.sleep(2)
  vdoe_site <- paste("http://schoolquality.virginia.gov/virginia-schools/page/", page_num, sep = "")
  vdoe_html <- read_html(vdoe_site)
  schools_pg <- as.data.frame(vdoe_html %>% html_nodes('td a') %>% html_text())
  divisions_pg <- as.data.frame(vdoe_html %>% html_nodes('td:nth-child(2)') %>% html_text())
  names(schools_pg)[1]<-paste("SchoolName")
  names(divisions_pg)[1] <- paste("Division")

  all_pg <- cbind(schools_pg, divisions_pg)

  schools_list <- rbind(schools_list, all_pg)
}

schools_list <- as.data.frame(schools_list[-1,])

write.csv(schools_list, "~/stem_edu/data/stem_edu/final/VDOE/all_schools_list.csv")
