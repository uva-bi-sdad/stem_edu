library(DataExplorer)

melt(DataExplorer::introduce(ads_cip_0717))
DataExplorer::profile_missing(ads_cip_0717)

### Uniqueness of Unique ID

#IDs that appear more than once
dupejobids <- ads_cip_0717 %>% dplyr::group_by(bgtjobid) %>% dplyr::summarise(count = dplyr::n()) %>% dplyr::filter(count > 1)

#Multi-appearance ID by date
dupecount <- ads_cip_0717[bgtjobid %in% dupejobids$bgtjobid, c("bgtjobid", "jobdate")] %>% group_by(bgtjobid, jobdate) %>% summarise(count = n())

nonuniqueIDs <- nrow(dupejobids) - nrow(dupecount)
print(paste(nonuniqueIDs), "unique job ids have multiple dates associated with them.")


### Histograms
# datatable[Filter, Select, BY]
### Job Date
values <-  ads_cip_0717[!is.na(lubridate::as_date(jobdate)), lubridate::as_date(jobdate)]
unique_values <- length(unique(values))
frequencies <- as.data.table(table( values))
ggplot(frequencies, aes(values, N)) +
  geom_bar(stat = "identity", fill ="paleturquoise4", width=.7) +
  ggtitle(paste("Job Date", "Value Distribution")) +
  xlab("Job Date") +
  ylab("N") +
  theme(panel.background=element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
### CIP
values <-  ads_cip_0717[!is.na(as.numeric(cip)), as.numeric(cip)]
unique_values <- length(unique(values))
frequencies <- as.data.table(table( values))
ggplot(frequencies, aes(as.integer(values), N)) +
  geom_bar(stat = "identity", fill ="paleturquoise4", width=.7) +
  ggtitle(paste("CIP", "Value Distribution")) +
  xlab("CIP") +
  ylab("N") +
  theme(panel.background=element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
### Salary
values <-  ads_cip_0717[salary >0, salary]
unique_values <- length(unique(values))
frequencies <- as.data.table(table( values))
ggplot(frequencies, aes(as.integer(values)/1000, N)) +
  geom_bar(stat = "identity", fill ="paleturquoise4", width=.7) +
  ggtitle(paste("Salary", "Value Distribution")) +
  xlab("Salary") +
  ylab("N") +
  theme(panel.background=element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
