# US map of STEM jobs across the county by metropolitan statistical areas
# data source - DataAtWork
library(rgdal)
library(maptools)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(reshape2)
library(scales)
library(OpenStreetMap)
library(stplanr)
library(rgeos)
library(acs)
library(viridis)
library(fiftystater)

# download all data at work data by geography
setwd("~/Documents/NSF STEM/DataAtWork/cleaned_geo_title_count/")
# use SOC Common code (this is the code Data at Work used to compare their data to BLS labor market data)
filenames<-list.files(pattern="2016Q1.csv", full.names=TRUE)

for (i in 1: length(filenames)) {

  tmp<-read.csv(filenames[i],stringsAsFactors = F)
  tmp<-tmp[,c(1:5,7,12:21)]

  col_names<-c("cbsa_fips","cbsa_name","state_code","title","soc_code_common_1","soc_code_common_total","skills_1","skills_2","skills_3","skills_4","skills_5","skills_6","skills_7","skills_8",
               "skills_9","skills_10")
  colnames(tmp) <- col_names

  tmp<-dplyr::group_by(tmp,state_code,title,soc_code_common_1,skills_1,skills_2,skills_3,skills_4,skills_5,skills_6,skills_7,skills_8,skills_9,skills_10) %>%
    dplyr::summarize(total=sum(soc_code_common_total))

  tmp$yearQtr<-str_sub(filenames[i],3,8)

  if (i==1) {
    dw<-tmp
  } else {
    dw<-rbind(dw,tmp)
  }
}

setwd("~/Documents/NSF STEM/")

# get total population by state
myacs<-read.acs("ACS_16_5YR_B01003_with_ann.csv",endyear=2016,span=5,geocols="auto")
myacs@acs.colnames
data<-myacs@estimate
data<-as.data.frame(data)
data<-add_rownames(data, "VALUE")

# state appreviations
st<-read.csv("state_appreviations.csv",stringsAsFactors = F)
st_pop<-left_join(data,st,by=c("VALUE"="State"))

# geo data
# cbsa <- readShapePoly("tl_2015_us_cbsa/tl_2015_us_cbsa.shp",
#                         proj4string=CRS('+proj=longlat + ellps=WGS84'))
# state <- readShapePoly("tl_2017_us_state/tl_2017_us_state.shp",
#                 proj4string=CRS('+proj=longlat + ellps=WGS84'))
state2 <- readShapePoly("cb_2016_us_state_20m/cb_2016_us_state_20m.shp",
                        proj4string=CRS('+proj=longlat + ellps=WGS84'))
data("fifty_states")

# state.df = fortify(state2, region="STATEFP")
# state.df = left_join(state.df, state2@data, by=c("id"="STATEFP"))
# rm = c("AK","VI","HI","GU","PR","MP","AS")
# state.df = filter(state.df,!(STUSPS %in% rm))
# state.df$STUSPS<-as.character(state.df$STUSPS)

# link geo to data at work data
dw_state_agg<-group_by(dw,state_code) %>%
  dplyr::summarise(total_job_postings=sum(total))
dw_state_agg<-filter(dw_state_agg,!(state_code %in% rm))
dw_state_agg<-left_join(dw_state_agg,st_pop,by=c("state_code"="Abbreviation"))
dw_state_agg$job_pop<-dw_state_agg$total_job_postings/dw_state_agg$`HD01_VD01.Estimate; Total`

dw_state<-left_join(state.df,dw_state_agg,by=c("STUSPS"="state_code"))

# map states by number of job postings
ggplot() +
  geom_polygon(data=dw_state,aes(long,lat,group=group,fill=job_pop)) +
  geom_path(color="grey70",lwd=.25) +
  scale_fill_viridis(option="magma",direction=-1,name=expression("Job Postings \nper Population")) +
  geom_path(aes(long,lat,group=group),data=state.df,size=.25,color="grey50") +
  coord_equal() +
  theme_bw() +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
        plot.caption = element_text(hjust=0)) + #labels
  labs(x="", y="", caption="Source: Data at Work, University of Chicago, 2016")

# map percent of job postings that are stem jobs by each stem classification system
cw<-read.csv("~/Google Drive/SDAL Google Drive Folders/NSF NCSES Innovation/Team Education /STEM Occupation Classification/crosswalks_clean/occupation_cw.csv",stringsAsFactors = F)
cw$nsf_stem<-ifelse(cw$nsf_se!="Non-S&E and Other","STEM","Non-STEM")
cw$rothwell_stem<-ifelse(cw$rothwell_classification!="Other","STEM","Non-STEM")
cw$soc_stem2<-ifelse(cw$soc_stem!="Non-STEM","STEM","Non-STEM")
cw_stem<-select(cw,onet_soc_code,onet_soc_title,soc_stem2,nsf_stem,rothwell_stem,soc_stem,nsf_se,rothwell_classification)

# link to data at work data
dw_agg<-as.data.frame(dw)
dw_stem<-dplyr::left_join(dw_agg,cw_stem,by=c("soc_code_common_1"="onet_soc_code"))

# sum by STEM, non-STEM
dw_stem_agg<-group_by(dw_stem,state_code,soc_stem2,nsf_stem,rothwell_stem,soc_stem,nsf_se,rothwell_classification) %>%
  dplyr::summarise(total=sum(total))

# long format
dw_steml<-melt(dw_stem_agg,id=c("state_code","total"))
dw_steml<-group_by(dw_steml,state_code,variable,value) %>%
  dplyr::summarise(total=sum(total))

dw_total<-group_by(dw_steml,state_code,variable) %>%
  dplyr::summarise(total_all=sum(total))

dw_steml<-left_join(dw_steml,dw_total,by=c("state_code","variable"))
dw_steml$percent<-dw_steml$total/dw_steml$total_all

# add state population
dw_steml<-left_join(dw_steml,st_pop,by=c("state_code"="Abbreviation"))
dw_steml$jobsPerPopulation<-dw_steml$total/dw_steml$`HD01_VD01.Estimate; Total`

# link to geo data
#dw_steml_geo<-left_join(state.df,dw_steml,by=c("STUSPS"="state_code"))
dw_steml$state_name<-tolower(dw_steml$VALUE)
dw_steml_geo<-left_join(fifty_states,dw_steml,by=c("id"="state_name"))

dw_steml_geo2<-filter(dw_steml_geo,variable=="nsf_se" | variable=="soc_stem" | variable=="rothwell_classification")
dw_steml_geo2$value2<-ifelse(dw_steml_geo2$value=="Non-S&E and Other","Non-STEM",dw_steml_geo2$value)
dw_steml_geo2$value2<-ifelse(dw_steml_geo2$value=="S&E Occupations","STEM",dw_steml_geo2$value2)
dw_steml_geo2$value2<-ifelse(dw_steml_geo2$value=="S&E-Related Occupations","STEM-related",dw_steml_geo2$value2)
dw_steml_geo2$value2<-ifelse(dw_steml_geo2$value=="Super-STEM","STEM",dw_steml_geo2$value2)
dw_steml_geo2$value2<-ifelse(dw_steml_geo2$value=="High-STEM","STEM-related",dw_steml_geo2$value2)
dw_steml_geo2$value2<-ifelse(dw_steml_geo2$value=="Other","Non-STEM",dw_steml_geo2$value2)

plotData<-filter(dw_steml_geo2,value2!="Non-STEM")
plotData$variable<-as.character(plotData$variable)

pdf("DataAtWork/stem_jobs.pdf",height=5,width=10)
ggplot(data=plotData) +
  geom_polygon(aes(long,lat,group=group,fill=percent),colour="grey70",lwd=.25) +
  scale_fill_viridis(
    option = "viridis",
    direction = -1,
    labels=percent,
    name = "Percentage",
    guide = guide_colorbar(
      ticks = F,
      nbins=100,
      direction = "horizontal",
      barheight = unit(3, units = "mm"),
      barwidth = unit(100, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      label.position = "bottom",na.value="lightgray")) +
  facet_grid(value2 ~ variable, labeller = as_labeller(c(
    "nsf_se" = "NSF Science & Engineering",
    "rothwell_classification" = "Rothwell Super and High STEM",
    "soc_stem" = "BLS STEM",
    "STEM"="STEM \n(S&E or Super STEM)", "STEM-related"="STEM-related \n(S&E-related or High STEM)"))) +
  coord_map() +
  theme_map() +
  labs(x=NULL,y=NULL,
       title="Proportion of posted jobs that are STEM jobs",
       subtitle="Source: Data at Work, University of Chicago, 2016") +
  theme(legend.position = "bottom") #+ fifty_states_inset_boxes()
dev.off()


# ggplot(data=filter(dw_steml_geo2,value2!="Non-STEM")) +
#   geom_polygon(aes(long,lat,group=group,fill=jobsPerPopulation)) +
#   #geom_path(color="grey70",lwd=.25) +
#   scale_fill_viridis(option="magma",direction=-1,name=expression("Job Postings \nper Population")) +
#   facet_grid(value2~variable) +
#   geom_path(aes(long,lat,group=group),data=state.df,size=.25,color="grey50") +
#   coord_map() +
#   theme_bw() +
#   theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
#         axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
#         plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
#         plot.caption = element_text(hjust=0)) + #labels
#   labs(title="STEM jobs per Population", x="", y="", caption="Source: Data at Work, University of Chicago, 2016")


# map  technologist
# filter for occupation
nmt<-filter(dw_stem,soc_code_common_1=="29-2033.00")
swd<-filter(dw_stem,soc_code_common_1=="15-1132.00")

nmt<-nmt[,c(3:14)]
nmt_l<-melt(nmt,id=c("soc_code_common_1","total"))
nmt_agg<-group_by(nmt_l,value) %>% dplyr::summarise(total=sum(total))
nmt_agg<-filter(nmt_agg,value!="")
nmt_agg<-arrange(nmt_agg,desc(total))
total_ads<-sum(nmt_agg$total)
nmt_top<-nmt_agg[1:20,]
nmt_top<-dplyr::rename(nmt_top,skill=value)
nmt_top$skill<-factor(nmt_top$skill,levels=nmt_top$skill)
nmt_top$percent<-nmt_top$total/total_ads
nmt_top$occ<-" Technologists"

swd<-swd[,c(3:14)]
swd_l<-melt(swd,id=c("soc_code_common_1","total"))
swd_agg<-group_by(swd_l,value) %>% dplyr::summarise(total=sum(total))
swd_agg<-filter(swd_agg,value!="")
swd_agg<-arrange(swd_agg,desc(total))
total_ads<-sum(swd_agg$total)
swd_top<-swd_agg[1:20,]
swd_top<-dplyr::rename(swd_top,skill=value)
swd_top$skill2<-ifelse(swd_top$skill=="troubleshooting","              troubleshooting",as.character(swd_top$skill))
swd_top$skill2<-factor(swd_top$skill2,levels=swd_top$skill2)
swd_top$percent<-swd_top$total/total_ads
swd_top$occ<-"Softare Developers, Applications"

nmt_top$num<-nchar(as.character(nmt_top$skill))
swd_top$num<-nchar(as.character(swd_top$skill2))


occ_top<-rbind(nmt_top,swd_top)

# skills bar chart
pdf("DataAtWork/nuclear_medicine_skills.pdf",height=7,width=8)
ggplot(nmt_top,aes(skill,percent)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels = scales::percent, limits = c(0, .12)) +
  theme_bw() +
  labs(x="Skill",y="Percentage Appearing in Job Postings",
       title="Percentage of  Technologist Job Postings with Top 20 Skills",
       subtitle="Source: Data at Work, University of Chicago, 2016") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

pdf("DataAtWork/software_developer_skills.pdf",height=7,width=8)
ggplot(swd_top,aes(skill2,percent)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels = scales::percent, limits = c(0, .120)) +
  theme_bw() +
  labs(x="Skill",y="Percentage Appearing in Job Postings",
       title="Percentage of Software Developers, Applications Job Postings with Top 20 Skills",
       subtitle="Source: Data at Work, University of Chicago, 2016") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

# sum by STEM, non-STEM
dw_stem_occ<-group_by(dw_stem,state_code,soc_code_common_1) %>%
  dplyr::summarise(total=sum(total))

# long format
dw_stemoccl<-melt(dw_stem_occ,id=c("state_code","total"))
dw_stemoccl<-group_by(dw_stemoccl,state_code,variable,value) %>%
  dplyr::summarise(total=sum(total))

dw_occtotal<-group_by(dw_stemoccl,state_code,variable) %>%
  dplyr::summarise(total_all=sum(total))

dw_stemoccl<-left_join(dw_stemoccl,dw_occtotal,by=c("state_code","variable"))
dw_stemoccl$percent<-dw_stemoccl$total/dw_stemoccl$total_all

# link to geo data
dw_stemoccl<-left_join(dw_stemoccl,st_pop,by=c("state_code"="Abbreviation"))
#dw_steml_geo<-left_join(state.df,dw_steml,by=c("STUSPS"="state_code"))
dw_stemoccl$state_name<-tolower(dw_stemoccl$VALUE)
dw_stemoccl_geo<-left_join(fifty_states,dw_stemoccl,by=c("id"="state_name"))

# map of  technologist
pdf("DataAtWork/nuclear_medicine.pdf",height = 4.4, width = 5.5)
ggplot() +
  geom_polygon(data=filter(dw_stemoccl_geo,value=="29-2033.00"),
               aes(long,lat,group=group,fill=percent),colour="grey70",lwd=.25) +
  coord_map() +
  theme_map() +
  scale_fill_viridis(
    option = "viridis",
    direction = -1,
    labels=percent,
    name = "Percentage",
    guide = guide_colorbar(
      ticks = F,
      nbins=100,
      direction = "horizontal",
      barheight = unit(3, units = "mm"),
      barwidth = unit(100, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      label.position = "bottom",na.value="lightgray")) +
  labs(x=NULL,y=NULL,
       title="Proportion of posted jobs for  Technologists",
       subtitle="Source: Data at Work, University of Chicago, 2016") +
  #caption = "Geometry: Virginia counties from ggplot2") +
  theme(legend.position = "bottom") + fifty_states_inset_boxes()
dev.off()

# map of software developer
pdf("DataAtWork/software_developer.pdf",height = 4.4, width = 5.5)
ggplot() +
  geom_polygon(data=filter(dw_stemoccl_geo,value=="15-1132.00"),
               aes(long,lat,group=group,fill=percent),colour="grey70",lwd=.25) +
  coord_map() +
  theme_map() +
  scale_fill_viridis(
    option = "viridis",
    direction = -1,
    labels=percent,
    name = "Percentage",
    guide = guide_colorbar(
      ticks = F,
      nbins=100,
      direction = "horizontal",
      barheight = unit(3, units = "mm"),
      barwidth = unit(100, units = "mm"),
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      label.position = "bottom",na.value="lightgray")) +
      labs(x=NULL,y=NULL,
               title="Proportion of posted jobs for Software Developers, Applications",
               subtitle="Source: Data at Work, University of Chicago, 2016") +
               #caption = "Geometry: Virginia counties from ggplot2") +
  theme(legend.position = "bottom") + fifty_states_inset_boxes()
dev.off()

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text=element_text(family="sans", color="#22211d"),
      axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      panel.grid.major=element_blank(),
      #panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor=element_blank(),
      #panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      plot.background=element_rect(fill="#f5f5f2", color = NA),
      panel.background=element_rect(fill="#f5f5f2", color = NA),
      legend.background=element_rect(fill="#f5f5f2", color = NA),
      panel.border=element_blank(),
      ...
    )
}


