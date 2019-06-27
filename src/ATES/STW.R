# This code separates out the non-degree, HS graduate responses using eduattn (responses 2-6)
load("~/git/stem_edu/data/stem_edu/original/ATES/ates_pu_pert.rdata")
setwd("~/git/stem_edu/src/VAL")
ATES<-ates_pu_pert; remove(ates_pu_pert); table(ATES$eduatt)

#Divide the ATES data into 3 groups, college degree (CD), no college degree but HS diploma (NCD), and no HS diploma (NHS)
#No 4-year college degree or higher 26,156
NCD<-ATES[ATES$eduattn>1 & ATES$eduattn<7,]; dim(NCD); dim(NCD)[1]/dim(ATES)[1]
  #Recode the fields of study that need to be split between two major occupation groups
  #CNFIELD1 4 and 19
  T4<-which(NCD$CNFIELD1==4);T4no<-ceiling(length(T4)/2);
  NCD$CNFIELD1[T4[1:T4no]]<--4
  T19<-which(NCD$CNFIELD1==19);T19no<-ceiling(length(T19)/2);
  NCD$CNFIELD1[T19[1:T19no]]<--19;rm(T4,T4no,T19,T19no)
  #psfos 3
  T3<-which(NCD$psfos==3);T3no<-ceiling(length(T3)/2)
  NCD$psfos[T3[1:T3no]]<--3;rm(T3,T3no)
  #wefolp 7
  T7<-which(NCD$wefolp==7);T7no<-ceiling(length(T7)/2)
  NCD$wefolp[T7[1:T7no]]<--7;rm(T7,T7no)

#4-year college degree or higher 18,521
CD<-ATES[ATES$eduattn>6,]; dim(CD); dim(CD)[1]/dim(ATES)[1]
  #Recode the fields of study that need to be split between two major occupation groups
  #CNFIELD1 4 and 19
  T4<-which(CD$CNFIELD1==4);T4no<-ceiling(length(T4)/2);
  CD$CNFIELD1[T4[1:T4no]]<--4
  T19<-which(CD$CNFIELD1==19);T19no<-ceiling(length(T19)/2);
  CD$CNFIELD1[T19[1:T19no]]<--19;rm(T4,T4no,T19,T19no)
  #psfos 3
  T3<-which(CD$psfos==3);T3no<-ceiling(length(T3)/2)
  CD$psfos[T3[1:T3no]]<--3;rm(T3,T3no)
  #wefolp 7
  T7<-which(CD$wefolp==7);T7no<-ceiling(length(T7)/2)
  CD$wefolp[T7[1:T7no]]<--7;rm(T7,T7no)

#No HS degree 3,067
NHS<-ATES[ATES$eduattn==1,]; dim(NHS); dim(NHS)[1]/dim(ATES)[1]
  #Recode the fields of study that need to be split between two major occupation groups
  #CNFIELD1 4 and 19
  T4<-which(NHS$CNFIELD1==4);T4no<-ceiling(length(T4)/2);
  NHS$CNFIELD1[T4[1:T4no]]<--4
  T19<-which(NHS$CNFIELD1==19);T19no<-ceiling(length(T19)/2);
  NHS$CNFIELD1[T19[1:T19no]]<--19;rm(T4,T4no,T19,T19no)
  #psfos 3
  T3<-which(NHS$psfos==3);T3no<-ceiling(length(T3)/2)
  NHS$psfos[T3[1:T3no]]<--3;rm(T3,T3no)
  #wefolp 7
  T7<-which(NHS$wefolp==7);T7no<-ceiling(length(T7)/2)
  NHS$wefolp[T7[1:T7no]]<--7;rm(T7,T7no)

#Q2. Field of study for highest level of education
table(NHS$edufos) #fields of study for no HS respondents - ALL valid skips
table(NCD$edufos) #fields of study for the noncollege degree respondents
  NCDf<-data.frame(table(NCD$edufos))[-1,];dim(NCDf)
table(CD$edufos) #fields of study for college degree respondents
  CDf<-data.frame(table(CD$edufos));dim(CDf)
#Table with the field of study % for CD and NCD
Q2<-data.frame(cbind(CD=(CDf[,2]/sum(CDf$Freq))*100, NCD=(NCDf[,2]/sum(NCDf$Freq))*100))



##############################################################################################
#Q7. Number of certifications/licenses
#Q9. Occupation for most important certifications/licenses
table(NCD$cnmain) #4781 currently active certifications/licenses
  table(NCD$CNFIELD1) #4609 smaller than above - ?? sum(table(NCD$CNFIELD1)[4:30])
table(CD$cnmain) #6766 currently active certifications/licenses
  table(CD$CNFIELD1) #6647 smaller than above - ?? sum(table(CD$CNFIELD1)[4:30])
#for no HS there is insufficient data to go any further
table(NHS$cnmain) #197 have at least one certification/license
  table(NHS$CNFIELD1) #166 sum(table(NHS$CNFIELD1)[4:21])

#Assign the people with NCD that have a certification/license to a major occupation group
#Assign Q9 to a STW field
NCD.Q9<-ifelse(NCD$CNFIELD1==24, "49-0000",
        ifelse(NCD$CNFIELD1==23, "47-0000",
         ifelse(NCD$CNFIELD1==10, "29-0000",
          ifelse(NCD$CNFIELD1==11, "29-0000",
           ifelse(NCD$CNFIELD1==1, "17-0000",
            ifelse(NCD$CNFIELD1==2, "17-0000",
             ifelse(NCD$CNFIELD1==3, "15-0000",
              ifelse(NCD$CNFIELD1==4, "15-0000",
              ifelse(NCD$CNFIELD1==-4, "19-0000",
               ifelse(NCD$CNFIELD1==19, "45-0000",
               ifelse(NCD$CNFIELD1==-19, "19-0000",
                ifelse(NCD$CNFIELD1==17, "33-0000",
                 ifelse(NCD$CNFIELD1==5, "13-0000",
                  ifelse(NCD$CNFIELD1==6, "13-0000",
                   ifelse(NCD$CNFIELD1==7, "13-0000",
                    ifelse(NCD$CNFIELD1==8, "13-0000",
                     ifelse(NCD$CNFIELD1==16, "23-0000",
                      ifelse(NCD$CNFIELD1==25, "53-0000",
                       ifelse(NCD$CNFIELD1==18, "21-0000",
                        ifelse(NCD$CNFIELD1==20, "21-0000",
                         ifelse(NCD$CNFIELD1==21, "25-0000",
                          ifelse(NCD$CNFIELD1==22, "25-0000",
                           ifelse(NCD$CNFIELD1==9, "31-0000",
                            ifelse(NCD$CNFIELD1==12, "31-0000",
                             ifelse(NCD$CNFIELD1==13, "39-0000",
                              ifelse(NCD$CNFIELD1==14, "39-0000",
                               ifelse(NCD$CNFIELD1==15, "39-0000",
                                ifelse(NCD$CNFIELD1==26, "OTHER",
                                 ifelse(NCD$CNFIELD1==27, "OTHER",
                                  ifelse(NCD$CNFIELD1==-8, "Uncodeable",
                                   ifelse(NCD$CNFIELD1==-9, "Missing","Valid Skip")))))))))))))))))))))))))))))))
NCDq9<-data.frame(table(NCD.Q9)); dim(NCDq9); names(NCDq9)<-c("MOG","NCD")

CD.Q9<-ifelse(CD$CNFIELD1==24, "49-0000",
        ifelse(CD$CNFIELD1==23, "47-0000",
         ifelse(CD$CNFIELD1==10, "29-0000",
          ifelse(CD$CNFIELD1==11, "29-0000",
           ifelse(CD$CNFIELD1==1, "17-0000",
            ifelse(CD$CNFIELD1==2, "17-0000",
             ifelse(CD$CNFIELD1==3, "15-0000",
              ifelse(CD$CNFIELD1==4, "15-0000",
              ifelse(CD$CNFIELD1==-4, "19-0000",
               ifelse(CD$CNFIELD1==19, "45-0000",
               ifelse(CD$CNFIELD1==-19, "19-0000",
                ifelse(CD$CNFIELD1==17, "33-0000",
                 ifelse(CD$CNFIELD1==5, "13-0000",
                  ifelse(CD$CNFIELD1==6, "13-0000",
                   ifelse(CD$CNFIELD1==7, "13-0000",
                    ifelse(CD$CNFIELD1==8, "13-0000",
                     ifelse(CD$CNFIELD1==16, "23-0000",
                      ifelse(CD$CNFIELD1==25, "53-0000",
                       ifelse(CD$CNFIELD1==18, "21-0000",
                        ifelse(CD$CNFIELD1==20, "21-0000",
                         ifelse(CD$CNFIELD1==21, "25-0000",
                          ifelse(CD$CNFIELD1==22, "25-0000",
                           ifelse(CD$CNFIELD1==9, "31-0000",
                            ifelse(CD$CNFIELD1==12, "31-0000",
                             ifelse(CD$CNFIELD1==13, "39-0000",
                              ifelse(CD$CNFIELD1==14, "39-0000",
                               ifelse(CD$CNFIELD1==15, "39-0000",
                                 ifelse(CD$CNFIELD1==26, "OTHER",
                                  ifelse(CD$CNFIELD1==27, "OTHER",
                                   ifelse(CD$CNFIELD1==-8, "Uncodeable",
                                    ifelse(CD$CNFIELD1==-9, "Missing","Valid Skip")))))))))))))))))))))))))))))))
CDq9<-data.frame(table(CD.Q9)); dim(CDq9); names(CDq9)<-c("MOG","CD")

NHS.Q9<-ifelse(NHS$CNFIELD1==24, "49-0000",
              ifelse(NHS$CNFIELD1==23, "47-0000",
               ifelse(NHS$CNFIELD1==10, "29-0000",
                ifelse(NHS$CNFIELD1==11, "29-0000",
                 ifelse(NHS$CNFIELD1==1, "17-0000",
                  ifelse(NHS$CNFIELD1==2, "17-0000",
                   ifelse(NHS$CNFIELD1==3, "15-0000",
                    ifelse(NHS$CNFIELD1==4, "15-0000",
                    ifelse(NHS$CNFIELD1==-4, "19-0000",
                     ifelse(NHS$CNFIELD1==19, "45-0000",
                     ifelse(NHS$CNFIELD1==-19, "19-0000",
                      ifelse(NHS$CNFIELD1==17, "33-0000",
                       ifelse(NHS$CNFIELD1==5, "13-0000",
                        ifelse(NHS$CNFIELD1==6, "13-0000",
                         ifelse(NHS$CNFIELD1==7, "13-0000",
                          ifelse(NHS$CNFIELD1==8, "13-0000",
                           ifelse(NHS$CNFIELD1==16, "23-0000",
                            ifelse(NHS$CNFIELD1==25, "53-0000",
                             ifelse(NHS$CNFIELD1==18, "21-0000",
                              ifelse(NHS$CNFIELD1==20, "21-0000",
                               ifelse(NHS$CNFIELD1==21, "25-0000",
                                ifelse(NHS$CNFIELD1==22, "25-0000",
                                ifelse(NHS$CNFIELD1==9, "31-0000",
                                 ifelse(NHS$CNFIELD1==12, "31-0000",
                                  ifelse(NHS$CNFIELD1==13, "39-0000",
                                   ifelse(NHS$CNFIELD1==14, "39-0000",
                                    ifelse(NHS$CNFIELD1==15, "39-0000",
                                     ifelse(NHS$CNFIELD1==26, "OTHER",
                                      ifelse(NHS$CNFIELD1==27, "OTHER",
                                       ifelse(NHS$CNFIELD1==-8, "Uncodeable",
                                        ifelse(NHS$CNFIELD1==-9, "Missing","Valid Skip")))))))))))))))))))))))))))))))
NHSq9<-data.frame(table(NHS.Q9)); dim(NHSq9); names(NHSq9)<-c("MOG","NHS")

M1<-merge(x=NCDq9, y=NHSq9, by="MOG", all=TRUE);Q9<-merge(x=CDq9, y=M1, by="MOG", all=TRUE);Q9<-Q9[-19,];rm(M1)

PROBq9<-c(0.015, 0.211, 0.256, 0.056, 0, 0.014, 0, 0.425, 0, 0.094, 0, 0.111, 0.561, 0.892, 0.009, 0, 0, 0)
Q9<-data.frame(Q9, CDstem=round(Q9[,2]*PROBq9,0), NCDStem=round(Q9[,3]*PROBq9,0), NHSstem=round(Q9[,4]*PROBq9,0))
Q9<-data.frame(Q9, CDp=round(((Q9[,2]-Q9[,5])/6766)*100,2), NCDp=round(((Q9[,3]-Q9[,6])/4781)*100,2), NHSp=round(((Q9[,4]-Q9[,7])/197)*100,2),
               CDpstem=round((Q9[,5]/6766)*100,2), NCDpstem=round((Q9[,6]/4781)*100,2), NHSpstem=round((Q9[,7]/197)*100,2),
               PROBstem=PROBq9)

sum(Q9[,5])/6766 #9.82% STEM certifications/licenses college degree
sum(Q9[,6])/4781 #12.86% STEM certifications/licenses no college degree
sum(Q9[,7],na.rm=TRUE)/197 #12.18% STEM certifications/licenses no high school


##############################################################################################
#Q30d. Number of certificates from a community or technical college or other school after HS
#Q31. Occupation for most post-secondary certificate
table(NCD$certprog) #4730 have at least one certificate
  table(NCD$psfos) #sum(table(NCD$psfos)[2:22])=4730
table(CD$certprog) #1815 have at least one certificate
  table(CD$psfos) #sum(table(CD$psfos)[2:22])=1815
#for no HS there is insufficient data to go any further
table(NHS$certprog) #131 have at least one certificate
  table(NHS$psfos) #sum(table(CD$psfos)[2:18])=131

#Assign Q31 to a STW field
NCD.Q31<-ifelse(NCD$psfos==19, "49-0000",
          ifelse(NCD$psfos==7, "47-0000",
           ifelse(NCD$psfos==14, "29-0000",
            ifelse(NCD$psfos==11, "17-0000",
             ifelse(NCD$psfos==18, "51-0000",
              ifelse(NCD$psfos==6, "15-0000",
               ifelse(NCD$psfos==4, "27-0000",
                ifelse(NCD$psfos==3, "45-0000",
                ifelse(NCD$psfos==-3, "19-0000",
                 ifelse(NCD$psfos==15, "33-0000",
                  ifelse(NCD$psfos==5, "11-0000",
                   ifelse(NCD$psfos==1, "13-0000",
                    ifelse(NCD$psfos==16, "23-0000",
                     ifelse(NCD$psfos==9, "35-0000",
                      ifelse(NCD$psfos==20, "53-0000",
                       ifelse(NCD$psfos==2, "43-0000",
                        ifelse(NCD$psfos==10, "25-0000",
                         ifelse(NCD$psfos==8, "39-0000",
                          ifelse(NCD$psfos==13, "39-0000",
                           ifelse(NCD$psfos==12, "nonSTEM",
                            ifelse(NCD$psfos==17, "nonSTEM",
                             ifelse(NCD$psfos==21, "OTHER","Valid Skip"))))))))))))))))))))))
NCDq31<-data.frame(table(NCD.Q31)); dim(NCDq31); names(NCDq31)<-c("MOG","NCD")

CD.Q31<-ifelse(CD$psfos==19, "49-0000",
         ifelse(CD$psfos==7, "47-0000",
          ifelse(CD$psfos==14, "29-0000",
           ifelse(CD$psfos==11, "17-0000",
            ifelse(CD$psfos==18, "51-0000",
             ifelse(CD$psfos==6, "15-0000",
              ifelse(CD$psfos==4, "27-0000",
               ifelse(CD$psfos==3, "45-0000",
               ifelse(CD$psfos==-3, "19-0000",
                ifelse(CD$psfos==15, "33-0000",
                 ifelse(CD$psfos==5, "11-0000",
                  ifelse(CD$psfos==1, "13-0000",
                   ifelse(CD$psfos==16, "23-0000",
                    ifelse(CD$psfos==9, "35-0000",
                     ifelse(CD$psfos==20, "53-0000",
                      ifelse(CD$psfos==2, "43-0000",
                       ifelse(CD$psfos==10, "25-0000",
                        ifelse(CD$psfos==8, "39-0000",
                         ifelse(CD$psfos==13, "39-0000",
                          ifelse(CD$psfos==12, "nonSTEM",
                           ifelse(CD$psfos==17, "nonSTEM",
                            ifelse(CD$psfos==21, "OTHER","Valid Skip"))))))))))))))))))))))
CDq31<-data.frame(table(CD.Q31)); dim(CDq31); names(CDq31)<-c("MOG","CD")

NHS.Q31<-ifelse(NHS$psfos==19, "49-0000",
                ifelse(NHS$psfos==7, "47-0000",
                 ifelse(NHS$psfos==14, "29-0000",
                  ifelse(NHS$psfos==11, "17-0000",
                   ifelse(NHS$psfos==18, "51-0000",
                    ifelse(NHS$psfos==6, "15-0000",
                     ifelse(NHS$psfos==4, "27-0000",
                      ifelse(NHS$psfos==3, "45-0000",
                      ifelse(NHS$psfos==-3, "19-0000",
                       ifelse(NHS$psfos==15, "33-0000",
                        ifelse(NHS$psfos==5, "11-0000",
                         ifelse(NHS$psfos==1, "13-0000",
                          ifelse(NHS$psfos==16, "23-0000",
                           ifelse(NHS$psfos==9, "35-0000",
                            ifelse(NHS$psfos==20, "53-0000",
                             ifelse(NHS$psfos==2, "43-0000",
                              ifelse(NHS$psfos==10, "25-0000",
                               ifelse(NHS$psfos==8, "39-0000",
                                ifelse(NHS$psfos==13, "39-0000",
                                 ifelse(NHS$psfos==12, "nonSTEM",
                                  ifelse(NHS$psfos==17, "nonSTEM",
                                   ifelse(NHS$psfos==21, "OTHER","Valid Skip"))))))))))))))))))))))
NHSq31<-data.frame(table(NHS.Q31)); dim(NHSq31); names(NHSq31)<-c("MOG","NHS")

M1<-merge(x=NCDq31, y=NHSq31, by="MOG", all=TRUE);Q31<-merge(x=CDq31, y=M1, by="MOG", all=TRUE);Q31<-Q31[-21,];rm(M1)

PROBq31<-c(0.026, 0.015, 0.211, 0.256, 0.056, 0.014, 0, 0.117, 0.425, 0.094, 0.010, 0, 0.002, 0.111, 0.561, 0.892, 0.255, 0.009, 0, 0)
Q31<-data.frame(Q31, CDstem=round(Q31[,2]*PROBq31,0), NCDStem=round(Q31[,3]*PROBq31,0), NHSstem=round(Q31[,4]*PROBq31,0))
Q31<-data.frame(Q31, CDp=round(((Q31[,2]-Q31[,5])/1815)*100,2), NCDp=round(((Q31[,3]-Q31[,6])/4730)*100,2), NHSp=round(((Q31[,4]-Q31[,7])/131)*100,2),
               CDpstem=round((Q31[,5]/1815)*100,2), NCDpstem=round((Q31[,6]/4730)*100,2), NHSpstem=round((Q31[,7]/131)*100,2),
               PROBstem=PROBq31)

sum(Q31[,5])/1815 #15.70% STEM certifications/licenses college degree
sum(Q31[,6])/4730 #25.77% STEM certifications/licenses no college degree
sum(Q31[,7],na.rm=TRUE)/131 #29.01% STEM certifications/licenses no high school



##############################################################################################
#Q39. Completed an internship, co-op, practicum, clerkship, externship, residency, clinical experience, apprenticeship, or similar experience.
#Q40. Occupation for most work experience programs
table(NCD$weprog) #3158 have completed a work experience program
  table(NCD$wefolp) #sum(table(NCD$wefolp)[2:27]) = 3158
table(CD$weprog) #7681 have completed a work experience program
  table(CD$wefolp) #sum(table(CD$wefolp)[2:27]) = 7681
#for no HS there is insufficient data to go any further
table(NHS$weprog) #92 have completed a work experience program
  table(NHS$wefolp) #sum(table(NHS$wefolp)[2:19]) = 92

#Assign Q40 to a STW field
NCD.Q40<-ifelse(NCD$wefolp==24, "49-0000",
          ifelse(NCD$wefolp==20, "49-0000",
           ifelse(NCD$wefolp==1, "47-0000",
            ifelse(NCD$wefolp==2, "47-0000",
             ifelse(NCD$wefolp==3, "47-0000",
              ifelse(NCD$wefolp==4, "47-0000",
               ifelse(NCD$wefolp==5, "47-0000",
                ifelse(NCD$wefolp==6, "29-0000",
                 ifelse(NCD$wefolp==7, "29-0000",
                 ifelse(NCD$wefolp==-7, "31-0000",
                  ifelse(NCD$wefolp==14, "17-0000",
                   ifelse(NCD$wefolp==18, "51-0000",
                    ifelse(NCD$wefolp==21, "51-0000",
                     ifelse(NCD$wefolp==11, "15-0000",
                      ifelse(NCD$wefolp==26, "27-0000",
                       ifelse(NCD$wefolp==16, "33-0000",
                        ifelse(NCD$wefolp==19, "11-0000",
                         ifelse(NCD$wefolp==9, "13-0000",
                          ifelse(NCD$wefolp==17, "23-0000",
                           ifelse(NCD$wefolp==10, "35-0000",
                            ifelse(NCD$wefolp==13, "53-0000",
                              ifelse(NCD$wefolp==22, "21-0000",
                               ifelse(NCD$wefolp==23, "25-0000",
                                ifelse(NCD$wefolp==8, "31-0000",
                                 ifelse(NCD$wefolp==12, "39-0000",
                                  ifelse(NCD$wefolp==15, "39-0000",
                                   ifelse(NCD$wefolp==25, "OTHER","Valid Skip")))))))))))))))))))))))))))
NCDq40<-data.frame(table(NCD.Q40)); dim(NCDq40); names(NCDq40)<-c("MOG","NCD")

CD.Q40<-ifelse(CD$wefolp==24, "49-0000",
         ifelse(CD$wefolp==20, "49-0000",
          ifelse(CD$wefolp==1, "47-0000",
           ifelse(CD$wefolp==2, "47-0000",
            ifelse(CD$wefolp==3, "47-0000",
             ifelse(CD$wefolp==4, "47-0000",
              ifelse(CD$wefolp==5, "47-0000",
               ifelse(CD$wefolp==6, "29-0000",
                ifelse(CD$wefolp==7, "29-0000",
                ifelse(CD$wefolp==-7, "31-0000",
                 ifelse(CD$wefolp==14, "17-0000",
                  ifelse(CD$wefolp==18, "51-0000",
                   ifelse(CD$wefolp==21, "51-0000",
                    ifelse(CD$wefolp==11, "15-0000",
                     ifelse(CD$wefolp==26, "27-0000",
                      ifelse(CD$wefolp==16, "33-0000",
                       ifelse(CD$wefolp==19, "11-0000",
                        ifelse(CD$wefolp==9, "13-0000",
                         ifelse(CD$wefolp==17, "23-0000",
                          ifelse(CD$wefolp==10, "35-0000",
                           ifelse(CD$wefolp==13, "53-0000",
                             ifelse(CD$wefolp==22, "21-0000",
                              ifelse(CD$wefolp==23, "25-0000",
                               ifelse(CD$wefolp==8, "31-0000",
                                ifelse(CD$wefolp==12, "39-0000",
                                 ifelse(CD$wefolp==15, "39-0000",
                                  ifelse(CD$wefolp==25, "OTHER","Valid Skip")))))))))))))))))))))))))))
CDq40<-data.frame(table(CD.Q40)); dim(CDq40); names(CDq40)<-c("MOG","CD")

NHS.Q40<-ifelse(NHS$wefolp==24, "49-0000",
              ifelse(NHS$wefolp==20, "49-0000",
               ifelse(NHS$wefolp==1, "47-0000",
                ifelse(NHS$wefolp==2, "47-0000",
                 ifelse(NHS$wefolp==3, "47-0000",
                  ifelse(NHS$wefolp==4, "47-0000",
                   ifelse(NHS$wefolp==5, "47-0000",
                    ifelse(NHS$wefolp==6, "29-0000",
                     ifelse(NHS$wefolp==7, "29-0000",
                     ifelse(NHS$wefolp==-7, "31-0000",
                      ifelse(NHS$wefolp==14, "17-0000",
                       ifelse(NHS$wefolp==18, "51-0000",
                        ifelse(NHS$wefolp==21, "51-0000",
                         ifelse(NHS$wefolp==11, "15-0000",
                          ifelse(NHS$wefolp==26, "27-0000",
                           ifelse(NHS$wefolp==16, "33-0000",
                            ifelse(NHS$wefolp==19, "11-0000",
                             ifelse(NHS$wefolp==9, "13-0000",
                              ifelse(NHS$wefolp==17, "23-0000",
                               ifelse(NHS$wefolp==10, "35-0000",
                                ifelse(NHS$wefolp==13, "53-0000",
                                 ifelse(NHS$wefolp==22, "21-0000",
                                  ifelse(NHS$wefolp==23, "25-0000",
                                   ifelse(NHS$wefolp==8, "31-0000",
                                    ifelse(NHS$wefolp==12, "39-0000",
                                     ifelse(NHS$wefolp==15, "39-0000",
                                      ifelse(NHS$wefolp==25, "OTHER","Valid Skip")))))))))))))))))))))))))))
NHSq40<-data.frame(table(NHS.Q40)); dim(NHSq40); names(NHSq40)<-c("MOG","NHS")

M1<-merge(x=NCDq40, y=NHSq40, by="MOG", all=TRUE);Q40<-merge(x=CDq40, y=M1, by="MOG", all=TRUE);Q40<-Q40[-19,];rm(M1)

PROBq40<-c(0.026, 0.015, 0.211, 0.256, 0, 0.014, 0, 0.117, 0.425, 0, 0.094, 0.010, 0, 0.561, 0.892, 0.255, 0.009, 0)
Q40<-data.frame(Q40, CDstem=round(Q40[,2]*PROBq40,0), NCDStem=round(Q40[,3]*PROBq40,0), NHSstem=round(Q40[,4]*PROBq40,0))
Q40<-data.frame(Q40, CDp=round(((Q40[,2]-Q40[,5])/7681)*100,2), NCDp=round(((Q40[,3]-Q40[,6])/3158)*100,2), NHSp=round(((Q40[,4]-Q40[,7])/92)*100,2),
                CDpstem=round((Q40[,5]/7681)*100,2), NCDpstem=round((Q40[,6]/3158)*100,2), NHSpstem=round((Q40[,7]/92)*100,2),
                PROBstem=PROBq40)

sum(Q40[,5])/7681 #8.01% STEM work experience programs college degree
sum(Q40[,6])/3158 #18.65% STEM work experience programs no college degree
sum(Q40[,7],na.rm=TRUE)/92 #33.70% STEM work experience programs no high school




