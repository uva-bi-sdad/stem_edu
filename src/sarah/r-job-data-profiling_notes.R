#R JOB DATA PROFILING NOTES
#bgtjobid
##found that almost all are 21 char long, but smaller numbers are 16-20 characters long. However, those shorter IDs otherwise resemble the 21 character IDs, and the sample ID in the data dictionary is only 4 characters long, so going to consider them good.
#jobdate
##jobdate was originally not in date format
#occfam
#occfamname
#employer
#city
#state
##should be two character rather than written out name
#county
#
#fipsstate
## is integer, should be character
#fipscounty
## is integer, should be character
#fips
## is integer, should be character
#lat
#lon
#onet
#onetname
#bgtoccc
#edu
##should be num, IS INTEGER
#degree
#exp
##some of the decimal numbers do not make a ton of sense and may be due to rounding error
#jobhours



#a lot of the char variables have excessive spacing
