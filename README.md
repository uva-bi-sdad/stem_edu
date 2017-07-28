# Stem Education

# Datasets
- coursera_comp_sci.csv 
  - produced from: `~/stem_edu/src/hsbb8/working/webscraping/coursera/cs_coursera_scraping.Rmd`
  - stored in: `~/stem_edu/data/stem_edu/final/webscraping_data/`
  - the final data set with all of the Coursera inforamtion from web scraping Coursera computer science courses
  
- coursera_keyword_counts.csv
  - produced from: `~/stem_edu/src/hsbb8/working/keyword_search/coursera_keyword_search.R`
  - stored in: `~/stem_edu/data/stem_edu/final/webscraping_data/`
  - counts of the number of Coursera.com courses that contain certain keywords of jobs skills (keywords from: O*NET)

- indeed_keyword_counts.csv
  - produced from: `~/stem_edu/src/hsbb8/working/keyword_search/Indeed-JobAdsWebscrapping.ipynb`
  - stored in: `~/stem_edu/data/stem_edu/final/webscraping_data/`
  - counts of the number of Indeed.com job ads that contain certain keywords of job skills (keywords from: O*NET)
  
- keywords_list.csv
  - from O*NET.com: technology skills listed for Software Developer, Applications
  - stored in: `~/stem_edu/data/stem_edu/original/hsbb8/`
  - list of keywords of skills that could be needed in junior software developer jobs
  - used to calculate the number of job ads or courses that these skills appear in 
    - used in: `coursera_keyword_counts.csv and indeed_keyword_counts.csv`
  
- virginia_res.csv
  - produced from: `~/stem_edu/src/chanida3/indeed_virginia_webscraping.R`
  - stored in: `~/stem_edu/data/stem_edu/chanida3/virginia_res.csv`
  - information scraped from dental hygienist resumes in Virginia from indeed.com
  

# Figures/ Plots

- coursera_final_fixed_3.png
  - produced from: `~/stem_edu/src/hsbb8/working/keyword_search/Indeed-JobAdsWebscrapping.ipynb`
  - stored in: `~/stem_edu/output/keyword_search_graphs/`
  - bar plot of the percentages of courses that certain job skills appear in
    - used for the final poster
  
- indeed_final_plot_fixed.png
  - produced from: `~/stem_edu/src/hsbb8/working/keyword_search/Indeed-JobAdsWebscrapping.ipynb`
  - stored in: `~/stem_edu/output/keyword_search_graphs/`
  - bar plot of the percentages of job ads that certain job skills appear in
    - used for the final poster
    
    
STEM Final Data README.md


# Datasets

-IPEDS_2010_degrees.zip -- IPEDS_2015_degrees.zip
    -./data/stem_edu/original/IPEDS_data_zipped
    -IPEDS data on STEM award counts by institution, award type (Master's, associates, certification, etc), race/ethnicity,and gender
    -each count of award type by race is a separate column
    -The ipedsM (melt table) makes the count by race into an observation
    -This data set should be re-downloaded from IPEDS data center
        -Download ALL degrees awarded for race/ethnicity and gender from 2010-2015 and then subset in R
        -I made the mistake of selecting variables to try to subset data from the IPEDS data center and may have made a mistake in PhD awards for 2010

-meltTable.RData (ipedsM is the object name)
    -./data/stem_edu/working/meltTable.RData
    -this is the melted version that turns the counts by race and gender into observations
    -Lines 187-193 in trend_IPEDS.R creates and saves this table
    
#Figures
-./output/mind_maps/
  -Dental_Hygienist.pdf
  -Junior_Software_Developer_Applications.pdf
  -Environmental_Engineer.pdf
    -Mind maps for the three occupations
    -The information contained comes from O*NET profile for each occupation
  
-./output/ipeds_trend/
  -AssociatesDegreeTrend.png
  -BachelorsDegreeTrend.png
  -MastersDegreeTrend.png
  -PHDTrend.png
  -TotalStemTrend.png
    -These are trend plots of STEM degrees earned each year as a percentage of all STEM degrees earned in a 6 year period, by race/ethnicity
    -Code to generate them is in ./src/benjs23/trend_IPEDS.R

-./output/ipeds_trend/
  -2010.png 
    -Shows all of the award types earned in 2010 by race/ethnicity
    -Code to generate them is in ./src/benjs23/bianica_raceAwardCrosstab.R
      -Code can produce plots for all years 2010-2015
