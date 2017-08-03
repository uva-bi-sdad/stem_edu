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
    
- ./data/stem_edu/original/IPUMS
  - Attachment_C_STEM.csv
      - List of SOC codes considered STEM occupations by BLS.
  - ipums.csv
      - 2015 ACS/Census data table downloaded from IPUMS *AFTER* running 'do' file on original download in STATA.
      - ipums does not preserve the sample you select for more than a couple of weeks. This was selected using Maddie Arnsbarger's account.
      - Must make personal account with IPUMS and select the variables/year/sources you want to use.
  - ipums_data_dictionary.csv
      - variable definitions and valid values for each variable in ipums.csv sample.
      - original data dictionary is in the dspg 2017 nsf stem project folder
  - origin
      - these 3 csv files compose ipums_data_dictionary.csv
      - source: `./src/maddieka/01.1-ipums_data_dict.R`
      
- ./data/stem_edu/original/NCES_ELS
  - byf3_dictionary.csv
      - csv version of Layout_BYF1SCH.txt
      - variable definitions for the els_02_12_byf3stu_v1_0.rdata file.
  - Codebook.txt
      - Comprehensive data dictionary for all 4 tables downloaded from NCES.
  - Layout_....txt
      - Data dictionaries for each corresponding .rdata file.
      - Includes variable definitions, valid values, and valid value definitions.
  - els_02...._v1_0.rdata
      - rdata files downloaded from NCES ELS website.
      - stu = student, inst = institution, sch = school.

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
    
- ./output/sankey_diagrams/ipums_women_deglevel_3.selected.occupations.html
  - description: sankey of ipums data (only women; degree level to stem categorization for occupation + 3 selected case study occupations.)
  - source: `src/maddieka/01.2-sankey_ipums.R
  
- ./output/sankey_diagrams/ipums_women_degreelevel_high.super.stem.occs.html
  - description: sankey of ipums data (only women; degree level to high/super stem occupation categories.)
  - source: `src/maddieka/poster_plotsDSPG2017.R`

- ./output/sankey_diagrams/nces_sankey.html
  - description: sankey of nces data
  - source: `src/maddieka/02.1-sankey_nces.R`

- ./output/sankey_diagrams/ipums_women_degreelevel_all.stem.occs.html
  - description: DSPG 2017 POSTER SANKEY -- sankey of ipums data 
        (only women; degree level to stem occupation categories (using Biania's stem score calculations))
  - source: `src/maddieka/poster_plotsDSPG2017.R`

- ./output/sankey_diagrams/ipums_women_degtype_only3selected.occs.html
  - description: sankey of ipums data (only women; degree level to 3 selected case study occupations.)
  - source: `src/maddieka/01.2-sankey_ipums.R`
    
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
