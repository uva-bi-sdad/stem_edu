### use nohup to write data
########
### convert job date to an actual field
########
### INDEX BGTJOBID, JOB DATE - first run
### CLEANTITLE, INDUSTRY? index as you need, separate lines for each one

library(magrittr)

econ_reg_orgs <- readxl::read_excel("data/stem_edu/original/BurningGlassData/Economic Regions.xlsx", sheet = "2019RegionsOrgs")
econ_reg_locs <- readxl::read_excel("data/stem_edu/original/BurningGlassData/Economic Regions.xlsx", sheet = "2019RegionsLoc")

va_counties <- tigris::counties(state = "VA")

sp::spplot(va_counties)

va_counties$FIPS <- paste0(va_counties$STATEFP, va_counties$COUNTYFP)
econ_va_counties <- sp::merge(va_counties, econ_reg_locs, 'FIPS')
econ_va_counties <- sp::merge(econ_va_counties, econ_reg_orgs, 'Region')

sp::spplot(econ_va_counties)

va_ecreg_plot <- sp::spplot(econ_va_counties, zcol = "Region")
va_ecreg_plot
