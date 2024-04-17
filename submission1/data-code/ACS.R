
# Meta --------------------------------------------------------------------

## Title:  American Community Survey Data
## Author: Ian McCarthy
## Date Created: 12/6/2019
## Date Edited:  9/6/2022


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, readr, readxl, scales, acs, tidyr)

#api.key.install(key='f5b7f0bc33365698944c752ec241f0132486a3b2')
#acs.lookup(endyear = 2012,table.number = "B27010", span = 1)

file1 <- readLines("data/input/acs_insurance.txt")
file2 <- readLines("data/input/acs_medicaid.txt")
file3 <- readLines("data/input/medicaid_expansion.txt")
combined_data <- c(file1, file2, file3)
writeLines(combined_data, "combined.txt")
# Retrieve ACS data -------------------------------------------------------
states <- geo.make(state="*")
for (t in 2012:2019) {
  # Read the combined text file
combined_data <- read.table("data/input/combined.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
ins.dat <- combined_data[, cbind("B27010_018", "B27010_020", "B27010_021", "B27010_022", "B27010_023", "B27010_024", "B27010_025", "B27010_033", "B27010_034", "B27010_036", "B27010_037", "B27010_038", "B27010_039", "B27010_040", "B27010_041", "B27010_050")]

  #ins.all <- acs.fetch(geography=states, table.number="B27010", span=1, endyear=t)
  #ins.dat <- ins.all@estimate[,cbind("B27010_018",
   #                              "B27010_020",
   #                              "B27010_021",
    #                             "B27010_022",
     #                            "B27010_023",
      #                           "B27010_024",
       #                          "B27010_025",
        #                         "B27010_033",
        #                         "B27010_034",
         #                        "B27010_036",
         #                        "B27010_037",
         #                        "B27010_038",
          #                       "B27010_039",
         #                        "B27010_040",
         #                        "B27010_041",
         #                        "B27010_050")]
  ins.dat <- as_tibble(ins.dat, 
                       rownames="State")
  ins.dat <- ins.dat %>%
    rename(all_18to34="B27010_018",
           employer_18to34="B27010_020",
           direct_18to34="B27010_021",
           medicare_18to34="B27010_022",
           medicaid_18to34="B27010_023",
           tricare_18to34="B27010_024",
           va_18to34="B27010_025",
           none_18to34="B27010_033",
           all_35to64="B27010_034",
           employer_35to64="B27010_036",
           direct_35to64="B27010_037",
           medicare_35to64="B27010_038",
           medicaid_35to64="B27010_039",
           tricare_35to64="B27010_040",
           va_35to64="B27010_041",
           none_35to64="B27010_050") %>%
    mutate(year=t)
  
  assign(paste0('insurance.',t),ins.dat)
}

# Tidy --------------------------------------------------------------------

final.insurance <- rbind(insurance.2012, insurance.2013, insurance.2014, insurance.2015, insurance.2016,
                         insurance.2017, insurance.2018, insurance.2019)
final.insurance <- final.insurance %>%
  mutate(adult_pop = all_18to34 + all_35to64,
         ins_employer = employer_18to34 + employer_35to64,
         ins_direct = direct_18to34 + direct_35to64,
         ins_medicare = medicare_18to34 + medicare_35to64,
         ins_medicaid = medicaid_18to34 + medicaid_35to64,
         uninsured = none_18to34 + none_35to64) %>%
  select(State, year, adult_pop, ins_employer, ins_direct, 
         ins_medicare, ins_medicaid, uninsured)


write_tsv(final.insurance,'data/output/acs_insurance.txt')
