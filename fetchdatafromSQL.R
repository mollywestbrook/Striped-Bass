## Script to grab data from SQL if necessary

library(here)
library(RODBC)
library(tidyverse)
library(dbplyr)

#open connection to SQL server:

tideROconnection <- odbcDriverConnect('driver={SQL Server};
                                    server=10.141.0.18;
                                    database=tide;
                                    trusted_connection=no;
                                    uid=tideRO;
                                    PWD=S8iche~9')

#simply swap out which Date you're looking for
#I'm not going to bother making this fancier because if I'm using this 
#I'm already going beyond what I usually do  

#okay so. For the late month cruises this won't work because of the insane frankenstein
#combination of dates for these...revisit later
thismonthscruise <- sqlQuery(tideROconnection, 
                        "SELECT * FROM [tide].[dbo].[ccnd_wqProfile_9Sep25]
                        WHERE [Date] BETWEEN '2025-08-15'AND '2025-08-31';")
thismonthscruise <- thismonthscruise %>%
  arrange(STATION)

#just compare to the file Andrew sent to Becca to make sure my dates are correct:

cruisefile <- read_csv("BAY895forBecca.csv")

cruisefile <- cruisefile %>%
  arrange(Date)

#check to make sure we're good:
setdiff(thismonthscruise$STATION, cruisefile$Station)

