
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(stringr)
library(lubridate)
library(scales)
library(RColorBrewer)
library(rvest)
library(ggpmisc)
library(kableExtra)
#install.packages("magick")
#install.packages("webshot")
#webshot::install_phantomjs()


#Steps
#1.) download data from FDEP csv
#2.) Tidy data. puts data in standard format
#4.) Run functions. THese help break analysis into managable steps
#5.) Run Blank ANalysis. Creates figs and tables
#6.) Run markdown file. Takes figs and table and knits into report.


# Import data from eXcel ------------------#Data Imported from FDEP WINS 11/12/21--------------------------------


FDEP_WIN_FIELD_BLANK_NOX <- read_excel("Data/FDEP_WIN_FIELD_BLANK_NOX.xlsx")



# Tidy Data ---------------------------------------------------------------

FDEP_data_tidy <- FDEP_WIN_FIELD_BLANK_NOX %>%
mutate(across(where(is.logical), as.numeric))%>%
mutate(across(where(is.double), as.character)) %>%
mutate(`DEP Result Value Number`=as.numeric(`DEP Result Value Number`),`DEP MDL`=as.numeric(`DEP MDL`)) %>%  
mutate(YEAR=year(`Activity Start Date Time`),MONTH=month(`Activity Start Date Time`))  


distinct(FDEP_data_tidy ,`Activity Type`)


# Summarise data ----------------------------------------------------------

#Sumamrise by sampling agency and year
FDEP_Blanks_hit_freq_wide <- FDEP_data_tidy  %>%
group_by(YEAR,`Sampling Agency Name`) %>%
summarise(n=n(),
            `Min MDL`=min(`DEP MDL`,na.rm = TRUE),
            `Min PQL`=min(`DEP PQL`,na.rm = TRUE),
            `Max MDL`=max(`DEP MDL`,na.rm = TRUE),
            `Max PQL`=max(`DEP PQL`,na.rm = TRUE),
            `Total Blanks`=sum( !is.na(`DEP Result Value Number`)),
            `Total Blanks > MDL`=sum(`DEP Result Value Number` >= `DEP MDL` & `Value Qualifier`!="U",na.rm=TRUE),
            `Total Blank Percent > MDL`=percent(if_else(`Total Blanks > MDL` >0,`Total Blanks > MDL`/`Total Blanks`,0)),
            `Field Blanks`=sum(!is.na(`DEP Result Value Number`) & `Activity Type`== "Field Blank"),
            `Field Blanks > MDL`=sum(`DEP Result Value Number` >= `DEP MDL` & `Value Qualifier`!="U" & `Activity Type`== "Field Blank",na.rm=TRUE),
            `Field Blank Percent > MDL`=percent(if_else(`Field Blanks > MDL` >0,`Field Blanks > MDL`/`Field Blanks`,0)),
            `Equipment Blanks`=sum(!is.na(`DEP Result Value Number`) & `Activity Type`== "Equipment Blank"),
            `Equipment Blanks > MDL`=sum(`DEP Result Value Number` >= `DEP MDL` & `Value Qualifier`!="U" & `Activity Type`== "Equipment Blank",na.rm=TRUE),
            `Equipment Blank Percent > MDL`=percent(if_else(`Equipment Blanks > MDL` >0,`Equipment Blanks > MDL`/`Equipment Blanks`,0)))

#Sumamrise by year
FDEP_Blanks_hit_by_year <- FDEP_data_tidy  %>%
group_by(YEAR) %>%
  summarise(n=n(),
            `Min MDL`=min(`DEP MDL`,na.rm = TRUE),
            `Min PQL`=min(`DEP PQL`,na.rm = TRUE),
            `Max MDL`=max(`DEP MDL`,na.rm = TRUE),
            `Max PQL`=max(`DEP PQL`,na.rm = TRUE),
            `String detect not U`=sum(!str_detect(`Value Qualifier`,"U"),na.rm = TRUE),
            `Equals not U`=sum(!`Value Qualifier`=="U",na.rm = TRUE),
            `Total Blanks`=sum( !is.na(`DEP Result Value Number`)),
            `Total Blanks > MDL`=sum(`DEP Result Value Number` >= `DEP MDL` & !str_detect(`Value Qualifier`,"U"),na.rm=TRUE),
            `Total Blank Percent > MDL`=percent(if_else(`Total Blanks > MDL` >0,`Total Blanks > MDL`/`Total Blanks`,0),accuracy=.1),
            `Field Blanks`=sum(!is.na(`DEP Result Value Number`) & `Activity Type`== "Field Blank"),
            `Field Blanks > MDL`=sum(`DEP Result Value Number` >= `DEP MDL` & !str_detect(`Value Qualifier`,"U") & `Activity Type`== "Field Blank",na.rm=TRUE),
            `Field Blank Percent > MDL`=percent(if_else(`Field Blanks > MDL` >0,`Field Blanks > MDL`/`Field Blanks`,0),accuracy=.1),
            `Equipment Blanks`=sum(!is.na(`DEP Result Value Number`) & `Activity Type`== "Equipment Blank"),
            `Equipment Blanks > MDL`=sum(`DEP Result Value Number` >= `DEP MDL` & !str_detect(`Value Qualifier`,"U") & `Activity Type`== "Equipment Blank",na.rm=TRUE),
            `Equipment Blank Percent > MDL`=percent(if_else(`Equipment Blanks > MDL` >0,`Equipment Blanks > MDL`/`Equipment Blanks`,0),accuracy=.1))

#Sumamrise ungrouped
FDEP_Blanks_hits <- FDEP_data_tidy  %>%
ungroup() %>%
summarise(n=n(),
            `Min MDL`=min(`DEP MDL`,na.rm = TRUE),
            `Min PQL`=min(`DEP PQL`,na.rm = TRUE),
            `Max MDL`=max(`DEP MDL`,na.rm = TRUE),
            `Max PQL`=max(`DEP PQL`,na.rm = TRUE),
            `String detect not U`=sum(!str_detect(`Value Qualifier`,"U"),na.rm = TRUE),
            `Equals not U`=sum(!`Value Qualifier`=="U",na.rm = TRUE),
            `Total Blanks`=sum( !is.na(`DEP Result Value Number`)),
            `Total Blanks > MDL`=sum(`DEP Result Value Number` >= `DEP MDL` & !`Value Qualifier`=="U",na.rm=TRUE),
            `Total Blank Percent > MDL`=percent(if_else(`Total Blanks > MDL` >0,`Total Blanks > MDL`/`Total Blanks`,0),accuracy=.1),
            `Field Blanks`=sum(!is.na(`DEP Result Value Number`) & `Activity Type`== "Field Blank"),
            `Field Blanks > MDL`=sum(`DEP Result Value Number` >= `DEP MDL` & !`Value Qualifier`=="U" & `Activity Type`== "Field Blank",na.rm=TRUE),
            `Field Blank Percent > MDL`=percent(if_else(`Field Blanks > MDL` >0,`Field Blanks > MDL`/`Field Blanks`,0),accuracy=.1),
            `Equipment Blanks`=sum(!is.na(`DEP Result Value Number`) & `Activity Type`== "Equipment Blank"),
            `Equipment Blanks > MDL`=sum(`DEP Result Value Number` >= `DEP MDL` & !`Value Qualifier`=="U" & `Activity Type`== "Equipment Blank",na.rm=TRUE),
            `Equipment Blank Percent > MDL`=percent(if_else(`Equipment Blanks > MDL` >0,`Equipment Blanks > MDL`/`Equipment Blanks`,0),accuracy=.1))

