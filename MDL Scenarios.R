
.libPaths("H:/Docs/R/win-library/3.4") #use when using work computer
remove(list=ls()) #removes all objects from project


library(RODBC)      #needed for download of from SQL
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
library(ggridges)

#Steps
#1.) download data from DBHYDRO and save to csv. Step takes long time. skip to step 2 unless data needs refresh 
#2.) upload data from csv. Fast
#3.) Tidy data. puts data in standard format
#4.) Run functions. THese help break analysis into managable steps
#5.) Run Blank ANalysis. Creates figs and tables



# Import Project Data from DBHYDRO -----------------------------------------------------
DBHYDRO  <- odbcConnect("wrep", uid="pub", pwd="pub", believeNRows=FALSE) # Connect to DBHYDRO
odbcGetInfo(DBHYDRO) # Just for checking the connection

Recent_Data_Connection_String <- ("SELECT SAMPLE_WITH_QC_VIEW.PROJECT_CODE, SAMPLE_WITH_QC_VIEW.SAMPLE_ID, SAMPLE_WITH_QC_VIEW.STATION_ID, SAMPLE_WITH_QC_VIEW.DATE_COLLECTED, SAMPLE_WITH_QC_VIEW.RECEIVE_DATE, SAMPLE_WITH_QC_VIEW.PROGRAM_TYPE, SAMPLE_WITH_QC_VIEW.SAMPLE_TYPE_NEW,
                               SAMPLE_WITH_QC_VIEW.MATRIX, SAMPLE_WITH_QC_VIEW.COLLECT_METHOD, SAMPLE_WITH_QC_VIEW.SAMPLE_TYPE, SAMPLE_WITH_QC_VIEW.QCTYPE, SAMPLE_WITH_QC_VIEW.DISCHARGE, SAMPLE_WITH_QC_VIEW.UP_DWN_STREAM, SAMPLE_WITH_QC_VIEW.WEATHER_CODE, SAMPLE_WITH_QC_VIEW.COLLECTION_AGENCY,
                               SAMPLE_WITH_QC_VIEW.COLLECTION_SPAN, SAMPLE_WITH_QC_VIEW.FIRST_TRIGGER_DATE, SAMPLE_WITH_QC_VIEW.DEPTH, SAMPLE_WITH_QC_VIEW.DEPTH_UNITS,  SAMPLE_WITH_QC_VIEW.TEST_NUMBER, SAMPLE_WITH_QC_VIEW.TEST_NAME, SAMPLE_WITH_QC_VIEW.TEST_GROUP, SAMPLE_WITH_QC_VIEW.STORET_CODE, 
                               SAMPLE_WITH_QC_VIEW.FLAG, SAMPLE_WITH_QC_VIEW.REMARK_CODE, SAMPLE_WITH_QC_VIEW.VALUE, SAMPLE_WITH_QC_VIEW.UNITS, SAMPLE_WITH_QC_VIEW.PQL, SAMPLE_WITH_QC_VIEW.MDL, SAMPLE_WITH_QC_VIEW.RDL, SAMPLE_WITH_QC_VIEW.LOWER_DEPTH, SAMPLE_WITH_QC_VIEW.UPPER_DEPTH, SAMPLE_WITH_QC_VIEW.NDEC,
                               SAMPLE_WITH_QC_VIEW.SIGFIG_VAL, SAMPLE_WITH_QC_VIEW.PERMIT_NUMBER, SAMPLE_WITH_QC_VIEW.SOURCE,SAMPLE_WITH_QC_VIEW.LIMS_NUMBER, SAMPLE_WITH_QC_VIEW.MEASURE_DATE,SAMPLE_WITH_QC_VIEW.METHOD, SAMPLE_WITH_QC_VIEW.OWNER, SAMPLE_WITH_QC_VIEW.TDEPTH, SAMPLE_WITH_QC_VIEW.SAMP_TAG, 
                               SAMPLE_WITH_QC_VIEW.SAMPLE_DESCRIPTION,  SAMPLE_WITH_QC_VIEW.ANALYST, SAMPLE_WITH_QC_VIEW.LOGIN_USER, SAMPLE_WITH_QC_VIEW.DTIM_ENTERED, SAMPLE_WITH_QC_VIEW.DTIM_MOD,SAMPLE_WITH_QC_VIEW.WORK_LAB, SAMPLE_WITH_QC_VIEW.WQ_RESULT_DATA_ID, SAMPLE_WITH_QC_VIEW.INDEX_ID, 
                               SAMPLE_WITH_QC_VIEW.PREP_DATE, SAMPLE_WITH_QC_VIEW.ALTERNATE_ID, SAMPLE_WITH_QC_VIEW.PREP_PROC_CODE, SAMPLE_WITH_QC_VIEW.DILUTION, SAMPLE_WITH_QC_VIEW.VALIDATION_LEVEL, SAMPLE_WITH_QC_VIEW.VALIDATOR, SAMPLE_WITH_QC_VIEW.SAMPLING_PURPOSE, SAMPLE_WITH_QC_VIEW.DATA_INVESTIGATION,
                               SAMPLE_WITH_QC_VIEW.UNCERTAINTY, SAMPLE_WITH_QC_VIEW.DCS, SAMPLE_WITH_QC_VIEW.FILTRATION_DATE FROM WQDORA.SAMPLE_WITH_QC_VIEW SAMPLE_WITH_QC_VIEW WHERE ((SAMPLE_WITH_QC_VIEW.DATE_COLLECTED>{ts '2019-01-01 00:00:00'}) AND (SAMPLE_WITH_QC_VIEW.COLLECT_METHOD='G'))")

Recent_data <- sqlQuery(DBHYDRO, Recent_Data_Connection_String  ) #all  Data 


#write data to csv     #most recent download 4/13/21. Contains data from 1/1/2019 to 3/18/21
write.csv(Recent_data,"Recent Data.csv")


# Import data from CSV ----------------------------------------------------

Recent_data <-read_csv("./Data/Recent Data.csv")   #imported 4/13/21 2019+
Sample_Data_2015 <- read_csv("Data/2015 Sample Data.csv")
Sample_Data_2016 <- read_csv("Data/2016 Sample Data.csv")
Sample_Data_2017 <- read_csv("Data/2017 Sample Data.csv")
Sample_Data_2018 <- read_csv("Data/2018 Sample Data.csv")


# Tidy Data ---------------------------------------------------------------
Study_analytes <- c("PHOSPHATE, ORTHO AS P","NITRATE+NITRITE-N","AMMONIA-N","PHOSPHATE, TOTAL AS P","CARBON, TOTAL ORGANIC","CHLORIDE","TOTAL NITROGEN")

Recent_data_tidy <-  bind_rows(Sample_Data_2015,Sample_Data_2016,Sample_Data_2017,Sample_Data_2018,Recent_data) %>%   
mutate(across(where(is.logical), as.numeric))%>%
mutate(across(where(is.double), as.character)) %>%
mutate(MDL=as.numeric(MDL),PQL=as.numeric(PQL),VALUE=as.numeric(VALUE))#

#Recent Samples
Recent_samples_tidy <- Recent_data_tidy  %>%
filter(SAMPLE_TYPE_NEW=="SAMP") %>%
mutate(DATE=as.Date(DATE_COLLECTED)) %>%
filter(year(as.Date(DATE_COLLECTED))<=2019) %>%
filter(year(as.Date(DATE_COLLECTED))>=2015) %>%  
filter(TEST_NAME !="STRONTIUM, DISSOLVED",TEST_NAME !="NITRATE-N") %>%    
rowwise() %>%
mutate(PNUM=str_sub(SAMPLE_ID,0,str_locate(SAMPLE_ID,"-")[1]))%>%
ungroup() 

#Blank Data
Recent_Blank_data_tidy <- Recent_data_tidy  %>%    #adds pnum column 
rowwise() %>%
mutate(PNUM=str_sub(SAMPLE_ID,0,str_locate(SAMPLE_ID,"-")[1]))%>%
ungroup() 

#Blank data wide format
Recent_Blanks_hit_freq_wide <- Quality_Control_Blanks(Recent_Blank_data_tidy)

#joined blank data with samples
Samples_with_Blanks <- Recent_samples_tidy %>% 
left_join(Recent_Blanks_hit_freq_wide,by=c("TEST_NAME","PROJECT_CODE","DATE","PNUM","COLLECTION_AGENCY","MDL","PQL"),keep=FALSE)

# #Table 2: Qualifier summary ---------------------------------------------


Recent_samples_remark_code <- Recent_samples_tidy %>%
group_by(TEST_NAME) %>%
summarise(`Total Samples`=n(),`I`=sum(str_detect(REMARK_CODE,"I"),na.rm=TRUE),`G`=sum(str_detect(REMARK_CODE,"G"),na.rm=TRUE),`J`=sum(str_detect(REMARK_CODE,"J"),na.rm=TRUE),`U`=sum(str_detect(REMARK_CODE,"U"),na.rm=TRUE),
`Y`=sum(str_detect(REMARK_CODE,"Y"),na.rm=TRUE),`Q`=sum(str_detect(REMARK_CODE,"Q"),na.rm=TRUE),`?`=sum(str_detect(REMARK_CODE,fixed("?")),na.rm=TRUE),`PMR`=sum(str_detect(REMARK_CODE,"PMR"),na.rm=TRUE)          )%>%
filter(`Total Samples`>500)

write_csv(Recent_samples_remark_code ,path="./N Contamination Data/Qualified_Data_2019.csv")

# Table 3: Qualified data as result of blank hit.  ------------------------

Qualified_Samples <- Samples_with_Blanks %>%
mutate(`Has Blank`=if_else( !is.na(EB) | !is.na(FB) | !is.na(FCEB),1,0)) %>%  #determine if sample has associated blank
filter(`Has Blank`==1)  %>% #filter out samples without associated blanks
mutate(`Highest Blank`=pmax(EB,FB,FCEB,na.rm = TRUE)) %>%
mutate(`Value < 10x MDL`=if_else(VALUE<`Highest Blank`*10,TRUE,FALSE))  %>% #find values with less thn 10x blank value
mutate(`Blank > MDL`=if_else(EB>=MDL | FB >= MDL | FCEB >=MDL,TRUE,FALSE))  %>%  #find blank hits
mutate(`Blank > MDL`=if_else(is.na(`Blank > MDL`),FALSE,`Blank > MDL`))  %>%  #convert NA data to FALSE
mutate(`Qualified from Blank`= if_else(`Value < 10x MDL`==TRUE & `Blank > MDL`==TRUE,TRUE,FALSE))  #qualify data 

Percent_Samples_Qualified <-Qualified_Samples %>%
group_by(TEST_NAME) %>%
summarise(`Total Samples`=n(),
          `Blank >= to MDL`=sum(`Blank > MDL`,na.rm=TRUE),
          `% Blank >= MDL`=percent(`Blank >= to MDL`/`Total Samples`),
          `Qualified Data from Blank >= MDL`=sum(`Qualified from Blank`),
          `G Qualified`=sum(str_detect(REMARK_CODE,"G"),na.rm=TRUE),
          `% Qualified from Blank >= MDL`=paste0(round((`Qualified Data from Blank >= MDL`/`Total Samples`*100),1),"%"),
          `% G Qualified`=paste0(round((`G Qualified`/`Total Samples`*100),1),"%")) %>%
filter(`Total Samples`>500)  %>%
arrange(desc(`% Qualified from Blank >= MDL`))

write_csv(Percent_Samples_Qualified ,path="./N Contamination Data/Percent_Samples_Qualified.csv")



# Plot of qualifed data  --------------------------------------------------


Percent_Samples_Qualified_renamed <-Percent_Samples_Qualified %>% 
mutate(`Percent of Data Qualified as Result of Blank Contamination`=`Qualified Data from Blank >= MDL`/`Total Samples`) %>% filter(`Percent of Data Qualified as Result of Blank Contamination`>0)

Percent_Samples_Qualified_renamed_plot  <-ggplot(Percent_Samples_Qualified_renamed ,aes(reorder(TEST_NAME,-`Percent of Data Qualified as Result of Blank Contamination`),`Percent of Data Qualified as Result of Blank Contamination`))+
geom_col()+theme_bw()+
theme(legend.position = "bottom",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12),axis.text.y = element_text(size=12),axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14,hjust = 1))+
labs(x="Analyte",y="Qualified Data from Blank Contamination (%)")+scale_y_continuous(breaks= pretty_breaks(n=10),labels = percent)

ggsave("./Figures/Percent_Samples_Qualified_renamed_plot 2015-19.jpeg",plot=Percent_Samples_Qualified_renamed_plot  ,height=6,width=10,units="in")


# Functions ---------------------------------------------------------------


Percent_qualified1 <-function(df,`New MDL`)
{
df1 <- df %>%
  mutate(`Has Blank`=if_else( !is.na(EB) | !is.na(FB) | !is.na(FCEB),1,0)) %>%  #determine if sample has associated blank
  filter(`Has Blank`==1)  %>% #filter out samples without associated blanks
  mutate(`Highest Blank`=pmax(EB,FB,FCEB,na.rm = TRUE)) %>%
  mutate(`Value < 10x MDL`=if_else(VALUE<`Highest Blank`*10,TRUE,FALSE))  %>% #find values with less thn 10x MDL
  mutate(`Blank > MDL`=if_else(EB>=`New MDL`| FB >=`New MDL` | FCEB >=`New MDL`,TRUE,FALSE))  %>%  #find blank hits
  mutate(`Blank > MDL`=if_else(is.na(`Blank > MDL`),FALSE,`Blank > MDL`))  %>%  #convert NA data to FALSE
  mutate(`Qualified from Blank`= if_else(`Value < 10x MDL`==TRUE & `Blank > MDL`==TRUE,TRUE,FALSE)) %>%
  group_by(TEST_NAME) %>%
  summarise(  
    `Total Samples`=n(),
    `Blank >= to MDL`=sum(`Blank > MDL`,na.rm=TRUE),
    `% Blank >= MDL`=`Blank >= to MDL`/`Total Samples`,
    `Qualified Data from Blank >= MDL`=sum(`Qualified from Blank`),
    `% Qualified from Blank >= MDL`=`Qualified Data from Blank >= MDL`/`Total Samples`) %>%
filter(`Total Samples`>500)  %>%
arrange(desc(`% Qualified from Blank >= MDL`))
return(df1)
}

Find_EB <-function(df)   #
{  
  df1 <-df  %>%
    mutate(DATE=as.Date(DATE_COLLECTED)) %>%
    filter(SAMPLE_TYPE_NEW=="EB") %>%
    select(PNUM,DATE,TEST_NAME,VALUE) %>%
    rename(EB=VALUE)
  return(df1)
}

Find_FB <-function(df)   #
{  
  df1 <-df  %>%
    mutate(DATE=as.Date(DATE_COLLECTED)) %>%
    filter(SAMPLE_TYPE_NEW=="FB") %>%
    select(PNUM,DATE,TEST_NAME,VALUE) %>%
    rename(FB=VALUE)
  return(df1)
}

#find Blanks
Find_FCEB <-function(df)
{  
  df1 <-df  %>%
    mutate(DATE=as.Date(DATE_COLLECTED)) %>%
    filter(SAMPLE_TYPE_NEW=="FCEB") %>%
    select(DATE,PNUM,TEST_NAME,VALUE) %>%
    rename(FCEB=VALUE) 
  return(df1)
}

Quality_Control_Blanks <-function(df)
{
  df1 <-df  %>% 
    mutate(DATE=as.Date(DATE_COLLECTED)) %>%
    distinct(DATE,PROJECT_CODE,COLLECTION_AGENCY,TEST_NAME,PNUM,MDL,PQL)  %>%
    left_join(Find_FCEB(df), by=c("DATE","PNUM","TEST_NAME"),keep=FALSE) %>%    #join FCEBs
    left_join(Find_EB(df), by=c("DATE","PNUM","TEST_NAME"),keep=FALSE) %>%          #Join EBs
    left_join(Find_FB(df), by=c("DATE","PNUM","TEST_NAME"),keep=FALSE)
  return(df1)
}





