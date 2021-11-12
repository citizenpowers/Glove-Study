
.libPaths("H:/Docs/R/win-library/3.4") #use when using work computer
remove(list=ls()) #removes all objects from project


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
#1.) download data from DBHYDRO and save to csv. Step takes long time. skip to step 2 unless data needs refresh 
#2.) upload data from csv. Fast
#3.) Tidy data. puts data in standard format
#4.) Run functions. THese help break analysis into managable steps
#5.) Run Blank ANalysis. Creates figs and tables
#6.) Run markdown file. Takes figs and table and knits into report.


# Import data from CSV ------------------#Data Imported from DBHYDRO 4/13/2021---------------------------------


EB_data <-read_csv("./Data/EB Data.csv")   

FCEB_data <- read_csv("./Data/FCEB Data.csv")

FB_data <- read_csv("./Data/FB Data.csv")


# Tidy Data ---------------------------------------------------------------
EB_data_tidy <- EB_data %>%
mutate(across(where(is.logical), as.numeric))%>%
mutate(across(where(is.double), as.character)) %>%
mutate(MDL=as.numeric(MDL),PQL=as.numeric(PQL),VALUE=as.numeric(VALUE))

FB_data_tidy <- FB_data %>%
mutate(across(where(is.logical), as.numeric))%>%
mutate(across(where(is.double), as.character)) %>%
mutate(MDL=as.numeric(MDL),PQL=as.numeric(PQL),VALUE=as.numeric(VALUE))

FCEB_data_tidy <- FCEB_data %>%
mutate(across(where(is.logical), as.numeric))%>%
mutate(across(where(is.double), as.character)) %>%
mutate(MDL=as.numeric(MDL),PQL=as.numeric(PQL),VALUE=as.numeric(VALUE))

Blank_data_tidy_all <- bind_rows(EB_data_tidy,FB_data_tidy,FCEB_data_tidy) %>%   #combine dataframes into single df
filter(year(as.Date(DATE_COLLECTED))<=2019) %>% #filter data to from 2015 through 2019
filter(year(as.Date(DATE_COLLECTED))>=2015) 

Blank_data_tidy <- Blank_data_tidy_all %>%    #adds pnum column 
rowwise() %>%
mutate(PNUM=str_sub(SAMPLE_ID,0,str_locate(SAMPLE_ID,"-")[1]))%>%
ungroup() 


# #figure 1 Analytes above MDL by Frequency -------------------------------


Top_above_PQL_MDL <-Quality_Control_Blanks(Blank_data_tidy) %>%
gather("Blank Type","Value",8:10) %>%
mutate(YEAR=year(DATE)) %>% 
#filter(YEAR >=2019)    %>%    #just last two years of data 
filter(TEST_NAME %in% c("STRONTIUM, DISSOLVED","NITRATE-N")==FALSE) %>%  
group_by(TEST_NAME,YEAR) %>%
summarise(`Total FCEB`=sum(`Blank Type`=="FCEB" & !is.na(Value)),
            `FCEB > MDL`=sum(`Blank Type`=="FCEB" & Value >= MDL,na.rm=TRUE),
            `FCEB Percent >MDL`=if_else(`FCEB > MDL`>0,`FCEB > MDL`/`Total FCEB`,0),
            `FCEB > PQL`=sum(`Blank Type`=="FCEB" & Value >= PQL,na.rm=TRUE),
            `FCEB Percent >PQL`=if_else(`FCEB > PQL`>0,`FCEB > PQL`/`Total FCEB`,0),
            `Total EB`=sum(`Blank Type`=="EB" & !is.na(Value)),
            `EB > MDL`=sum(`Blank Type`=="EB" & Value >= MDL,na.rm=TRUE),
            `EB Percent >MDL`=if_else(`EB > MDL`>0,`EB > MDL`/`Total EB`,0),
            `EB > PQL`=sum(`Blank Type`=="EB" & Value >= PQL,na.rm=TRUE),
            `EB Percent >PQL`=if_else(`EB > PQL`>0,`EB > PQL`/`Total EB`,0),
            `Total FB`=sum(`Blank Type`=="FB" & !is.na(Value)),
            `FB > MDL`=sum(`Blank Type`=="FB" & Value >= MDL,na.rm=TRUE),
            `FB Percent >MDL`=if_else(`FB > MDL`>0,`FB > MDL`/`Total FB`,0),
            `FB > PQL`=sum(`Blank Type`=="FB" & Value >= PQL,na.rm=TRUE),
            `FB Percent >PQL`=if_else(`FB > PQL`>0,`FB > PQL`/`Total FB`,0),
            `Total Blanks`=`Total FCEB`+`Total FB`+`Total EB`,
            `Total Blanks Greater Than MDL`=`FCEB > MDL`+`FB > MDL`+`EB > MDL`,
            `Percent Total Blanks Greater than or Equal to MDL`=`Total Blanks Greater Than MDL`/`Total Blanks`,
            `Total Blanks Greater Than PQL`=`FCEB > PQL`+`FB > PQL`+`EB > PQL`,
            `Percent Total Blanks Greater than or Equal to PQL`=`Total Blanks Greater Than PQL`/`Total Blanks`)

Top_above_MDL_top_hits <-Top_above_PQL_MDL %>%
filter(`Total Blanks`>100,`Percent Total Blanks Greater than or Equal to MDL`>0) 

#hits above MDL
Top_above_MDL_top_hits_plot <-ggplot(Top_above_MDL_top_hits ,aes(stats::reorder(TEST_NAME,-`Percent Total Blanks Greater than or Equal to MDL`),`Percent Total Blanks Greater than or Equal to MDL`))+
geom_col()+theme_bw()+
theme(legend.position = "bottom",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12),axis.text.y = element_text(size=12),axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14))+
labs(x="Analyte",y="Blanks above or equal to MDL (%) ")+scale_y_continuous(breaks= pretty_breaks(n=10),labels = percent)

ggsave("./Figures/Blanks above MDL by Analyte 2015-2019.jpeg",plot=Top_above_MDL_top_hits_plot ,height=6,width=10,units="in")

#Ammonia Blanks FCEB vs EB
NH3_FCEB_vs_EB <-Top_above_PQL_MDL %>%
filter(TEST_NAME=="AMMONIA-N")  %>%
select(TEST_NAME,`FCEB Percent >MDL`,`EB Percent >MDL`) %>%
pivot_longer(2:3,names_to = "QC Blank Type", values_to="Values")

ggplot(NH3_FCEB_vs_EB,aes(`QC Blank Type`,Values))+geom_col()+theme_bw()


# #Figure 3 Appendix  Frequency of hits above MDL and PQL ---------------------------

All_Blanks_hit_freq_wide <- Quality_Control_Blanks(Blank_data_tidy) %>%
gather("Blank Type","Value",8:10) %>%
mutate(YEAR=year(DATE),MONTH=month(DATE)) %>% 
group_by(YEAR,MONTH,TEST_NAME) %>%
summarise(n=n(),
            `Min MDL`=min(MDL,na.rm = TRUE),
            `Min PQL`=min(PQL,na.rm = TRUE),
            `Max MDL`=max(MDL,na.rm = TRUE),
            `Max PQL`=max(PQL,na.rm = TRUE),
            `Total FCEB`=sum(`Blank Type`=="FCEB" & !is.na(Value)),
            `FCEB > MDL`=sum(`Blank Type`=="FCEB" & Value >= MDL,na.rm=TRUE),
            `FCEB Percent >MDL`=if_else(`FCEB > MDL`>0,`FCEB > MDL`/`Total FCEB`,0),
            `FCEB > PQL`=sum(`Blank Type`=="FCEB" & Value >= PQL,na.rm=TRUE),
            `FCEB Percent >PQL`=if_else(`FCEB > PQL`>0,`FCEB > PQL`/`Total FCEB`,0),
            `Total EB`=sum(`Blank Type`=="EB" & !is.na(Value)),
            `EB > MDL`=sum(`Blank Type`=="EB" & Value >= MDL,na.rm=TRUE),
            `EB Percent >MDL`=if_else(`EB > MDL`>0,`EB > MDL`/`Total EB`,0),
            `EB > PQL`=sum(`Blank Type`=="EB" & Value >= PQL,na.rm=TRUE),
            `EB Percent >PQL`=if_else(`EB > PQL`>0,`EB > PQL`/`Total EB`,0),
            `Total FB`=sum(`Blank Type`=="FB" & !is.na(Value)),
            `FB > MDL`=sum(`Blank Type`=="FB" & Value >= MDL,na.rm=TRUE),
            `FB Percent >MDL`=if_else(`FB > MDL`>0,`FB > MDL`/`Total FB`,0),
            `FB > PQL`=sum(`Blank Type`=="FB" & Value >= PQL,na.rm=TRUE),
            `FB Percent >PQL`=if_else(`FB > PQL`>0,`FB > PQL`/`Total FB`,0),
            `Total Blanks`=`Total FCEB`+`Total FB`+`Total EB`,
            `Total Blanks Greater Than MDL`=`FCEB > MDL`+`FB > MDL`+`EB > MDL`,
            `Percent Total Blanks Greater than or Equal to MDL`=`Total Blanks Greater Than MDL`/`Total Blanks`,
            `Total Blanks Greater Than PQL`=`FCEB > PQL`+`FB > PQL`+`EB > PQL`,
            `Percent Total Blanks Greater than or Equal to PQL`=`Total Blanks Greater Than PQL`/`Total Blanks`) 
 
Analytes_above_MDL <- unlist(distinct(Top_above_MDL_top_hits,TEST_NAME)[,1])   #create vector of analytes with blanks above the MDL

All_Blanks_hit_freq <- All_Blanks_hit_freq_wide %>%
gather("Blank Type","VALUE",4:28)  %>%
filter(TEST_NAME %in% Analytes_above_MDL)   #filter analytes 


#Total Blanks percent above MDL and PQL
All_Blanks_hit_freq_plot <- ggplot(filter(All_Blanks_hit_freq,str_detect(`Blank Type`,"Percent Total"),TEST_NAME %in% c("NITRATE+NITRITE-N","AMMONIA-N")),aes(make_date(YEAR,MONTH,1),VALUE,fill=`Blank Type`,color=`Blank Type`))+
geom_smooth(method = "loess",se=FALSE)+facet_wrap(~TEST_NAME,ncol=1 )+theme_bw() + geom_point(color="black",size=2,shape=21)+scale_fill_brewer(palette = "Set2")+scale_color_brewer(palette = "Set2")+
scale_y_continuous(breaks= pretty_breaks(n=5),labels = percent) +theme(legend.position = "bottom",axis.text.x = element_text(angle = 90)) +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12),axis.text.y = element_text(size=12),axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),title =element_text(size = 14),legend.text=element_text(size=12),strip.text = element_text(size=12))+
labs(x="Date",y="Monthly Blank Hit Frequency (%)",title = "")+
scale_x_date(labels=date_format("%b %y"),breaks =seq(as.Date('2015-01-01'),as.Date('2021-04-01'),by = "3 months") )+coord_cartesian(xlim = c(as.Date(ISOdate(2015, 01, 01, 0)),as.Date(ISOdate(2021, 04, 01, 0))))

ggsave("./Figures/Percent_Samples_Qualified_renamed_plot.jpeg",plot=All_Blanks_hit_freq_plot ,height=6,width=10,units="in")


# Functions ---------------------------------------------------------------

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

PARAMETER_Names <- function(df) #convert to standard analyte names. DBHYDRO TEST_NAME to ERDP PARAMETER 
{
  df1 <- df %>%
    mutate(TEST_NAME=case_when(TEST_NAME=="TEMP"~"Temp",
                               TEST_NAME=="DISSOLVED OXYGEN"~"DO",
                               TEST_NAME=="SP CONDUCTIVITY, FIELD"~"SpCond",
                               TEST_NAME=="PH, FIELD"~"PH",
                               TEST_NAME=="TOTAL SUSPENDED SOLIDS"~"TSS",
                               TEST_NAME=="NITRATE+NITRITE-N"~"NOX",
                               TEST_NAME=="AMMONIA-N"~"NH4",
                               TEST_NAME=="PHOSPHATE, ORTHO AS P"~"OPO4",
                               TEST_NAME=="PHOSPHATE, TOTAL AS P"~"TPO4",
                               TEST_NAME=="PHOSPHATE, DISSOLVED AS P"~"TDPO4",
                               TEST_NAME=="SODIUM"~"NA",
                               TEST_NAME=="POTASSIUM"~"K",
                               TEST_NAME=="CALCIUM"~"CA",
                               TEST_NAME=="MAGNESIUM"~"MG",
                               TEST_NAME=="CHLORIDE"~"CL",
                               TEST_NAME=="SULFATE"~"SO4",
                               TEST_NAME=="HARDNESS AS CACO3"~"HARDNESS",
                               TEST_NAME=="ALKALINITY, TOT, CACO3"~"ALKA",
                               TEST_NAME=="TOTAL NITROGEN"~"TN",
                               TEST_NAME=="DEPTH, TOTAL"~"Depth",
                               TEST_NAME=="NO BOTTLE SAMPLE"~"No Bottle",
                               TEST_NAME=="CARBON, DISSOLVED ORGANIC"~"DOC",
                               TEST_NAME=="TURBIDITY"~"Turbidity",
                               TEST_NAME=="VOLATILE SUSPENDED SOLIDS"~"VSS",
                               TEST_NAME=="CHLOROPHYLL-A(LC)"~"CHLOROPHYLL A",
                               TEST_NAME=="PHEOPHYTIN-A(LC)"~"PHEOPHYTIN A"))
  return(df1)                             
}




