
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
library(RVAideMemoire)

#Steps
#1.) 
#2.) 
#3.) Tidy data. puts data in standard format
#4.) Run functions. THese help break analysis into managable steps
#5.) Run Blank ANalysis. Creates figs and tables
#6.) Run markdown file. Takes figs and table and knits into report.


# Import data from CSV ----------------------------------------------------


EB_data <-read_csv("Data/EB Data.csv")

FCEB_data <- read_csv("Data/FCEB Data.csv")

FB_data <- read_csv("Data/FB Data.csv")

QC_data <- read_csv("Data/Qc Data.csv")


# Tidy Data ---------------------------------------------------------------
Study_analytes <- c("PHOSPHATE, ORTHO AS P","NITRATE+NITRITE-N","AMMONIA-N","PHOSPHATE, TOTAL AS P","CARBON, TOTAL ORGANIC","CHLORIDE","TOTAL NITROGEN")
NOX_NH4 <- c("NITRATE+NITRITE-N","AMMONIA-N")
Excluded_Projects  <- c("ACRA","RAIN","S356FT","EVER","C111SC","TOHONRP")


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

Pre_guidance <- bind_rows(EB_data_tidy,FB_data_tidy,FCEB_data_tidy) %>%   #combine dataframes into single df
filter(as.Date(DATE_COLLECTED)<"2021-03-01 00:00:00") %>% #Pre-Guidance data
filter(as.Date(DATE_COLLECTED) > "2019-01-01 00:00:00") %>%  
filter(PROJECT_CODE %in% Excluded_Projects != TRUE) %>%   #Excluded projects list.
filter(TEST_NAME!="STRONTIUM, DISSOLVED")

Post_guidance <-bind_rows(EB_data_tidy,FB_data_tidy,FCEB_data_tidy) %>%   #combine dataframes into single df
filter(as.Date(DATE_COLLECTED)>="2021-03-01 00:00:00") %>%  #Post-Guidance data
filter(PROJECT_CODE %in% Excluded_Projects != TRUE) %>%  #Excluded projects list.
filter(TEST_NAME!="STRONTIUM, DISSOLVED")

Pre_guidance_tidy <- Pre_guidance %>%    #adds pnum column 
rowwise() %>%
mutate(PNUM=str_sub(SAMPLE_ID,0,str_locate(SAMPLE_ID,"-")[1]))%>%
ungroup()

Post_guidance_tidy <- Post_guidance %>%    #adds pnum column 
rowwise() %>%
mutate(PNUM=str_sub(SAMPLE_ID,0,str_locate(SAMPLE_ID,"-")[1]))%>%
ungroup()

# Pre-Guidance Analytes above MDL by Frequency -------------------------------

Pre_guidance_Top_above_PQL_MDL <-Quality_Control_Blanks(Pre_guidance_tidy) %>%
gather("Blank Type","Value",8:10) %>%
group_by(TEST_NAME) %>%
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
            `Percent Total Blanks Greater Than MDL`=`Total Blanks Greater Than MDL`/`Total Blanks`,
            `Total Blanks Greater Than PQL`=`FCEB > PQL`+`FB > PQL`+`EB > PQL`,
            `Percent Total Blanks Greater Than PQL`=`Total Blanks Greater Than PQL`/`Total Blanks`)

Pre_guidance_Top_above_MDL_top_hits <-Pre_guidance_Top_above_PQL_MDL %>%
filter(`Total Blanks`>100,`Percent Total Blanks Greater Than MDL`>0) 

Pre_guidance_Top_above_MDL_top_hits_plot <-ggplot(Pre_guidance_Top_above_MDL_top_hits ,aes(reorder(TEST_NAME,-`Percent Total Blanks Greater Than MDL`),`Percent Total Blanks Greater Than MDL`))+geom_col()+theme_bw()+theme(legend.position = "bottom",axis.text.x = element_text(angle = 90))+labs(x="Analyte",y="Blanks above or equal to MDL (%)")+scale_y_continuous(breaks= pretty_breaks(n=10),labels = percent)

ggsave("./N Contamination Data/Pre Guidance Blanks above MDL by Analyte.jpeg",plot=Pre_guidance_Top_above_MDL_top_hits_plot ,height=5,width=11,units="in")


# Post Guidance Blanks above MDL ------------------------------------------

Post_guidance_Top_above_PQL_MDL <-Quality_Control_Blanks(Post_guidance_tidy) %>%
gather("Blank Type","Value",8:10) %>%
group_by(TEST_NAME) %>%
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
            `Percent Total Blanks Greater Than MDL`=`Total Blanks Greater Than MDL`/`Total Blanks`,
            `Total Blanks Greater Than PQL`=`FCEB > PQL`+`FB > PQL`+`EB > PQL`,
            `Percent Total Blanks Greater Than PQL`=`Total Blanks Greater Than PQL`/`Total Blanks`)

Post_guidance_Top_above_MDL_top_hits <-Post_guidance_Top_above_PQL_MDL %>%
filter(`Total Blanks`>50,`Percent Total Blanks Greater Than MDL`>0) 

Post_guidance_Top_above_MDL_top_hits_plot <-ggplot(Post_guidance_Top_above_MDL_top_hits ,aes(reorder(TEST_NAME,-`Percent Total Blanks Greater Than MDL`),`Percent Total Blanks Greater Than MDL`))+geom_col()+theme_bw()+theme(legend.position = "bottom",axis.text.x = element_text(angle = 90))+labs(x="Analyte",y="Blanks above or equal to MDL (%)")+scale_y_continuous(breaks= pretty_breaks(n=10),labels = percent)

ggsave("/Figures/Post Guidabce Blanks above MDL by Analyte.jpeg",plot=Post_guidance_Top_above_MDL_top_hits_plot ,height=5,width=11,units="in")


# Pre and Post guidance joined --------------------------------------------

Pre_post_Guidance_data <-select(Pre_guidance_Top_above_MDL_top_hits,TEST_NAME,`Percent Total Blanks Greater Than MDL`) %>%
rename(`Pre-Guidance`="Percent Total Blanks Greater Than MDL" ) %>%
left_join(select(Post_guidance_Top_above_MDL_top_hits,TEST_NAME,`Percent Total Blanks Greater Than MDL`),by="TEST_NAME")  %>%
rename(`Post-Guidance`="Percent Total Blanks Greater Than MDL" )   %>%
drop_na()  %>% #remove NA values
pivot_longer(names_to = "Period",values_to="Values",2:3 ) %>%
mutate(Period = factor(Period, levels = c("Pre-Guidance","Post-Guidance")))

Pre_post_Guidance_plot <-ggplot(Pre_post_Guidance_data,aes(reorder(TEST_NAME,-Values),Values,fill=Period))+geom_col(position="dodge")+theme_bw()+scale_fill_manual(values = c("grey30","grey80"))+
theme(legend.position = "bottom",axis.text.x = element_text(angle = 90))+labs(x="Analyte",y="Blanks above or equal to MDL (%)",caption = "Pre-guidance Jan 1st, 2019 to February 28th 2021. Post-Guidance March 1st, 2021 to June 8th, 2021")+scale_y_continuous(breaks= pretty_breaks(n=10),labels = percent)

ggsave("./Figures/Pre and post-guidance Blank hit Frequency.jpeg",plot=Pre_post_Guidance_plot  ,height=8,width=11,units="in")



# Fishers Exact Test ------------------------------------------------------

#filter data to March 1st to June 8th for 2019 and 2020 to compare against post-guidance period in 2021
Hits_by_year <- bind_rows(EB_data_tidy,FB_data_tidy,FCEB_data_tidy) %>%   #combine dataframes into single df
filter(PROJECT_CODE %in% Excluded_Projects != TRUE) %>%   #Excluded projects list.
filter(DATE_COLLECTED>"2015-01-01 00:00:00") %>%
mutate(Year=year(DATE_COLLECTED),Month=month(DATE_COLLECTED),Day=day(DATE_COLLECTED)) %>%
filter(Month >=3, Month <=6) %>%
filter(if_else(Month==6 & Day >8,TRUE,FALSE)==FALSE) %>%  #Exclude data from June 9 onward for each year
filter(TEST_NAME %in% NOX_NH4) %>%  
rowwise() %>%
mutate(PNUM=str_sub(SAMPLE_ID,0,str_locate(SAMPLE_ID,"-")[1]))%>%
ungroup() %>%
Quality_Control_Blanks() %>%
mutate(Year=year(DATE)) %>%  
gather("Blank Type","Value",8:10) %>%
group_by(TEST_NAME,Year) %>%
Blank_Hit_Summary()

#DF of hit frequency for NOx
Hits_by_year_NOx <- Hits_by_year %>%
filter(TEST_NAME=="NITRATE+NITRITE-N") %>%
ungroup() %>%
mutate(`Non_detects`=`Total Blanks`-`Total Blanks Greater Than MDL`)  %>%
select(Year,`Total Blanks Greater Than MDL`,`Non_detects`) %>%
rename(`Detects`="Total Blanks Greater Than MDL")  %>%
mutate(Year=as.character(Year))%>%  
as.matrix(header=TRUE,row.names=1)

#NOX 2015- 2021
NOx_Input =("Year   Hits  non_detects
        Y2015   39    525
        Y2016   24    478
        Y2017   29    407
        Y2018   34    414
        Y2019   23    389
        Y2020   12    470
        Y2021   1     434    ")

#Create matrix from NOx DF
NOX_matrix = as.matrix(read.table(textConnection(NOx_Input), header=TRUE,row.names=1))
.05/7
G.test(NOX_matrix )
pairwise.G.test(NOX_matrix,p.method = "none")    

Hits_by_year_NH4 <- Hits_by_year %>%
filter(TEST_NAME=="AMMONIA-N") %>%
ungroup() %>%
mutate(`Non_detects`=`Total Blanks`-`Total Blanks Greater Than MDL`)  %>%
select(Year,`Total Blanks Greater Than MDL`,`Non_detects`) %>%
rename(`Detects`="Total Blanks Greater Than MDL")  %>%
mutate(Year=as.character(Year))%>%  
as.matrix(header=TRUE,row.names=1)

#NH4 2015 vs 2020
NH4_Input =("Year   Hits  non_detects
            Y2015   37    404
            Y2016   51    294
            Y2017   30    292
            Y2018   17    329
            Y2019   6     339
            Y2020   6     416
            Y2021   24    355    ")

#Create matrix from NOx DF
NH4_matrix = as.matrix(read.table(textConnection(NH4_Input), header=TRUE,row.names=1))
.05/7
G.test(NH4_matrix )
pairwise.G.test(NH4_matrix,p.method = "none")  

# #Fig 6 study analytes ---------------------------------------------------


NOX_NH4_study <- All_Blanks_hit_freq_wide %>%
gather("Blank Type","VALUE",4:28)  %>%
filter(TEST_NAME %in% NOX_NH4 )   #filter analytes 

All_Blanks_hit_freq_study <- All_Blanks_hit_freq_wide %>%
gather("Blank Type","VALUE",4:28)  %>%
filter(TEST_NAME %in% Study_analytes)   #filter analytes 

All_Blanks_hit_freq_study_plot <- ggplot(filter(All_Blanks_hit_freq_study,str_detect(`Blank Type`,"Percent Total")),aes(make_date(YEAR,MONTH,15),VALUE,fill=`Blank Type`))+geom_rect(xmin = make_date(2020,5,1), ymin=0, xmax=make_date(2020,12,1), ymax = 1,fill="grey",alpha=.05)+  
geom_col(color="black")+facet_grid(TEST_NAME~`Blank Type`)+theme_bw()+scale_fill_brewer(palette = "Set2")+scale_color_brewer(palette = "Set2")+
scale_y_continuous(breaks= pretty_breaks(n=10),labels = percent) +theme(legend.position = "bottom",axis.text.x = element_text(angle = 90)) +
labs(x="Year",y="Monthly Blank Hit Frequency (%)",title = "")+
scale_x_date(date_breaks  ="3 month",labels=date_format("%b %y"),limits=c(as.Date(ISOdate(2018, 12, 01, 0)),as.Date(ISOdate(2020, 11, 01, 0))))

ggsave("./N Contamination Data/Study_analytes_freq_blank_hits 2019.jpeg",plot=All_Blanks_hit_freq_study_plot  ,height=8,width=11,units="in")

ggsave("./N Contamination Data/Study_analytes_freq_blank_hits 2019_potriat.jpeg",plot=All_Blanks_hit_freq_study_plot  ,height=14,width=11,units="in")

NOX_NH4_study_plot <- ggplot(filter(NOX_NH4_study,str_detect(`Blank Type`,"Percent Total")),aes(make_date(YEAR,MONTH,15),VALUE,fill=`Blank Type`))+geom_rect(xmin = make_date(2020,5,1), ymin=0, xmax=make_date(2020,12,1), ymax = 1,fill="grey",alpha=.05)+  
geom_col(color="black")+facet_grid(TEST_NAME~`Blank Type`)+theme_bw()+scale_fill_brewer(palette = "Set2")+scale_color_brewer(palette = "Set2")+
scale_y_continuous(breaks= pretty_breaks(n=10),labels = percent) +theme(legend.position = "bottom",axis.text.x = element_text(angle = 90)) +
labs(x="Year",y="Monthly Blank Hit Frequency (%)",title = "")+
scale_x_date(date_breaks  ="3 month",labels=date_format("%b %y"),limits=c(as.Date(ISOdate(2018, 12, 01, 0)),as.Date(ISOdate(2020, 11, 01, 0))))

ggsave("./N Contamination Data/Study_analytes_freq_blank_hits_NOX_NH4_study_plot.jpeg",plot=NOX_NH4_study_plot  ,height=8,width=11,units="in")


# #Fig 7: blank hit by blank type -----------------------------------------


All_Blanks_hit_freq_type_plot <- ggplot(filter(All_Blanks_hit_freq,str_detect(`Blank Type`,"B Percent >MDL")),aes(make_date(YEAR,MONTH,1),VALUE,fill=`Blank Type`,color=`Blank Type`))+
geom_smooth(method = "loess",se=FALSE)+facet_wrap(~TEST_NAME,scales ="free_y",ncol=4 )+theme_bw() + geom_point(size=.5)+scale_fill_brewer(palette = "Set2")+scale_color_brewer(palette = "Set2")+
scale_y_continuous(breaks= pretty_breaks(n=5),labels = percent) +theme(legend.position = "bottom",axis.text.x = element_text(angle = 90)) +
labs(x="Date",y="Monthly Blank Hit Frequency (%)",title = "")+scale_x_date(date_breaks  ="3 month",labels=date_format("%b %y"),limits=c(as.Date(ISOdate(2018, 12, 01, 0)),as.Date(ISOdate(2020, 11, 01, 0))))

ggsave("./N Contamination Data/All_Blanks_hit_freq_type_MDL_plot 2019.jpeg",plot=All_Blanks_hit_freq_type_plot  ,height=12,width=11,units="in")


# #Fig 8: blank hit by blank type -----------------------------------------

All_Blanks_hit_freq_type_study_plot <- ggplot(filter(All_Blanks_hit_freq_study,str_detect(`Blank Type`,"B Percent >MDL")),aes(make_date(YEAR,MONTH,15),VALUE,fill=`Blank Type`))+geom_rect(xmin = make_date(2020,5,1), ymin=0, xmax=make_date(2020,12,1), ymax = 1,fill="grey",alpha=.05)+  
geom_col(color="black")+facet_grid(TEST_NAME~`Blank Type`)+theme_bw()+scale_fill_brewer(palette = "Set2")+scale_color_brewer(palette = "Set2")+
scale_y_continuous(breaks= pretty_breaks(n=10),labels = percent,limits=c(0,.5)) +theme(legend.position = "bottom",axis.text.x = element_text(angle = 90)) +
labs(x="Date",y="Monthly Blank Hit Frequency (%)",title = "")+scale_x_date(date_breaks  ="1 month",labels=date_format("%b %y"),limits=c(as.Date(ISOdate(2018, 12, 01, 0)),as.Date(ISOdate(2020, 11, 01, 0))))

ggsave("./N Contamination Data/All_Blanks_hit_freq_type_study_plot 2019.jpeg",plot=All_Blanks_hit_freq_type_study_plot  ,height=14,width=11,units="in")


# Other figs --------------------------------------------------------------


# Percent greater than MDL of hits
Percent_greater_MDL<-Quality_Control_Blanks(Blank_data_tidy) %>%
gather("Blank Type","Value",8:10) %>%
mutate(`Blank Percent Greater Than MDL`=if_else(!is.na(Value) & !is.na(MDL) & Value > MDL,Value/MDL,NaN)) %>%
filter(is.finite(`Blank Percent Greater Than MDL`)) %>%
mutate(`Value ug/l`=Value*1000)

Percent_greater_MDL_Analytes_above_MDL <- Percent_greater_MDL %>%
filter(TEST_NAME %in% Analytes_above_MDL)

#Percent above MDL -all analytes with blank hits
ggplot(Percent_greater_MDL_Analytes_above_MDL,aes(`Blank Percent Greater Than MDL`))+geom_histogram()+facet_wrap(~TEST_NAME,scales = "free")+theme_bw()+scale_y_log10()

Percent_greater_MDL_Study_analytes <- Percent_greater_MDL %>%
filter(TEST_NAME %in% Study_analytes )  

#percent above MDL study analytes
ggplot(Percent_greater_MDL_Study_analytes,aes(`Blank Percent Greater Than MDL`))+geom_histogram()+facet_wrap(~TEST_NAME,scales = "free")+theme_bw()+
scale_y_log10()

#Fig 9: concentration where Blank Hitsoccur study analytes
Concentration_where_blanks_occur <-ggplot(Percent_greater_MDL_Study_analytes,aes(y=`Blank Type`,x=`Value ug/l`, fill=`Blank Type`))+geom_density_ridges()+theme_bw()+facet_wrap(~TEST_NAME,nrow = 4)+
scale_x_continuous(trans = log_trans(),breaks=log_breaks(8),labels = prettyNum,limits = c(1,2500))+scale_fill_brewer(palette = "Set2")+labs(y="Density of Blank Hits by Type")

ggsave("./N Contamination Data/Concentration where blanks hits occur- study analytes.jpeg",plot=Concentration_where_blanks_occur,height=8,width=11,units="in")

#Blank Hits in above MDL study analytes by year TPO4
ggplot(filter(Percent_greater_MDL_Study_analytes,TEST_NAME=="PHOSPHATE, TOTAL AS P"),aes(y=`Blank Type`,x=`Value ug/l`,fill=`Blank Type`))+geom_density_ridges()+theme_bw()+facet_wrap(~year(DATE))+
scale_x_continuous(trans = log_trans(),breaks=log_breaks(8),labels = prettyNum)+scale_fill_brewer(palette = "Set2")

#Blank Hits in above MDL study analytes by year
ggplot(Percent_greater_MDL_Study_analytes,aes(y=`Blank Type`,x=`Value ug/l`,fill=`Blank Type`))+geom_density_ridges()+theme_bw()+facet_wrap(~year(DATE))+
scale_x_continuous(trans = log_trans(),breaks=log_breaks(8),labels = prettyNum,limits = c(1,1500))+scale_fill_brewer(palette = "Set2")

#Annual trends
ggplot(filter(All_Blanks_hit_freq,str_detect(`Blank Type`,"Percent Total")),aes(x=VALUE,y=reorder(as.factor(YEAR),-YEAR),fill=`Blank Type`))+facet_wrap(~TEST_NAME)+
geom_density_ridges(alpha=.6)+scale_fill_brewer(palette = "Set2")+theme_bw()+scale_x_continuous(labels = percent,limits = c(0,1))

#Sesonality monthly
All_Blanks_hit_freq_monthly <- All_Blanks_hit_freq %>%
mutate(`Month Name`=month(MONTH,label=TRUE, abbr=TRUE)) 

#FIg 10: Seasonal Trends 
Seasonal_trends <-ggplot(filter(All_Blanks_hit_freq_monthly,str_detect(`Blank Type`,"Percent Total Blanks Greater Than MDL"),TEST_NAME %in% Study_analytes,YEAR>=2010),aes(y=VALUE,x=`Month Name`,fill=`Blank Type`))+facet_wrap(~TEST_NAME,ncol=2,scales = "free")+
scale_fill_brewer(palette = "Set2")+theme_bw()+scale_y_continuous(labels = percent)+geom_boxplot()+theme(legend.position = "bottom",axis.text.x = element_text(angle = 90))+
labs(x="Month",y="Frequency of Blanks above MDL (%)",title = "")

ggsave("./N Contamination Data/Seasonal_trends- study analytes.jpeg",plot=Seasonal_trends,height=8,width=11,units="in")

#
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


Blank_Hit_Summary <- function(df) #calculate blank hit frequencies by analyte
{  
df1 <- df %>%  
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
          `Percent Total Blanks Greater Than MDL`=`Total Blanks Greater Than MDL`/`Total Blanks`,
          `Total Blanks Greater Than PQL`=`FCEB > PQL`+`FB > PQL`+`EB > PQL`,
          `Percent Total Blanks Greater Than PQL`=`Total Blanks Greater Than PQL`/`Total Blanks`)
return(df1) 
}
