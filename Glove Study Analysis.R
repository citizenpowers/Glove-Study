

library(readr)
library(readxl)
library(scales)
library(dplyr)
library(ggpmisc)
library(ggplot2)
library(lubridate)
library(tidyr)
library(grid)
library(plotrix)
library(RVAideMemoire)
library(rcompanion)

# Import Data -------------------------------------------------------------

NOX_data <- read_excel("Data/Glove_study_201016_data_prelim.xlsx",sheet="NOx")  #import NOx data

NH4_data <- read_excel("Data/Glove_study_201016_data_prelim.xlsx", sheet = "NH4")  #import NH4 data

Pilot_Study_data <- read_excel("Data/Pilot_Glove_Study_Data.xlsx", sheet = "Sheet1")  #import pilot study data

LIMS_Data_with_qualifiers <- read_excel("Data/LIMS_Data_asof_201104.xlsx") #import data with 

# Tidy Data ---------------------------------------------------------------

Glove_Data_tidy <- NOX_data %>%               
mutate(Comments=`John Comment from COC`)  %>%              #create comments column
bind_rows(NH4_data) %>%                                    #join NH4 data
select(-FLAG) %>%  #flag column unpopulated
mutate(`Treatment`=case_when(Comments=="blank - bottle rinsed"~"Bottle Rinsed",                         #Better labels. 
                                     Comments=="Nitrile bottle rub"~"Nitrile Bottle Rub",
                                     Comments=="Nitrile finger dip -1 sec"~"Nitrile 1 Second Dip",
                                     Comments=="Nitrile finger dip -10 sec"~"Nitrile 10 Second Dip",
                                     Comments=="Nitrile inside out bottle rub"~"Nitrile Inside Out Rub",
                                     Comments=="Rinsed Nitrile bottle rub"~"Nitrile Rinsed Rub",
                                     Comments=="Rinsed Nitrile finger dip -1 sec"~"Nitrile Rinsed 1 Second Dip",
                                     Comments=="Rinsed Nitrile finger dip -10 sec"~"Nitrile Rinsed 10 Second Dip",
                                     Comments=="Vinyl finger dip -1 sec"~"Vinyl 1 Second Dip",
                                     Comments=="Vinyl finger dip -10 sec"~"Vinyl 10 Second Dip",
                                     Comments=="Unrinsed Bottle"~"Unrinsed Bottle",
                                     TRUE~as.character(Comments)))  %>%
mutate(`Treatment` = factor(`Treatment`, levels = c("Unrinsed Bottle", "Bottle Rinsed","Vinyl 1 Second Dip","Vinyl 10 Second Dip","Nitrile Bottle Rub","Nitrile 1 Second Dip","Nitrile 10 Second Dip","Nitrile Inside Out Rub","Nitrile Rinsed Rub","Nitrile Rinsed 1 Second Dip","Nitrile Rinsed 10 Second Dip"))) %>% 
left_join(select(LIMS_Data_with_qualifiers,SAMPLE_ID,FLAG,TEST_NAME),by=c("SAMPLE_ID","TEST_NAME"),keep=FALSE)   #Added populated flag data

# Summary Table -----------------------------------------------------------

Summary_table <- Glove_Data_tidy %>%
group_by(`Treatment`,TEST_NAME) %>%
summarise(n=n(),min=min(VALUE,na.rm = TRUE),max=max(VALUE,na.rm = TRUE),Range=paste(min,"-",max),   mean=mean(VALUE,na.rm = TRUE),median=median(VALUE,na.rm = TRUE),`non-detects`=sum(FLAG=="U",na.rm=TRUE),`detects`=sum(is.na(FLAG)==TRUE)+sum(FLAG!="U",na.rm = TRUE),`% Equal or Greater than MDL`=percent(`detects`/5))

write_csv(Summary_table,path ="./Data/Summary_table.csv")


NOx_table <- Glove_Data_tidy %>%
filter(TEST_NAME=="NOX") %>%
select(Treatment,COLLECT_DATE,SAMPLE_ID,VALUE,UNITS,MDL,FLAG) %>%
mutate(`Treatment` = factor(`Treatment`, levels = c("Unrinsed Bottle", "Bottle Rinsed","Vinyl 1 Second Dip","Vinyl 10 Second Dip","Nitrile Bottle Rub","Nitrile 1 Second Dip","Nitrile 10 Second Dip","Nitrile Inside Out Rub","Nitrile Rinsed Rub","Nitrile Rinsed 1 Second Dip","Nitrile Rinsed 10 Second Dip"))) %>%
arrange(`Treatment`)  

write_csv(NOx_table,path ="./Data/NOx_table.csv")
  
NH4_table <- Glove_Data_tidy %>%
filter(TEST_NAME=="NH4") %>%
select(Treatment,COLLECT_DATE,SAMPLE_ID,VALUE,UNITS,MDL,FLAG) %>%
mutate(`Treatment` = factor(`Treatment`, levels = c("Unrinsed Bottle", "Bottle Rinsed","Vinyl 1 Second Dip","Vinyl 10 Second Dip","Nitrile Bottle Rub","Nitrile 1 Second Dip","Nitrile 10 Second Dip","Nitrile Inside Out Rub","Nitrile Rinsed Rub","Nitrile Rinsed 1 Second Dip","Nitrile Rinsed 10 Second Dip"))) %>%
arrange(`Treatment`)  

write_csv(NH4_table,path ="./Data/NH4_table.csv")

# Figures -----------------------------------------------------------------

#NOX and NH4 figs combined 
ggplot(Glove_Data_tidy ,aes(as.factor(Comments),VALUE,color=TEST_NAME,fill=TEST_NAME))+geom_boxplot(color="black")+geom_jitter(color="black",fill="grey",shape=21,height=0,width = .1)+
theme_bw()+facet_wrap(~TEST_NAME,scales = "free",nrow = 2)+
scale_y_continuous(breaks = pretty_breaks(6))+scale_fill_brewer(palette = "Set1")+
geom_hline(yintercept = c(0.01,0.005),linetype="dashed")+geom_hline(yintercept = 0)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
labs(title="Potential Nitrogen Contamination from Different Gloves Types",y="TPO4  (ug/L)",x="Treatment")  

ggsave("Figures/Potential Nitrogen Contamination from Different Gloves Types.jpeg", plot = last_plot(), width = 8, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#NOX fig
ggplot(filter(Glove_Data_tidy, TEST_NAME=="NOX"),aes(Treatment,VALUE*1000,color=TEST_NAME,fill=TEST_NAME))+geom_boxplot(color="black",fill="grey80")+#geom_jitter(color="black",fill="grey50",shape=21,height=0,width = .15)+
scale_y_continuous(breaks = pretty_breaks(10))+scale_fill_brewer(palette = "Set1")+theme_bw()+
annotate("text", x = "Vinyl 10 Second Dip", y = -12, label = "MDL 5",color="grey40",size=5)+geom_hline(yintercept = 5,linetype="dotted",color="grey30",size=1)+   #Add MDL
geom_segment(aes(x = "Vinyl 10 Second Dip", y = -7.5, xend = "Nitrile Bottle Rub", yend = 3), colour='grey30', size=1,arrow = arrow(length = unit(0.25, "cm")))+  #Add arrow to MDL
annotate("text", x = "Nitrile Inside Out Rub", y = 26, label = "PQL 10",color="grey40",size=5)+geom_hline(yintercept = 10,linetype="longdash",color="grey30",size=1)+   #Add PQL
geom_segment(aes(x = "Nitrile Inside Out Rub", y = 20, xend = "Nitrile 10 Second Dip", yend = 11.5), colour='grey30', size=1,arrow = arrow(length = unit(0.25, "cm")))+  #Add arrow to PQL
geom_hline(yintercept = 0)+
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12),axis.text.y = element_text(size=12),axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14),title =element_text(size = 16) )+
labs(title="Nitrate-Nitrite Contamination from Treatments",y=expression(NOx~mu*g~L^-1),x="Treatment")  

ggsave("Figures/Potential NOx Contamination from Different Gloves Types.jpeg", plot = last_plot(), width = 7.5, height =10.5, units = "in", dpi = 300, limitsize = TRUE)


#Gapped boxplot using plotrix
NOxData <-split(filter(Glove_Data_tidy, TEST_NAME=="NOX")$VALUE*1000,filter(Glove_Data_tidy, TEST_NAME=="NOX")$Treatment )

from <- 25 #gap start
to <-160   #gap end
par(mar=c(10,5,0.5,0.5)) #set margins around plot

gap.boxplot(NOxData ,gap.axis="y",col="grey80",gap=list(top=c(to,from),bottom=c(NA,NA)),axes = FALSE)
axis.break(2, from, breakcol="grey80", style="gap") 
axis.break(2, from*(1+0.02), breakcol="black", style="slash")
axis.break(4, from*(1+0.02), breakcol="black", style="slash")  
ablineclip(h = c(0,5,10,20,50,75,100,125,150),lty = 3, col = "grey")
staxlab(side=1,labels=c("Unrinsed Bottle","Bottle Rinsed","Vinyl 1 Second Dip","Vinyl 10 Second Dip","Nitrile Bottle Rub","Nitrile 1 Second Dip","Nitrile 10 Second Dip","Nitrile Inside Out Rub",
                        "Nitrile Rinsed Rub","Nitrile Rinsed 1 Second Dip","Nitrile Rinsed 10 Second Dip"),
        top.line=0.2,line.spacing=0.1,srt=45,ticklen=0.03,adj=.9,xlab="Treatment")
staxlab(side=2,at=c(0,5,10,20,27.5,50,75,100,125,150),labels=c(0,"MDL 5","PQL 10",20,150,175,200,225,250," "),top.line=0.5,line.spacing=0.8,ticklen=0.03,adj=1,srt=0)



#NH4 fig
ggplot(filter(Glove_Data_tidy, TEST_NAME=="NH4"),aes(as.factor(Comments),VALUE,color=TEST_NAME,fill=TEST_NAME))+geom_boxplot(color="black",fill="grey80")+#geom_jitter(color="black",fill="grey",shape=21,height=0,width = .1)+
scale_y_continuous(breaks = pretty_breaks(6))+scale_fill_brewer(palette = "Set1")+theme_bw()+ 
annotate("text", x = "Nitrile bottle rub", y = .0047, label = "MDL 0.005",color="grey30")+geom_hline(yintercept = 0.005,linetype="dotted",color="grey30",size=1)+   #Add MDL
annotate("text", x = "Nitrile bottle rub", y = .0097, label = "PQL 0.01",color="grey30")+geom_hline(yintercept = 0.01,linetype="longdash",color="grey30",size=1)+   #Add PDL
geom_hline(yintercept = c(0.01,0.005),linetype="dashed")+geom_hline(yintercept = 0)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
labs(title="NH4 Contamination from Treatments",y="TPO4  (ug/L)",x="Treatment")  

ggsave("Figures/Potential NH4 Contamination from Different Gloves Types.jpeg", plot = last_plot(), width = 8, height = 8, units = "in", dpi = 300, limitsize = TRUE)


#Pilot study NH4 fig
ggplot(filter(Pilot_Study_data, TEST_NAME!="No Bottle",TEST_NAME=="NH4"),aes(as.factor(`Glove Type`),VALUE*1000))+geom_col(fill="grey60",color="black")+
scale_y_continuous(breaks = pretty_breaks(6))+theme_bw()+ 
annotate("text", x = "Fischer Vinyl", y = 5.6, label = "MDL 5",color="grey30")+geom_hline(yintercept = 5,linetype="dotted",color="grey30",size=1)+   #Add MDL
annotate("text", x = "Fischer Vinyl", y = 10.6, label = "PQL 10",color="grey30")+geom_hline(yintercept = 10,linetype="longdash",color="grey30",size=1)+   #Add PDL
geom_hline(yintercept = 0)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12),axis.text.y = element_text(size=12),axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),title =element_text(size = 14) )+
labs(title="Ammonia Contamination from Worst Case Scenario Glove Treatment",y=expression(NH3-N~~mu*g~L^-1),x="Glove Type")  

ggsave("Figures/Ammonia Contamination from Worst Case Scenario Glove Treatment.jpeg", plot = last_plot(), width = 8, height = 8, units = "in", dpi = 300, limitsize = TRUE)

#Pilot study NOx fig
ggplot(filter(Pilot_Study_data, TEST_NAME!="No Bottle",TEST_NAME=="NOX"),aes(as.factor(`Glove Type`),VALUE*1000))+geom_col(fill="grey60",color="black")+
scale_y_continuous(breaks = pretty_breaks(10))+theme_bw()+ 
annotate("text", x = "Fischer Vinyl", y = -30, label = "                  MDL 5",color="grey30")+geom_hline(yintercept = 5,linetype="dotted",color="grey30",size=1)+   #Add MDL
geom_segment(aes(x = "Fischer Vinyl", y = -30, xend = "Blank", yend = 2), colour='grey30', size=1,arrow = arrow(length = unit(0.25, "cm")))+  #Add arrow to MDL  
annotate("text", x = "Fischer Vinyl", y = 50, label = "                PQL 10",color="grey30")+geom_hline(yintercept = 10,linetype="longdash",color="grey30",size=1)+   #Add PQL
geom_segment(aes(x = "Fischer Vinyl", y = 50, xend = "Blank", yend = 15), colour='grey30', size=1,arrow = arrow(length = unit(0.25, "cm")))+  #Add arrow to PQL  
geom_hline(yintercept = 0)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12),axis.text.y = element_text(size=12),axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12),title =element_text(size = 14) )+
labs(title="NOx Contamination from Worst Case Scenario Glove Treatment",y=expression(NOx~mu*g~L^-1),x="Glove Type")  

ggsave("Figures/NOx Contamination from Worst Case Scenario Glove Treatment.jpeg", plot = last_plot(), width = 8, height = 8, units = "in", dpi = 300, limitsize = TRUE)



# Fisher Exact test  ---------------------------------------------------------

#From summary Table of detections
#Treatment                      Hits  non_detects
#Bottle Rinsed                  0   5 
#Vinyl 1 Second Dip             0   5
#Vinyl 10 Second Dip            0   5 
#Nitrile 1 Second Dip           5   0
#Nitrile 10 Second Dip          5   0
#Nitrile Rinsed 1 Second Dip    0   5
#Nitrile Rinsed 10 Second Dip   1   4
#Nitrile Bottle Rub             5   0
#Nitrile Inside Out Rub         0   5
#Nitrile Rinsed Rub             0   5
#Unrinsed Bottle                0   5 


#NOX glove data   - Use abreviations for treatment names. Also zeros will not work in  names?
NOx_Glove_Input =("Treatment      Hits  non_detects
                  BR              0     5
                  V1D             0     5
                  V1OD            0     5
                  N1D             5     0
                  N1OD            5     0
                  NR1D            0     5
                  NR1OD           1     4
                  NBR             5     0
                  NBIOR           0     5 
                  NRR             0     5 
                  UB              0     5 ")

?p.adjust.methods

#Create matrix from NOx DF
NOX__glove_matrix = as.matrix(read.table(textConnection(NOx_Glove_Input), header=TRUE,row.names=1))

#run fisher exact test
fisher.test(NOX__glove_matrix,alternative="two.sided")

#Pairwise comaprison
FE_glove_test <-pairwiseNominalIndependence(NOX__glove_matrix,fisher = TRUE,gtest  = FALSE,chisq  = FALSE, digits = 3)

#display significance with letters
cldList(comparison = FE_glove_test$Comparison,p.value    = FE_glove_test$p.adj.Fisher,threshold  = 0.05)

#Fisher Exact Test for NH3
NH3_Glove_Input =("Treatment      Hits  non_detects
                  BR              0     5
                  V1D             0     5
                  V1OD            0     5
                  N1D             0     5
                  N1OD            0     5
                  NR1D            2     3
                  NR1OD           0     5
                  NBR             0     5
                  NBIOR           0     5 
                  NRR             0     5 
                  UB              0     5 ")

NH3__glove_matrix = as.matrix(read.table(textConnection(NH3_Glove_Input), header=TRUE,row.names=1))

fisher.test(NH3__glove_matrix,alternative="two.sided")

FE_glove_test_NH3 <-pairwiseNominalIndependence(NH3__glove_matrix,fisher = TRUE,gtest  = FALSE,chisq  = FALSE, digits = 3)

cldList(comparison = FE_glove_test_NH3$Comparison,
        p.value    = FE_glove_test_NH3$p.adj.Fisher,
        threshold  = 0.05)

