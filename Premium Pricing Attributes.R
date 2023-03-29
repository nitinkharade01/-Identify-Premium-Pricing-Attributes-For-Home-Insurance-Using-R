#importing the require Libraries

library(dplyr)
library(ggplot2)
library(naniar)
library(patchwork)
library(eeptools)

#ignore the unnecessary warning 
options(warn=-1)

#Lets create data_frame by reading insurance.csv
insurance=read.csv(file.choose(),header=TRUE,stringsAsFactors =TRUE)

#Viewing the Top 10 Rows.
Top_10_rows=head(insurance,10)
View(Top_10_rows)

#View last 10 Rows
Bottom_10_rows=tail(insurance,10)
View(Bottom_10_rows)

#lets check Total number of Rows and Column
dim(insurance)

#Checking all the Column Names
colnames(insurance)

#checking the Structure of all the column
str(insurance)

#while Check the Structure of all column we found all there are 4 dates Column  are in factor datatype  .
#Lets change that in date (while changing that in Date we have to also specify the format .)
insurance$QUOTE_DATE=as.Date(insurance$QUOTE_DATE,"%m/%d/%Y")
insurance$COVER_START=as.Date(insurance$COVER_START,"%d/%m/%Y")
insurance$P1_DOB=as.Date(insurance$P1_DOB,"%d/%m/%Y")
insurance$MTA_DATE=as.Date(insurance$MTA_DATE,"%d/%m/%Y")

#Lets Re-check the datatype for given date column by observing structure of the dataset.
insurance %>% select(QUOTE_DATE,COVER_START,P1_DOB,MTA_DATE) %>% str()

#Now before begining the next phase we found column i.
#lets view the i colum
col_i=insurance %>% select(i)
View(col_i)

#When we observed i, we found out it has not has any significant importance in our analysis.
#it is same as given index, so lets exclude i from the dataset.

insurance=insurance %>% select(-i)

#We create a user-def function to check column is excluded or not
colexluded=function(value){
for (val in seq(1:ncol(insurance)))
{
 if(colnames(insurance[val])==value)
 {cat("Column" ,value," not excluded")
   break }
  else
 {cat("Column",value,"excluded")
  break }
}}

#lets check column i is excluded or not
colexluded("i")

#Column i excluded

#Lets check for Null Value.
#While checking the top 10 and last 10 in our datset we found that there large number of column have  empty value .
#Lets convert all the empty value into NA, and check total N/A in our dataset .

insurance=na_if(insurance,"")

#Lets check for Total NA value and Total NA percentage using miss_var_summary() from naniar Package.
Missing_value=miss_var_summary(insurance)
View(Missing_value)

#From the chart we found that the column we have highest null value is CAMPAIGN_DESC followed by P1_PT_EMP_STATUS
#The Column doesn't have any null value is i, and Police .

#we found that that column which have highest value is CAMPAIGN_DESC .
#The Missing Value is 100%, it is safer to exclude that column from our datset.
insurance=insurance %>% select(-CAMPAIGN_DESC)

#Lets check the column name CAMPAIGN_DESC is Excluded or not
colexluded("CAMPAIGN_DESC")

#Column CAMPAIGN_DESC excluded

#while checking the null values summary , there are multiple feature including cover start have same number of missing values.
#lets check missing value in cover start. . 
insurance %>% filter(is.na(COVER_START)) %>% View()

#We found that N/A present is COVER_START is relate to multiple Column.
#COVER_START is basically the cover payment started dated.
#All the N/A in Cover_start is basically cover payment not started.
#we have almost no values for other columns that relate to cover payment not started.
#As there is no information with respect to N/a in cover start lets include all the N/A i cover start.

insurance=insurance %>% filter(COVER_START!=is.na(COVER_START))

#lets check the null value summary
Missing_value=miss_var_summary(insurance)
View(Missing_value)

#Now N/A Value left for this following Column are-
#P1_PT_EMP_STATUS
#CLERICAL
#MTA_DATE
#MTA_FAP
#MTA_APRP
#QUOTE_DATE
#PAYMENT_FREQUENCY
#RISK_RATED_AREA_B
#RISK_RATED_AREA_C


#First Check with Payment Frequency:
#Lets check unique value in the column
unique(insurance$PAYMENT_FREQUENCY)

#The unique value is we found in NA and 1
#Basically Payment frequent is how frequently client made a payment.
#1 is client is frequent while NA is not frequent.
#Lets Convert NA --> 0 which indicate Non Frequent.

insurance=insurance %>% mutate(PAYMENT_FREQUENCY=ifelse(is.na(PAYMENT_FREQUENCY),0,1))

#Lets Recheck the Payment Frequency Column .
unique(insurance$PAYMENT_FREQUENCY)

#Missing Value % of P1_PT_EMP_STATUS and CLERICAL is more then 95%.
#Both the Column almost having no values.
#Lets exclude both the Column.

insurance=insurance %>% select(-c(P1_PT_EMP_STATUS,CLERICAL))

#lets Check weather both the Column Excluded or not.
colexluded("P1_PT_EMP_STATUS")
colexluded("CLERICAL")


#Lets Check N/A for RISK_RATED_AREA_B and RISK_RATED_AREA_C
#Percentage of NA in Both the Column is less then 30% .
#Lets impute both column using statistical measure .
#As they both are numerical lets check any outlier present in the column or not.

insurance %>% select(RISK_RATED_AREA_B,RISK_RATED_AREA_C) %>% 
boxplot(main = "Outlier Detection",col=c("red","Blue"))

#From the the given chart we found outliers exist in both the columns.
#Lets Replace the NA with Median.

insurance=insurance %>% 
 mutate(RISK_RATED_AREA_B=ifelse(is.na(RISK_RATED_AREA_B),median(RISK_RATED_AREA_B,na.rm =T),RISK_RATED_AREA_B),
        RISK_RATED_AREA_C=ifelse(is.na(RISK_RATED_AREA_C),median(RISK_RATED_AREA_C,na.rm =T),RISK_RATED_AREA_C))

#Lets Check for MTA_DATE ,MTA_FAP and MTA_APRP
#mid-term adjustment (MTA), also called a mid-term modification or mid-term change, 
#MTA refers to a change to an insurance policy prior to the end of the policy period (when coverage is offered).
#lets Compare all the Column with MTA_FLAG.
#MTA_FLAG has two values Y and N .
#Y is  mid-term adjustment present in insurance policy.
#N is  mid-term adjustment is not present in insurance policy.
#MTA_FAP is denoted to increase in Premium due to mid-term adjustment.
#MTA_APRP is denoted to decrease in Premium due to mid-term adjustment.
#MTA_DATE is date where mid-term adjustment is Made.
#We already indicate mid-term adjustment for MTA_FLAG .
#As we already have column that that indicate mid-term adjustment so MTA_Date not require.
#Lets exclude that Column.
insurance=insurance %>% select(-MTA_DATE)

#Lets check column is excluded or not.
colexluded("MTA_DATE")

#Now lets check the missing value percentage for MTA_FAP and MTA_APRP with MTA_FLAG is Y/N
MTA_Y=insurance %>% select(MTA_FLAG,MTA_FAP,MTA_APRP) %>% filter(MTA_FLAG=="Y")
MTA_N=insurance %>% select(MTA_FLAG,MTA_FAP,MTA_APRP) %>% filter(MTA_FLAG=="N")

#lets check the missing value percentage for each column.
miss_var_summary(MTA_Y)
miss_var_summary(MTA_N)

#we found that missing value is only present in row where MTA_FLAG is N which represent mid-term adjustment is No.
#lets convert all the NA value in MTA_FAP and MTA_APRP to 0, which indicate no change in value.
insurance=insurance %>% 
          mutate(MTA_APRP=ifelse(is.na(MTA_APRP),0,MTA_APRP),
                 MTA_FAP=ifelse(is.na(MTA_FAP),0,MTA_FAP))

#last column remain is QUOTE_DATE.
#QUOTE_DATE is basically tell insurance in Quoted or not.
#To understand the following column we have compare Quote Date with Pol_status and Last_ANNU_Premium_Gross

#Lets Check police Status is present in Quote_date for Both NA and Non-NA Values for every policy.
Pol_sta_QUOTE_DATE=insurance %>% select(Police,QUOTE_DATE,POL_STATUS) %>% filter(QUOTE_DATE!=is.na(QUOTE_DATE))
View(Pol_sta_QUOTE_DATE)

Pol_sta_QUOTE_DATE_NA=insurance %>% select(Police,QUOTE_DATE,POL_STATUS) %>% filter(is.na(QUOTE_DATE))
View(Pol_sta_QUOTE_DATE_NA)

#Policy status present in Quote_Date for both NA and Non-NA Values.

#Similary we check if premium amount is present in Quote_date for Both NA and Non-NA Values for every policy.
Pre_amt_QUOTE_DATE=insurance %>% select(Police,QUOTE_DATE,LAST_ANN_PREM_GROSS) %>% filter(QUOTE_DATE!=is.na(QUOTE_DATE))
View(Pre_amt_QUOTE_DATE)

Pre_amt_QUOTE_DATE_NA=insurance %>% select(Police,QUOTE_DATE,LAST_ANN_PREM_GROSS) %>% filter(is.na(QUOTE_DATE))
View(Pre_amt_QUOTE_DATE_NA)

# premium amount is present in Quote_Date for both NA and Non-NA Values.
#Hence we concluded that insurance is quoted for every policy number as policy status and premium amount is mentioned.

#Lets change the Quote_Date column into Policy_Quoted consist of Value Y which represent Policy is Quoted.
insurance=insurance %>% rename(Policy_Quoted=QUOTE_DATE) %>%
          mutate(Policy_Quoted=ifelse(is.na(Policy_Quoted),"Y","Y"))

#now again lets check Final NA value summary in chart .
gg_miss_var(insurance)

#As NA Value is Cleared , now we have select feature/columns that require for further analysis.
#We have to exclude all the columns that is not require in our analysis.
#Our main goal of analysis is to find out the attribute impacting home insurance and we select feature according to it.

#We have to find characteristics of customers who are likelier to default on their payments.

Person=insurance %>% select(P1_SEX,P1_EMP_STATUS,P1_DOB,P1_MAR_STATUS,P1_POLICY_REFUSED,LAST_ANN_PREM_GROSS,PAYMENT_FREQUENCY) %>%
       rename(Premium=LAST_ANN_PREM_GROSS)


View(Person)
#We have to find characteristics of a house that can drive up the price of premium . 

Property=insurance %>% select(PROP_TYPE,OWNERSHIP_TYPE,YEARBUILT,BEDROOMS,ROOF_CONSTRUCTION,WALL_CONSTRUCTION,
                              FLOODING,SUBSIDENCE,LISTED,MAX_DAYS_UNOCC,NEIGH_WATCH,APPR_ALARM,APPR_LOCKS,
                              SAFE_INSTALLED,BUILDINGS_COVER,LAST_ANN_PREM_GROSS) %>% 
                              rename(Premium=LAST_ANN_PREM_GROSS)
                  

View(Property)


#lets explore the person dataset, and find out which gender are most like to pay frequently pay their premium .

Gender_payment_frequent=Person %>% select(P1_SEX,PAYMENT_FREQUENCY) %>% filter(PAYMENT_FREQUENCY==1) %>% group_by(P1_SEX) %>%
                        summarise(maximum_Payment_Frequency=n()) %>% 
                        mutate(Gender_payment_frequeny=round((maximum_Payment_Frequency/sum(maximum_Payment_Frequency)*100),2)) %>%
                        select(P1_SEX,Gender_payment_frequeny) %>%
                        ggplot(aes(x=reorder(P1_SEX,-Gender_payment_frequeny),y=Gender_payment_frequeny,fill=P1_SEX))+
                        geom_bar(stat ="identity",width =0.5,color="Black")+
                        geom_text(aes(label=paste(Gender_payment_frequeny,"%"),vjust=-0.25)) +
                        ggtitle("maximum number of Frequent Payment (%) by Gender")+
                        theme(plot.title=element_text(hjust = 0.5),
                        axis.text.y =element_blank(),
                        axis.title.y =element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.text.x=element_text(face="bold"),
                        legend.position ="none")+
                        xlab("Gender")


Gender_payment_Not_frequent=Person %>% select(P1_SEX,PAYMENT_FREQUENCY) %>% filter(PAYMENT_FREQUENCY==0) %>% group_by(P1_SEX) %>%
                            summarise(maximum_Not_Payment_Frequency=n()) %>% 
                            mutate(Gender_NO_payment_frequeny=round((maximum_Not_Payment_Frequency/sum(maximum_Not_Payment_Frequency)*100),2)) %>%
                            select(P1_SEX,Gender_NO_payment_frequeny) %>%
                            ggplot(aes(x=reorder(P1_SEX,-Gender_NO_payment_frequeny),y=Gender_NO_payment_frequeny,fill=P1_SEX))+
                            geom_bar(stat ="identity",width =0.5,color="Black")+
                            geom_text(aes(label=paste(Gender_NO_payment_frequeny,"%"),vjust=-0.25)) +
                            ggtitle("maximum number of Not Frequent Payment (%) by Gender")+
                            theme(plot.title=element_text(hjust = 0.5),
                            axis.text.y =element_blank(),
                            axis.title.y =element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.text.x=element_text(face="bold"),
                            legend.position ="none")+
                            xlab("Gender")


Gender=Gender_payment_frequent+Gender_payment_Not_frequent
Gender


#From the Given Chart we find out maximum number of Non_frequency payment in Male compare to Female and N(Not Specified).

#lets find out which P1_EMP_STATUS are most like to pay frequently pay their premium .

Emps_payment_frequent=Person %>% select(P1_EMP_STATUS,PAYMENT_FREQUENCY) %>% filter(PAYMENT_FREQUENCY==1) %>% group_by(P1_EMP_STATUS) %>%
                        summarise(maximum_Payment_Frequency=n()) %>% 
                        mutate(Emps_payment_frequeny=round((maximum_Payment_Frequency/sum(maximum_Payment_Frequency)*100),2)) %>%
                        select(P1_EMP_STATUS,Emps_payment_frequeny) %>%
                        ggplot(aes(x=reorder(P1_EMP_STATUS,-Emps_payment_frequeny),y=Emps_payment_frequeny,fill=P1_EMP_STATUS))+
                        geom_bar(stat ="identity",width =0.5)+
                        geom_text(aes(label=paste(Emps_payment_frequeny,"%"),vjust=-0.25)) +
                        ggtitle("Employee Status with maximum number of Frequent Payment (%) ")+
                        theme(plot.title=element_text(hjust = 0.5),
                        axis.text.y =element_blank(),
                        axis.title.y =element_blank(),
                        axis.ticks.y = element_blank(),
                        axis.text.x=element_text(face="bold"),
                        legend.position ="none")+
                        xlab("Employee Status")


Emps_payment_Not_frequent=Person %>% select(P1_EMP_STATUS,PAYMENT_FREQUENCY) %>% filter(PAYMENT_FREQUENCY==0) %>% group_by(P1_EMP_STATUS) %>%
                            summarise(maximum_Not_Payment_Frequency=n()) %>% 
                            mutate(Emps_NO_payment_frequeny=round((maximum_Not_Payment_Frequency/sum(maximum_Not_Payment_Frequency)*100),2)) %>%
                            select(P1_EMP_STATUS,Emps_NO_payment_frequeny) %>%
                            ggplot(aes(x=reorder(P1_EMP_STATUS,-Emps_NO_payment_frequeny),y=Emps_NO_payment_frequeny,fill=P1_EMP_STATUS))+
                            geom_bar(stat ="identity",width =0.5)+
                            geom_text(aes(label=paste(Emps_NO_payment_frequeny,"%"),vjust=-0.25)) +
                            ggtitle("Employee Status with maximum number of Non Frequent Payment (%)")+
                            theme(plot.title=element_text(hjust = 0.5),
                            axis.text.y =element_blank(),
                            axis.title.y =element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.text.x=element_text(face="bold"),
                            legend.position ="none")+
                            xlab("Employee Status")


Emps=Emps_payment_frequent/Emps_payment_Not_frequent
Emps


#From the Given Chart we find out maximum number of Frequent payment in R(Retired) followed by E(Employee) and others.

#Lets find payment frequency in married/unmarried person.

MarriedS_payment_frequent=Person %>% select(P1_MAR_STATUS,PAYMENT_FREQUENCY) %>% filter(PAYMENT_FREQUENCY==1) %>% group_by(P1_MAR_STATUS) %>%
                          summarise(maximum_Payment_Frequency=n()) %>% 
                          mutate(MARS_payment_frequeny=round((maximum_Payment_Frequency/sum(maximum_Payment_Frequency)*100),2)) %>%
                          select(P1_MAR_STATUS,MARS_payment_frequeny) %>%
                          ggplot(aes(x=reorder(P1_MAR_STATUS,-MARS_payment_frequeny),y=MARS_payment_frequeny,fill=P1_MAR_STATUS))+
                          geom_bar(stat ="identity",width =0.5)+
                          geom_text(aes(label=paste(MARS_payment_frequeny,"%"),vjust=-0.25)) +
                          ggtitle("Marital Status with maximum number of Frequent Payment (%) ")+
                          theme(plot.title=element_text(hjust = 0.5),
                                axis.text.y =element_blank(),
                                axis.title.y =element_blank(),
                                axis.ticks.y = element_blank(),
                                axis.text.x=element_text(face="bold"),
                          legend.position ="none")+
                          xlab("Marital Status")


MarriedS_payment_Not_frequent=Person %>% select(P1_MAR_STATUS,PAYMENT_FREQUENCY) %>% filter(PAYMENT_FREQUENCY==0) %>% group_by(P1_MAR_STATUS) %>%
                              summarise(maximum_Not_Payment_Frequency=n()) %>% 
                              mutate(MARS_NO_payment_frequeny=round((maximum_Not_Payment_Frequency/sum(maximum_Not_Payment_Frequency)*100),2)) %>%
                              select(P1_MAR_STATUS,MARS_NO_payment_frequeny) %>%
                              ggplot(aes(x=reorder(P1_MAR_STATUS,-MARS_NO_payment_frequeny),y=MARS_NO_payment_frequeny,fill=P1_MAR_STATUS))+
                              geom_bar(stat ="identity",width =0.5)+
                              geom_text(aes(label=paste(MARS_NO_payment_frequeny,"%"),vjust=-0.25)) +
                              ggtitle("Marital Status with maximum number of Non Frequent Payment (%)")+
                              theme(plot.title=element_text(hjust = 0.5),
                              axis.text.y =element_blank(),
                              axis.title.y =element_blank(),
                              axis.ticks.y = element_blank(),
                              axis.text.x=element_text(face="bold"),
                              legend.position ="none")+
                              xlab("Marital Status")


MAR_S=MarriedS_payment_frequent/MarriedS_payment_Not_frequent
MAR_S

#Lets Find out Payment Frequency in Policy Refusal

POlR_Y_payment_frequent=Person %>% select(P1_POLICY_REFUSED,PAYMENT_FREQUENCY) %>% filter(P1_POLICY_REFUSED=="Y") %>%
                       group_by(PAYMENT_FREQUENCY) %>% 
                       summarise(PAYMENT_FREQUENCY_count=n()) %>%
                       mutate(PAYMENT_FREQUENCY_Perc=round(PAYMENT_FREQUENCY_count/sum(PAYMENT_FREQUENCY_count)*100,2),
                              PAYMENT_FREQUENCY=ifelse(PAYMENT_FREQUENCY==0,"Payment Not Frequent","Payment Frquent")) %>%
                       ggplot(aes(x=reorder(PAYMENT_FREQUENCY,-PAYMENT_FREQUENCY_Perc),y=PAYMENT_FREQUENCY_Perc,fill=PAYMENT_FREQUENCY))+
                       geom_bar(stat ="identity",width =0.5)+
                       geom_text(aes(label=paste(PAYMENT_FREQUENCY_Perc,"%"),vjust=-0.25)) +
                       ggtitle("Payment Status For the Client Who Refuse the Policy")+
                       theme(plot.title=element_text(hjust = 0.5),
                       axis.text.y =element_blank(),
                       axis.title.y =element_blank(),
                       axis.ticks.y = element_blank(),
                       axis.text.x=element_text(face="bold"),
                       legend.position ="none")+
                       xlab("Payment Frequency Status")

                      
POlR_N_payment_frequent=Person %>% select(P1_POLICY_REFUSED,PAYMENT_FREQUENCY) %>% filter(P1_POLICY_REFUSED=="N") %>%
                        group_by(PAYMENT_FREQUENCY) %>% 
                        summarise(PAYMENT_FREQUENCY_count=n()) %>%
                        mutate(PAYMENT_FREQUENCY_Perc=round(PAYMENT_FREQUENCY_count/sum(PAYMENT_FREQUENCY_count)*100,2),
                               PAYMENT_FREQUENCY=ifelse(PAYMENT_FREQUENCY==0,"Payment Not Frequent","Payment Frquent")) %>%
                        ggplot(aes(x=reorder(PAYMENT_FREQUENCY,-PAYMENT_FREQUENCY_Perc),y=PAYMENT_FREQUENCY_Perc,fill=PAYMENT_FREQUENCY))+
                        geom_bar(stat ="identity",width =0.5)+
                        geom_text(aes(label=paste(PAYMENT_FREQUENCY_Perc,"%"),vjust=-0.25)) +
                        ggtitle("Payment Status For the Client Who Not Refuse the Policy")+
                        theme(plot.title=element_text(hjust = 0.5),
                              axis.text.y =element_blank(),
                              axis.title.y =element_blank(),
                              axis.ticks.y = element_blank(),
                              axis.text.x=element_text(face="bold"),
                              legend.position ="none")+
                        xlab("Payment Frequency Status")


Pol_Ref=POlR_Y_payment_frequent+POlR_N_payment_frequent
Pol_Ref

#We found the more non frequent payment in married client and frequent in P (Domestic Partner).

#Now lets check for DOB column ,
#We have first covert DOB to age and check the payment frequency with respect to age .

AGE_Payment_Frequency_Y=Person %>% filter(PAYMENT_FREQUENCY==1) %>%
                        select(P1_DOB) %>% 
                        mutate(Age=round(age_calc(P1_DOB,Sys.Date(),units="years"),0)) %>%
                        select(Age) %>%
                        ggplot(aes(x=Age))+
                        geom_density(color="darkgreen",fill="lightgreen")+
                        ggtitle("Age group Probability of Frequent Payment ")+
                        theme(plot.title=element_text(hjust = 0.5),
                              axis.title.y =element_blank())

AGE_Payment_Frequency_N=Person %>% filter(PAYMENT_FREQUENCY==0) %>%
                        select(P1_DOB) %>% 
                        mutate(Age=round(age_calc(P1_DOB,Sys.Date(),units="years"),0)) %>%
                        select(Age) %>%
                        ggplot(aes(x=Age))+
                        geom_density(color="darkblue",fill="lightblue")+
                        ggtitle("Age group Probability of Not Frequent Payment.")+
                        theme(plot.title=element_text(hjust = 0.5),
                              axis.title.y =element_blank())

Age_Payment_freq=AGE_Payment_Frequency_Y+AGE_Payment_Frequency_N
Age_Payment_freq

#Now lets check for payment frequency with premium amou

Premium_Payment_Frequency_Y=Person %>% filter(PAYMENT_FREQUENCY==1) %>%
                            select(Premium) %>% 
                            ggplot(aes(x=Premium))+
                            scale_x_continuous(limits=c(0,1000))+
                            geom_density(color="darkgreen",fill="azure")+
                            ggtitle("Probability of Frequent Payment for Premium Amount")+
                            theme(plot.title=element_text(hjust = 0.5),
                              axis.title.y =element_blank())


Premium_Payment_Frequency_N=Person %>% filter(PAYMENT_FREQUENCY==0) %>%
                            select(Premium) %>% 
                            ggplot(aes(x=Premium))+
                            scale_x_continuous(limits=c(0,1000))+
                            geom_density(color="darkgreen",fill="lightgreen")+
                            ggtitle("Probability of Not Frequent Payment for Premium Amount")+
                            theme(plot.title=element_text(hjust = 0.5),
                                  axis.title.y =element_blank())

Premium_Payment_Freq=Premium_Payment_Frequency_Y+Premium_Payment_Frequency_N
Premium_Payment_Freq

#we found that Client who having Premium Amount from 0-250 are probably going pay premium frequently.

#lets  find characteristics of a house that can drive up the price of premium . 

#Lets view the property type and find out which property type have to pay more premium.

Property_Type_Premium=Property %>% select(PROP_TYPE,Premium)%>%
                      group_by(PROP_TYPE) %>%
                      summarise(Average_Premium=round(mean(Premium),2)) %>% 
                      ggplot(aes(y=reorder(PROP_TYPE,Average_Premium),x=Average_Premium,fill=Average_Premium))+
                      geom_bar(stat ="identity",width =0.5,color="Black")+
                      geom_text(aes(label=paste("$",Average_Premium),hjust=-0.25)) +
                      ggtitle("Propety Types with Higest Premium")+
                      theme(plot.title=element_text(hjust = 0.5),
                            axis.text.x=element_blank(),
                            axis.title.x=element_blank(),
                            axis.ticks.x=element_blank(),
                            axis.text.y=element_text(face="bold"),
                            legend.position ="none")+
                      ylab("Propety Types")+
                      scale_fill_distiller(palette="YlGn",direction=1)

Property_Type_Premium                   

#we found that client who having property type 37 are mostly like to pay more premium .


#Lets view the Ownership type and find out which Ownership type have to pay more premium.
Ownership_type=Property %>% select(OWNERSHIP_TYPE,Premium) %>%
               group_by(OWNERSHIP_TYPE) %>%
               summarise(Average_Premium=round(mean(Premium),2)) %>% 
               ggplot(aes(y=reorder(OWNERSHIP_TYPE,Average_Premium),x=Average_Premium,fill=Average_Premium))+
               geom_bar(stat ="identity",width =0.5,color="Black")+
               geom_text(aes(label=paste("$",Average_Premium),hjust=-0.25)) +
               ggtitle("Ownership Types with Higest Premium")+
               theme(plot.title=element_text(hjust = 0.5),
               axis.text.x =element_blank(),
               axis.title.x =element_blank(),
               axis.ticks.x = element_blank(),
               axis.text.y=element_text(face="bold"),
               legend.position ="none")+
               ylab("Ownership Types")+
               scale_fill_distiller(palette="YlGnBu",direction=1)
Ownership_type

l#we found that client who having Ownership type 3 are mostly like to pay more premium.

#Lets Find out how year built have impact on premium 

Year_bulit_Premium=Property %>% select(YEARBUILT,Premium) %>% group_by(YEARBUILT) %>%
                   summarise(Average_Premium=round(mean(Premium),2))%>%
                   ggplot(aes(x=YEARBUILT,y=Average_Premium))+
                   scale_x_continuous(limits=c(1749,2000))+
                   scale_y_continuous(limits=c(0,300))+
                   ggtitle("Year Built with Higest Premium")+
                   theme(plot.title=element_text(hjust = 0.5),
                         axis.text=element_text(face="bold"))+
                   geom_line(color="Blue")
Year_bulit_Premium


#The property that just built before 1750 have highest premium , while we see drop around 1750 and spike in between 1800-1850.


#we found out how number of bedroom in property impact on premium.

Bedroom_Premium=Property %>% select(BEDROOMS,Premium) %>% group_by(BEDROOMS) %>%
                summarise(Average_Premium=round(mean(Premium),2))%>%
                ggplot(aes(x=reorder(BEDROOMS,-Average_Premium),y=Average_Premium))+
                geom_bar(stat ="identity",width =0.5,fill="Orange",color="Black")+
                geom_text(aes(label=paste("$",Average_Premium),vjust=-0.25)) +
                ggtitle("No of Bedrooms with Higest Premium")+
                theme(plot.title=element_text(hjust = 0.5),
                      axis.text.y =element_blank(),
                      axis.title.y =element_blank(),
                      axis.ticks.y = element_blank(),
                      axis.text.x=element_text(face="bold"),
                      legend.position ="none")+
                xlab("No of Bedrooms")
                
Bedroom_Premium

#Property which having 7 bedroom have  highest Premium.

#Lets find out how Roof_construction impact on premium.
ROOF_CONSTRUCTION_Premium=Property %>% select(ROOF_CONSTRUCTION,Premium) %>%
                          group_by(ROOF_CONSTRUCTION) %>%
                          summarise(Average_Premium=round(mean(Premium),2))%>%
                          ggplot(aes(y=reorder(ROOF_CONSTRUCTION,Average_Premium),x=Average_Premium))+
                          geom_bar(stat ="identity",width =0.5,fill="lightgreen",color="Black")+
                          geom_text(aes(label=paste("$",Average_Premium),hjust=-0.25)) +
                          ggtitle("Roof Construction with Higest Premium")+
                          theme(plot.title=element_text(hjust = 0.5),
                                axis.text.x =element_blank(),
                                axis.title.x =element_blank(),
                                axis.ticks.x = element_blank(),
                                axis.text.y=element_text(face="bold"),
                          legend.position ="none")+
                          ylab("Roof Construction")

ROOF_CONSTRUCTION_Premium
#The Property which have Roof Construction 12 are going to paid more premium .

#Lets find out how Wall_construction impact on premium.
WALL_CONSTRUCTION_Premium=Property %>% select(WALL_CONSTRUCTION,Premium) %>%
                          group_by(WALL_CONSTRUCTION) %>%
                          summarise(Average_Premium=round(mean(Premium),2))%>%
                          ggplot(aes(y=reorder(WALL_CONSTRUCTION,Average_Premium),x=Average_Premium))+
                          geom_bar(stat ="identity",width =0.5,fill="lightblue",color="Black")+
                          geom_text(aes(label=paste("$",Average_Premium),hjust=-0.25)) +
                          ggtitle("Wall Construction with Higest Premium")+
                          theme(plot.title=element_text(hjust = 0.5),
                                axis.text.x =element_blank(),
                                axis.title.x =element_blank(),
                                axis.ticks.x = element_blank(),
                                axis.text.y=element_text(face="bold"),
                          legend.position ="none")+
                          ylab("WALL CONSTRUCTION")

WALL_CONSTRUCTION_Premium

#The Client which have WALL CONSTRUCTION 99 are going to paid more premium .

#Lets Check how flooding impact on Premium.

Flood_premium=Property %>% select(FLOODING,Premium) %>% group_by(FLOODING) %>%
              summarise(Average_Premium=round(mean(Premium),2))%>%
              ggplot(aes(x=reorder(FLOODING,-Average_Premium),y=Average_Premium))+
              geom_bar(stat ="identity",width =0.5,fill="aquamarine",color="Black")+
              geom_text(aes(label=paste("$",Average_Premium),vjust=-0.25)) +
              ggtitle("Property with Flood Zone Higest Premium")+
              theme(plot.title=element_text(hjust = 0.5),
                    axis.text.y =element_blank(),
                    axis.title.y =element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.text.x=element_text(face="bold"),
              legend.position ="none")+
              xlab("Flood Zone")
Flood_premium

#The Property which have Flood Zone N are  likely paid more premium , but difference is minimum.

#Lets Check how SUBSIDENCE impact on Premium.

SUBSIDENCE_premium=Property %>% select(SUBSIDENCE,Premium) %>% group_by(SUBSIDENCE) %>%
                   summarise(Average_Premium=round(mean(Premium),2))%>%
                   ggplot(aes(x=reorder(SUBSIDENCE,-Average_Premium),y=Average_Premium))+
                   geom_bar(stat ="identity",width =0.5,fill="66C2A5",color="Black")+
                   geom_text(aes(label=paste("$",Average_Premium),vjust=-0.25)) +
                   ggtitle("SUBSIDENCE with Higest Premium")+
                   theme(plot.title=element_text(hjust = 0.5),
                         axis.text.y =element_blank(),
                         axis.title.y =element_blank(),
                         axis.ticks.y = element_blank(),
                         axis.text.x=element_text(face="bold"),
                         legend.position ="none")+
                    xlab("SUBSIDENCE")
SUBSIDENCE_premium

#The Property which have SUBSIDENCE N are  likely paid more premium , but again difference is minimum.

#Lets check the property listed impact on premium.
LISTED_premium=Property %>% select(LISTED,Premium) %>%
               mutate(LISTED=cut(LISTED,breaks=c(1,4,6),labels=c("1-3","4-5"),right=F)) %>%
               group_by(LISTED) %>%
               summarise(Average_Premium=round(mean(Premium),2))%>%
               ggplot(aes(x=reorder(LISTED,-Average_Premium),y=Average_Premium))+
               geom_bar(stat ="identity",width =0.5,fill="bisque",color="Black")+
               geom_text(aes(label=paste("$",Average_Premium),vjust=-0.25)) +
               ggtitle("Property LISTED with Higest Premium")+
               theme(plot.title=element_text(hjust = 0.5),
                      axis.text.y =element_blank(),
                      axis.title.y =element_blank(),
                      axis.ticks.y = element_blank(),
                      axis.text.x=element_text(face="bold"),
               legend.position ="none")+
               xlab("Property LISTED")
LISTED_premium

#The Property which listed 4-5 times have the highest Premium .

#Lets check the property which unoccupied maximum days have any impact on premium.
MAX_DAYS_UNOCC_premium=Property %>% select(MAX_DAYS_UNOCC,Premium) %>%
                       mutate(MAX_DAYS_UNOCC=cut(MAX_DAYS_UNOCC,breaks=c(0,100,185),labels=c("0-90","90-181"),right=F)) %>%
                       group_by(MAX_DAYS_UNOCC) %>%
                       summarise(Average_Premium=round(mean(Premium),2))%>%
                       ggplot(aes(x=reorder(MAX_DAYS_UNOCC,-Average_Premium),y=Average_Premium))+
                       geom_bar(stat ="identity",width =0.5,fill="#8040c0",color="Black")+
                       geom_text(aes(label=paste("$",Average_Premium),vjust=-0.25)) +
                       ggtitle("Unoccupied Property with Higest Premium")+
                       theme(plot.title=element_text(hjust = 0.5),
                             axis.text.y =element_blank(),
                             axis.title.y =element_blank(),
                             axis.ticks.y = element_blank(),
                             axis.text.x=element_text(face="bold"),
                             legend.position ="none")+
                       xlab("Property Unoccupied")
MAX_DAYS_UNOCC_premium

#The Property is which UNOCCUPIED for more then 90 Days have Highest Premium.

#Lets check the premium of the property which having NEIGH_WATCH(Neighborhood Watch).

NEIGH_WATCH_premium=Property %>% select(NEIGH_WATCH,Premium) %>% group_by(NEIGH_WATCH) %>%
                    summarise(Average_Premium=round(mean(Premium),2))%>%
                    ggplot(aes(x=reorder(NEIGH_WATCH,-Average_Premium),y=Average_Premium))+
                    geom_bar(stat ="identity",width =0.5,fill="#004040",color="Black")+
                    geom_text(aes(label=paste("$",Average_Premium),vjust=-0.25)) +
                    ggtitle("Premium of Property which having Neighborhood Watch")+
                    theme(plot.title=element_text(hjust = 0.5),
                             axis.text.y =element_blank(),
                             axis.title.y =element_blank(),
                             axis.ticks.y = element_blank(),
                             axis.text.x=element_text(face="bold"),
                             legend.position ="none")+
                    xlab("Neighborhood Watch")
NEIGH_WATCH_premium

#The Property which have NEIGH_WATCH Installed are likely to paid more premium .

#Lets check the security feature of the property.

#Lets check the premium of the property which having Alarm Installed.

APPR_ALARM_premium=Property %>% select(APPR_ALARM,Premium) %>% group_by(APPR_ALARM) %>%
                   summarise(Average_Premium=round(mean(Premium),2))%>%
                   ggplot(aes(x=reorder(APPR_ALARM,-Average_Premium),y=Average_Premium))+
                   geom_bar(stat ="identity",width =0.5,fill="#80c040",color="Black")+
                   geom_text(aes(label=paste("$",Average_Premium),vjust=-0.25)) +
                   ggtitle("Premium of Property if ALARM Installed")+
                   theme(plot.title=element_text(hjust = 0.5),
                          axis.text.y =element_blank(),
                          axis.title.y =element_blank(),
                          axis.ticks.y = element_blank(),
                          axis.text.x=element_text(face="bold"),
                   legend.position ="none")+
                   xlab("ALARM Installed")
APPR_ALARM_premium


#Lets check the premium of the property which having Safe Installed.

Safe_premium=Property %>% select(SAFE_INSTALLED,Premium) %>% group_by(SAFE_INSTALLED) %>%
             summarise(Average_Premium=round(mean(Premium),2))%>%
             ggplot(aes(x=reorder(SAFE_INSTALLED,-Average_Premium),y=Average_Premium))+
             geom_bar(stat ="identity",width =0.5,fill="#c0c040",color="Black")+
             geom_text(aes(label=paste("$",Average_Premium),vjust=-0.25)) +
             ggtitle("Premium of Property if Safe Installed")+
             theme(plot.title=element_text(hjust = 0.5),
                   axis.text.y =element_blank(),
                   axis.title.y =element_blank(),
                   axis.ticks.y = element_blank(),
                   axis.text.x=element_text(face="bold"),
                   legend.position ="none")+
                   xlab("SAFE INSTALLED")
Safe_premium

Security=APPR_ALARM_premium+Safe_premium
Security
#The Property which have SAFE Installed are likely to paid more premium .

#The Property which have ALARM Installed are likely to paid more premium .

#Lets check the premium of the property which having Building Cover .

BUILDINGS_COVER_premium=Property %>% select(BUILDINGS_COVER,Premium) %>% group_by(BUILDINGS_COVER) %>%
                        summarise(Average_Premium=round(mean(Premium),2))%>%
                        ggplot(aes(x=reorder(BUILDINGS_COVER,-Average_Premium),y=Average_Premium))+
                        geom_bar(stat ="identity",width =0.5,fill="#002080")+
                        geom_text(aes(label=paste("$",Average_Premium),vjust=-0.25)) +
                        ggtitle("Premium of Property which having Building Cover")+
                        theme(plot.title=element_text(hjust = 0.5),
                              axis.text.y =element_blank(),
                              axis.title.y =element_blank(),
                              axis.ticks.y = element_blank(),
                              axis.text.x=element_text(face="bold"),
                              legend.position ="none")+
                         xlab("BUILDINGS_COVER")
BUILDINGS_COVER_premium

#The Property which have Building Cover are likely to paid more premium .