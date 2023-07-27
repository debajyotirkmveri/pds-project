#To read the file#
churn3<-read.csv("C://Users/debaj/Downloads/tourn_1_calibration_csv.csv",header=T)
churn3

####compute this column which na values is less than50%
churn1<-apply(churn3,2,function(col){(sum(is.na(col))/length(col))*100})<50

churn3<-churn3[,churn1]
dim(churn3)

view(churn3)
str(churn3)
###
modes<-function(x){
  un<-unique(x)
  un[which.max(tabulate(match(x,un)))]
}

apply(churn3,2,modes)


library(tidyverse)


#Active subscribe householder------------------------
ggplot(as.data.frame(table(churn3$actvsubs))[-1,],aes(x=Var1,y=Freq)) +
  geom_bar(stat='identity',fill='yellow',color='orange',width=0.7) +
  ggtitle('Bar diagram of number of active subscribers in household') + xlab('No. of active subs in a household') + ylab('Frequency') +
  theme(plot.title = element_text(family='serif',hjust=0.5,size=15)) +
  theme(axis.title.x = element_text(family='mono',size=15)) +
  theme(axis.title.y = element_text(family='mono',size=15)) +
  labs(caption = 'Bar diagram 1') + theme(plot.caption=element_text(family='serif',size=16, hjust=0.5)) +
  geom_text(aes(label=Freq,vjust=-0.3))
summary(churn3$actvsubs)
#churn based on geographical area-----------------------
ggplot(as.data.frame(table(churn3 %>% select(area,churn) %>% group_by(churn)))[-c(1,21),], aes(fill=churn,y=Freq,x=area)) +
  geom_bar(stat='identity',color='slategray1') + ggtitle('Churn based on Geographical Area') + 
  xlab('Area') + ylab('Frequency') + scale_fill_manual(values = c('slateblue3','red3')) +
  theme(axis.text.x = element_text(family='serif', size=10,angle=90,vjust=0.5)) +
  theme(plot.title = element_text(family='serif',hjust=0.45,size=20)) + 
  theme(axis.title.x = element_text(family='mono',hjust=0.5,size=15)) +
  theme(axis.title.y = element_text(family='mono',hjust=0.4,size=15)) +
  labs(caption=' Bar diagram 2') + labs(fill='Churn')
theme(legend.text=element_text(size=12)) +
  theme(plot.caption=element_text(size=12, hjust=0.5))
#condition of phones by ethnicity-------------------------------------
f1 <- as.data.frame(churn3 %>% select(refurb_new,ethnic))
f1$ethnic[f1$ethnic==''] <- NA
f1$refurb_new[f1$refurb_new==''] <- NA
f1 <- na.omit(f1)
ggplot(f1,aes(fill=refurb_new,x=ethnic)) +
  geom_bar() + ggtitle('Condition of handset by Ethnicity') +
  xlab('Ethnicity') + ylab('Frequency Count') + 
  theme(axis.text.x = element_text(family='serif',size=12)) +
  theme(plot.title = element_text(family='serif',hjust=0.45,size=20)) + 
  theme(axis.title.x = element_text(family='mono',hjust=0.5,size=15)) +
  theme(axis.title.y = element_text(family='mono',hjust=0.5,size=15)) +
  labs(caption='Bar diagram 3',fill='Refurbish/New') +
  theme(legend.text=element_text(family='serif',size=12)) +
  theme(plot.caption=element_text(size=12, hjust=0.5)) +
  scale_fill_manual(values = c('sandybrown','royalblue3'))
rm(f1)

#B = Asian (non-Oriental)
#D = Southern European
#F = French
#G = German
#H = Hispanic
#I = Italian
#J = Jewish
#M = Miscellaneous
#N = Northern European
#O = Asian
#P = Polynesian
#R = Arab
#S = Scottish / Irish
#U = Unknown
#Z = African-American

#frequency polygon of age1 based on churn----------------------




churnvalue<-as.factor(churn3$churn)
churn3 %>% select(age1,churn) %>%
  group_by(churn) %>%
  ggplot()+geom_freqpoly(aes(age1,group=churn,color=churnvalue),bins=50,na.rm=TRUE)+xlim(18,120)+labs(caption = 'Frequency polygon1')+ggtitle("Frequency polygon of age1 based on churn value")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))

summary(churn3$age1)

f5 <- as.data.frame(churn3 %>% select(age1,churn))
f5$age1[f5$age1==""]<-NA
f5$churn[f5$churn==""]<-NA
f5<-na.omit(f5)
f5
summary(f5$age1)


#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00    0.00   36.00   30.95   48.00   99.00 

######
summary(churn3$age2)

#   Min.   1st Qu.  Median    Mean 3rd Qu.    Max.   
 #  0.00    0.00    0.00   20.78   42.00   99.00

#scatter plot between  rev_mean and mou_mean------------------------
plot(churn3$rev_Mean,churn3$mou_Mean,col="green",main="Scatter plot between mean monthly revinue and mean number of monthly minutes uses",sub="scatter plot1")
a<-complete.cases(churn3$rev_Mean)
rev_Mean<-churn3$rev_Mean[a]
rev_Mean
mean(churn3$rev_Mean,na.rm=T)
cor(churn3$rev_Mean,churn3$mou_Mean)
summary(rev_Mean)
#frequency polygon of age2 based on churn------------------

churnvalue<-as.factor(churn3$churn)
churn3 %>% select(age2,churn) %>%
  group_by(churn) %>%
  ggplot()+geom_freqpoly(aes(age2,group=churn,color=churnvalue),bins=50,na.rm=TRUE)+xlim(18,120)+labs(caption = 'Frequency polygon 2')+ggtitle("Frequency polygon of age2 based on churn value")

#phone based on churn----------------------------
ggplot(as.data.frame(table(churn3 %>% select(phones,churn) %>% group_by(churn))), aes(fill=churn,y=Freq,x=phones)) +
  geom_bar(stat='identity',color='magenta') + ggtitle('phone based on churn')+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+labs(caption = 'Bar diagram 5') 
#barplot of ethnic------------------------------------------------------ 
f1 <- as.data.frame(churn3 %>% select(ethnic))
f1$ethnic[f1$ethnic==""]<-NA
na.omit(f1$ethnic)
f1$ethnic
as.factor(f1$ethnic)
f2<-table(as.factor(f1$ethnic))
f2

ggplot(f1)+geom_bar(aes(x=f1$ethnic),fill="magenta",col="black")+xlab("Ethnc roll up code")+ggtitle("Ethnic")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+labs(caption = 'Bar diagram4')  
#pie chart of block data calls and voice calls-------------------------------




a<-sum(churn3$drop_dat_Mean)

b<-sum(churn3$blck_vce_Mean)

c<-c(a,b)
prop.table(c)
pie(c,labels=paste0(round(prop.table(c)*100),"%"),col=c("green","red"),radius=1,main=" Blocked data calls and voice calls",sub="pie chart 2")
legend("topright",legend=c("Block data call","Block voice call"),fill=c("green","red"))

#pie chart ofreceive sms calls and voice calls----------------------------------------------

a<-sum(churn3$recv_sms_Mean)

b<-sum(churn3$recv_vce_Mean)

c<-c(a,b)
prop.table(c)
pie(c,labels=paste0(round((prop.table(c)*100),2),"%"),col=c("green","red"),radius=1,main="Mean number of Receive calls",sub="pie chart 3")
legend("topright",legend=c("SMS CALLS"," VOICE CALLS"),fill=c("green","red"))

#Mean number of unanswered calls--------------------------------------

a<-sum(churn3$unan_dat_Mean)

b<-sum(churn3$unan_vce_Mean)

c<-c(a,b)
prop.table(c)
pie(c,labels=paste0(round((prop.table(c)*100),2),"%"),col=c("green","red"),radius=1,main="Mean number of unanswered calls",sub="pie chart 4")
legend("topright",legend=c("DataCALLS"," VOICE CALLS"),fill=c("green","red"))
#----
ggplot(churn3,aes(x=recv_sms_Mean,y=recv_vce_Mean))+geom_point(col="green")+ggtitle("Scatter plot")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+labs(caption = "scatter plot2")

#Box plot of adults and active subscriber------------------------
boxplot(churn3$actvsubs~churn$adults,xlab="Adults per house",ylab="active house holder",col="red",main="Box plot of active house holder based on adults")

#Active subscriber based on area-------------------------------------------

f2 <- as.data.frame(churn3 %>% select(actvsubs,income))
f2$actvsubs[f2$actvsubs==""]<-NA
f2$income[f2$income==""]<-NA
f2<-na.omit(f2)
table(f2)
ggplot(f2,aes(x=factor(actvsubs)))+geom_bar(fill="magenta")+facet_wrap(~income)+ggtitle("Active subscriber based on income")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+labs(caption = 'Bar diagram 6')

ggplot(f2,aes(x=factor(actvsubs)))+geom_bar(fill="magenta",xlab="Active subscriber")+ggtitle("Active subscriber barplot")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+labs(caption = 'Bardiagram 7')
# piechart churn or unchurn------------------------------------------- 

f11 <- as.data.frame(churn3 %>% select(churn))
f11$churn[f11$churn==""]<-NA
f11<-na.omit(f11)
f11
f21<-table(f11)
f21

a<-prop.table(table(f11))*100
a

pie(f21,col=c("red","green"),labels=paste0(a,"%"),radius=0.5,main="Churn or unchurn customer ",sub='pie chart 5')
legend("topright",legend=c("unchurn","churn"),fill=c("red","green"),cex=0.8)
#Active subscriber based on phones---------------------
f4 <- as.data.frame(churn3 %>% select(area,actvsubs,phones))
f4$area[f4$area==""]<-NA
f4$actvsubs[f4$actvsubs==""]<-NA
f4$phones[f4$phones==""]<-NA
f4<-na.omit(f4)
dim(f4)
ggplot(f4,aes(x=factor(phones)))+geom_bar(fill="red")+facet_wrap(~area)+ggtitle("Active subscriber based on phones")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+xlab('phones')+labs(caption = 'Bardiagram 7')

#marital effect on active subscriber-----------------------------

f5 <- as.data.frame(churn3 %>% select(marital,actvsubs))
f5$actvsubs[f5$actvsubs==""]<-NA
f5$marital[f5$marital==""]<-NA
f5<-na.omit(f5)
table(f5)
ggplot(f5,aes(x=factor(marital)))+geom_bar(fill=c("red","green","blue","yellow","magenta"))+ggtitle("Active subscriber based on marital")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+labs(caption = 'Bardiagram 8')



U = Unknown
M = Married
S = Single
B = Inferredsingle
A = Inferredmarried
#matial status on churn-----------------------
f13 <- as.data.frame(churn3 %>% select(churn,marital))
f13$churn[f13$churn==''] <- NA
f13$marital
f13$marital[f13$marita=='']<-NA


f13 <- na.omit(f13)
table(f13)
counts<-table(f13)
barplot(counts,main="Marital status based on churn",xlab="Marital",ylab="frequency")
barplot(counts,main="Marital status based on churn",xlab="Marital",ylab="frequency",col=c("red","green"),legend=c("unchurn","churn"),beside=TRUE,args.legend = list(x="topleft"))




U = Unknown
M = Married
S = Single
B = Inferredsingle
A = Inferredmarried
#rug plot of roam mean and mou_mean----------------------


ggplot(churn3)+geom_rug(aes(x=roam_Mean,y=mou_Mean))

#Mean number of waiting  calls--------------------------
ggplot(churn3,aes(x=callwait_Mean,y=..density..))+geom_histogram(fill="magenta",breaks=seq(0,20,2))+ggtitle("Mean number of waiting calls")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))

ggplot(churn3,aes(x=callwait_Mean,y=..density..))+geom_freqpoly(col="red",breaks=seq(0,20,2))+ggtitle("Mean number of waiting calls")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+labs(caption = 'Frequency polygon3 ') 
#mean number of attempted calls------------------
ggplot(churn3,aes(x=attempt_Mean,y=..density..))+geom_histogram(fill="yellow",breaks=seq(0,2000,100))+ggtitle("Mean number of attempted calls")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+labs(caption = 'histogarm 2 ') 
ggplot(churn3,aes(x=callwait_Mean,y=..density..))+geom_freqpoly(col="blue",breaks=seq(0,2000,100))+ggtitle("Mean number of attempted calls")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+labs(caption = 'Frequency polygon3 ') 

#
#Mean number of forwarding calls------------
ggplot(churn3,aes(x=callfwdv_Mean))+geom_freqpoly(col="red",breaks=seq(0,20,2))+ggtitle("Mean number of forwarding calls")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20)) 

#Mean number of complete calls----------------

ggplot(churn3,aes(x=complete_Mean,y=..density..))+geom_histogram(fill="red",breaks=seq(0,1900,50))+ggtitle("Mean number of complete calls")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+labs(caption="Histogram 3") 
ggplot(churn3,aes(x=complete_Mean,y=..density..))+geom_freqpoly(col="blue",breaks=seq(0,1900,50))+ggtitle("Mean number of complete calls")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+labs(caption = 'Frequency polygon4 ') 

#Inbound and out bound wireless to wireless voice calls-------------------
churn$mouiwylisv_Mean
churn$mouowylisv_Mean

tab1<-churn3 %>% select(mouiwylisv_Mean,mouowylisv_Mean)
tab1

ggplot(churn3,aes(x=mouiwylisv_Mean,y=..density..))+geom_histogram(fill="red",breaks=seq(0,200,10))+ggtitle("Mean unrounded minutes of use of inbound wireless to wireless voice calls")+labs(caption="Histogram4")
ggplot(churn3,aes(x=mouowylisv_Mean,y=..density..))+geom_histogram(fill="blue",breaks=seq(0,200,10))+ggtitle("Mean unrounded minutes of use of outbound wireless to wireless voice calls")+labs(caption="Histogram5")


ggplot(churn3,aes(x=mouiwylisv_Mean))+geom_freqpoly(col="red")+ggtitle("Mean unrounded minutes of use of inbound wireless to wireless voice calls")+labs(caption="Frequency poly gon 5")
ggplot(churn3,aes(x=mouowylisv_Mean))+geom_freqpoly(col="blue")+ggtitle("Mean unrounded minutes of use of outbound wireless to wireless voice calls")+labs(caption="Frequency polygon 6")

# histogram of total number calls of the life of the customer----------------------
ggplot(churn3,aes(x=totcalls,y=..density..))+geom_histogram(fill="green",breaks=seq(0,25000,1000))+ggtitle("Total number of calls of the life of the customer")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))
# Educaton of first house member-----------------
churn2<-read.csv("C://Users/debaj/Downloads/tourn_1_calibration_csv.csv",header=T)
churn2


ggplot(churn2,aes(x=educ1))+geom_bar(fill="red")+ggtitle("Education of first house hold member")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+labs(caption="Bardiagram 10")  
#1 = Completed high school
#2 = Completed college
#3 = Completed graduate school
#4 = Attended vocational / technical school




##billing adjusted total minutes of use over life customer histogram---------- 

f1 <- as.data.frame(churn3 %>% select(adjmou,area))
f1$adjmou[f1$adjmou==""]<-NA
f1$area[f1$area==""]<-NA

f1<-na.omit(f1)

ggplot(f1,aes(x=adjmou,y=..density..))+geom_histogram(fill="blue",breaks=seq(0,600000,10000))+facet_wrap(~area)+ggtitle("Billing adjusted total minutes of use over life customer")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))


#churn based on income---------------------------------
f3 <- as.data.frame(churn3 %>% select(churn,income))
f3$churn[f3$churn==""]<-NA
f3$income[f3$income==""]<-NA
f3<-na.omit(f3)
table(f3)
ggplot(f3)+geom_bar(fill="magenta",aes(x=factor(income)))+facet_wrap(~churn)
#Mean rounded minutes of use of customer care (see CUSTCARE_MEAN) calls-------------------

f61 <- as.data.frame(churn3 %>% select(ccrndmou_Mean,churn))
f61$ccrndmou_Mean[f61$ccrndmou_Mean==""]<-NA
f61$churn[f61$churn==""]<-NA
f61<-na.omit(f61)
f61
b<-subset(f61,churn==1)
b
ggplot(b)+geom_freqpoly(aes(ccrndmou_Mean,col="churn"),bins=50,na.rm=TRUE)+xlim(18,120)+ggtitle("Mean rounded minutes of use of customer care (see CUSTCARE_MEAN) calls")+labs(caption="Frequency poly gon 6")
##pie chart about number of kids in the household-------------------
churn3$kid0_2=="Y"
churn3$kid0_2[churn3$kid0_2=="Y"]
kid0_2<-table(churn3$kid0_2[churn3$kid0_2=="Y"])
kid0_2

churn3$kid3_5=="Y"
churn3$kid0_2[churn3$kid3_5=="Y"]
kid3_5<-table(churn3$kid3_5[churn3$kid3_5=="Y"])
kid3_5

churn3$kid6_10=="Y"
churn3$kid6_10[churn3$kid6_10=="Y"]
kid6_10<-table(churn3$kid6_10[churn3$kid6_10=="Y"])
kid6_10

churn3$kid11_15=="Y"
churn3$kid11_15[churn3$kid11_15=="Y"]
kid11_15<-table(churn3$kid11_15[churn3$kid11_15=="Y"])
kid11_15

churn3$kid16_17=="Y"
churn3$kid16_17[churn3$kid16_17=="Y"]
kid16_17<-table(churn3$kid16_17[churn3$kid16_17=="Y"])
kid16_17

kid<-c(kid0_2,kid3_5,kid6_10,kid11_15,kid16_17)
kid
pie(kid,labels=paste0(round((prop.table(kid)*100),2),"%"),col=c("green","red","blue","yellow","magenta"),radius=1,main="Number of kids in the household",sub="pie chart 1")
legend("topright",legend=c("kid0_2","kid3_5","kid6_10","kid11_15","kid16_17"),fill=c("green","red","blue","yellow","magenta"),cex=0.7)
###-------------
theme(axis.text.x = element_text(family='serif', size=10,angle=90,vjust=0.5))
##
f151<-as.data.frame(churn3 %>% select(avg6qty))


f151$avg6qty[f151$avg6qty=='']<-NA
f151<-na.omit(f151)

ggplot(f151,aes(x=avg6qty,y=..density..))+geom_histogram(fill="green")+ggtitle("Average monthly number of calls over last six month")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))



####pie chart about income of the family
Question:2-------------------------------#question-3-----------#question-4----------------
#categorical to categorical(building new features)
#pie chart about income of the family
f12=as.data.frame(churn3%>%select(income))
f12
f12$income[f12$income==""]<-NA
f12=na.omit(f12)
f12
dim(f12)
min(f12)
max(f12)
f12$income_cha=ifelse(f12$income<=2,"poor class family",ifelse(f12$income<=5,"middle class family","higher class family"))
f12
f12$income_cha
f12

f13<-table(f12$income_cha)
f13
pie(f13,labels=paste0(round((prop.table(f13)*100),2),"%"),col=c("green","red","blue"),radius=1,main="Family class",sub="pie chart")
legend("topright",legend=c("higher class family","middle class family","poor class family"),cex=0.7,fill=c("green","red","blue"))
#####pie chart about mean number of completed data calls
##numerical to categorical---------------
f11=as.data.frame(churn3%>%select(comp_dat_Mean))
f11
f11$comp_dat_Mean[f11$comp_dat_Mean==""]<-NA
f11=na.omit(f11)
f11
dim(f11)
min(f11)
max(f11)          
f11$cat=cut(f11$comp_dat_Mean,breaks=c(-1,80,180,300,420,500,560),labels=c("low","medium","good","very good","better","best"))
f11
df12<-as.data.frame(cbind(churn3,f11$cat))
df12
dim(df12)


churn3
f11$cat
f13<-table(f11$cat)
f13
barplot(f13,labels=paste0(round((prop.table(f13)*100),2),"%"),col=c("green","red","blue","magenta","yellow"),main="category of completed data calls",sub="barplot11")
legend("topright",legend=c("low","medium","good","very good","better","best"),cex=0.7,fill=c("green","red","blue","magenta","yellow"))
#######
#deriving new features using logical conditiontotal number of calls over life
#
f15=churn3%>%select(totcalls)
f15
sum(is.na(f15$totcalls))
ggplot(f15,aes(x=totcalls,y=..density..))+geom_histogram(fill="green")+ggtitle("Total number of calls over life of customer")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))

churn3$totcalls
summary(churn3$totcalls)

f16=ifelse(f15$totcalls<=4000,"low communitated family",ifelse(f15$totcalls<=10000,"middle communicated family",ifelse(f15$totcalls<=50000,"highly communicated family","most highly communicated family")))
f16
f17=table(f16)
f17
pie(f17,labels=paste0(round((prop.table(f17)*100),2),"%"),col=c("red","blue","green","yellow"),radius=0.7,main="Total number of calls over life",sub="pie chart")
legend("topright",legend=c(" most highly communicated family","highly communicated family","middle communicated family","low communitated family"),cex=0.5,fill=c("red","blue","green","yellow"))



###question:3
###histogram of average monthly number of calls over last six months--------------
f13=churn3%>%select(avg6qty)
f13
sum(is.na(f13$avg6qty))
f13_1=na.omit(f13)
f13_1

#after removing the na values
ggplot(f13_1,aes(x=avg6qty,y=..density..))+geom_histogram(fill="magenta")+ggtitle("Average monthly number of calls over last six month")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))

mean(na.omit(f13$avg6qty))
dim(f13)
#replace na values by mean values
f13[is.na(churn3$avg6qty)==T,1]=178.36
f13
sum(is.na(f13$avg6qty))
ggplot(f13,aes(x=avg6qty,y=..density..))+geom_histogram(fill="magenta")+ggtitle("Average monthly number of calls over last six month")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))
####
f13=churn3%>%select(avg6qty)
f13
sum(is.na(f13$avg6qty))
f13_1=na.omit(f13)
f13_1

summary(f13_1)
#after removing the na values
ggplot(f13_1,aes(x=avg6qty,y=..density..))+geom_histogram(fill="green")+ggtitle("Average monthly number of calls over last six month")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))

dim(f13)
#replace na values by median values
f13[is.na(churn3$avg6qty)==T,1]=127
f13
sum(is.na(f13$avg6qty))

ggplot(f13,aes(x=avg6qty,y=..density..))+geom_histogram(fill="green")+ggtitle("Average monthly number of calls over last six month")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))
###
########replacing "NA" values by mode values
f13=churn3%>%select(avg6qty)
f13
sum(is.na(f13$avg6qty))
f13_1=na.omit(f13)
f13_1
modes(f13_1$avg6qty)
summary(f13_1)
#after removing the na values
ggplot(f13_1,aes(x=avg6qty,y=..density..))+geom_histogram(fill="red")+ggtitle("Average monthly number of calls over last six month")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))

dim(f13)
#replace na values by mode values
f13[is.na(churn3$avg6qty)==T,1]=0
f13
sum(is.na(f13$avg6qty))

ggplot(f13,aes(x=avg6qty,y=..density..))+geom_histogram(fill="red")+ggtitle("Average monthly number of calls over last six month")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))




library(tidyverse)
###############
#Mean number of monthly minutes of uses---------------
f14=churn3%>%select(mou_Mean)
f14
sum(is.na(f14$mou_Mean))

f14_1=na.omit(f14)
f14_1
summary(f14_1)
#after removing na values
ggplot(f14_1,aes(x=mou_Mean,y=..density..))+geom_histogram(fill="purple")+ggtitle("Mean number of monthly minutes of use")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))
ggplot(f14_1)+geom_boxplot(aes(mou_Mean),fill="purple")+ggtitle("Mean number of monthly minutes of use")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))

#replace na values by median values
f14[is.na(churn3$mou_Mean)==T,1]=355.5
f14
sum(is.na(f14$avg6qty))
ggplot(f14,aes(x=mou_Mean,y=..density..))+geom_histogram(fill="purple")+ggtitle("Mean number of monthly minutes of use")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))
ggplot(f14)+geom_boxplot(aes(mou_Mean),fill="purple")+ggtitle("Mean number of monthly minutes of use")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))

##############--------------------
#mou mean 
f14=churn3%>%select(mou_Mean)
f14
sum(is.na(f14$mou_Mean))

f14_1=na.omit(f14)
f14_1
summary(f14_1)
#after removing na values
ggplot(f14_1,aes(x=mou_Mean,y=..density..))+geom_histogram(fill="blue")+ggtitle("Mean number of monthly minutes of use")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))

#replace na values by mean values
f14[is.na(churn3$mou_Mean)==T,1]=513.6
f14
sum(is.na(f14$mou_Mean))
ggplot(f14,aes(x=mou_Mean,y=..density..))+geom_histogram(fill="blue")+ggtitle("Mean number of monthly minutes of use")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))
##############
f14=churn3%>%select(mou_Mean)
f14
sum(is.na(f14$mou_Mean))

f14_1=na.omit(f14)
f14_1
summary(f14_1)
modes(churn3$mou_Mean)
#after removing na values
ggplot(f14_1,aes(x=mou_Mean,y=..density..))+geom_histogram(fill="orange")+ggtitle("Mean number of monthly minutes of use")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))

#replace na values by mode values
f14[is.na(churn3$mou_Mean)==T,1]=0
f14
sum(is.na(f14$mou_Mean))
ggplot(f14,aes(x=mou_Mean,y=..density..))+geom_histogram(fill="orange")+ggtitle("Mean number of monthly minutes of use")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))

### identification of missing values appropiately substitute the missing values ----------------------
#identification of missing value and appropriately sudstituting the missing value
f18=churn3%>%select(income)
f18
table(f18)
sum(is.na(f18$income))
f18_1=na.omit(f18)
f18_1
f19=table(f18_1)
f19

pie(f19,labels=paste0(round((prop.table(f19)*100),2),"%"),col=c("green","white","blue","magenta","yellow","purple","brown","cadetblue","azure"),radius=0.6,main="Family Income",sub="pie chart")
legend("topright",legend=c("Less than $15,000","$15,000 to $19,999","$20,000 to $29,999","$30,000 to $39,999","$40,000 to $49,999","$50,000 to $74,999","$75,000 to $99,999","$100,000 to $124,999","$125,000 +"),cex=0.7,fill=c("green","white","blue","magenta","yellow","purple","brown","cadetblue","azure"))

#replace na values by different category
f18[is.na(churn3$income)==T,1]=10
f18
f20=table(f18)
f20
sum(is.na(f18$income))
pie(f20,labels=paste0(round((prop.table(f20)*100),2),"%"),col=c("green","white","blue","magenta","yellow","purple","brown","cadetblue","azure","chocolate 1"),radius=0.6,main="Family Income",sub="pie chart")
legend("topright",legend=c("Less than $15,000","$15,000 to $19,999","$20,000 to $29,999","$30,000 to $39,999","$40,000 to $49,999","$50,000 to $74,999","$75,000 to $99,999","$100,000 to $124,999","$125,000 +","unknown"),cex=0.7,fill=c("green","white","blue","magenta","yellow","purple","brown","cadetblue","azure","chocolate 1"))
############
#identification of missing value and appropriately sudstituting the missing value
f22=churn3%>%select(numbcars)
f22
table(f22)
sum(is.na(f22$numbcars))
f22_1=na.omit(f22)
f22_1
f23=table(f22_1)
f23

pie(f23,labels=paste0(round((prop.table(f23)*100),2),"%"),col=c("green","blue","magenta"),radius=1,main="Numbers of vehicles in household",sub="pie chart")
legend("topright",legend=c(1,2,3),cex=0.7,fill=c("green","blue","magenta"))

#replace na values by different levels
f22[is.na(churn3$numbcars)==T,1]=4
f22
f24=table(f22)
f24
sum(is.na(f22$numbcars))

pie(f24,labels=paste0(round((prop.table(f24)*100),2),"%"),col=c("green","blue","magenta","orange"),radius=1,main="Numbers of vehicles in household",sub="pie chart")
legend("topright",legend=c(1,2,3,"unknown"),cex=0.7,fill=c("green","blue","magenta","orange"))
###
###numerical to categorical (building new features)
#month service
library(tidyverse)
df=vector(length = 100000)
df[churn3$months>30]="More months in service"
df[churn3$months<=30]="Less months in service"
df
df1=as.data.frame(cbind(churn3$churn,df))
df1
colnames(df1) = c("churn","months_service")
df1
ggplot(df1)+geom_bar(aes(fill=churn,x=months_service),position = "dodge")+labs(title="Multiple Barplot of months in service and churn non-churn",subtitle = "Multiple barplot")
####categorical to categorical(building new features)-----------
f12=as.data.frame(churn3%>%select(income))
f12
#removing na values
f2=na.omit(f12)
f2
dim(f2)
f2$income_cha=ifelse(f2$income<=3,"poor class family",ifelse(f2$income<=6,"middle class family","higher class family"))
f2
f2$income_cha
f3=table(f2$income_cha)
f3

pie(f3,labels=paste0(round((prop.table(f3)*100),2),"%"),col=c("green","red","blue"),radius=1,main="Family status",sub="pie chart")
legend("topright",legend=c("higher class family","middle class family","poor class family"),cex=0.7,fill=c("green","red","blue"))
#####churn based on income------------------------------------------------------------------------------------------------
library(tidyverse)

df1<-as.data.frame(churn3 %>% select(income,churn))
df1
df2<-complete.cases(df1$income,df1$churn)
df2
df3<-df1[df2,]
df3
df3[df3$churn==0,2]<-"unchurn"
df3[df3$churn==1,2]<-"churn"
df3
bar<-ggplot(df3)+geom_bar(mapping=aes(x=factor(income),fill=churn),position="dodge")+xlab("Income")+ggtitle("Churn based on income")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))
bar
bar<-ggplot(df3)+geom_bar(mapping=aes(x=factor(income)),fill="blue")+xlab("Income")+ggtitle("Income level of the customer")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+ylim(0,20000)
bar


bar<-ggplot(df3)+geom_bar(mapping=aes(x=factor(income),fill=churn),position="fill")+xlab("Income")+scale_fill_brewer(palette="Pastel1")

bar
bar+coord_polar()
##churn based on income on area wise---------------------- 

df1<-as.data.frame(churn3 %>% select(income,churn,area))
df1
df2<-complete.cases(df1$income,df1$churn)
df2
df3<-df1[df2,]
df3
df3[df3$churn==0,2]<-"unchurn"
df3[df3$churn==1,2]<-"churn"
df3
df4<-subset(df3,area!="")
df4


b<-ggplot(df4)+geom_bar(aes(factor(income),fill=churn),position="dodge")+facet_wrap(~df4$area)+xlab("Income")+ggtitle("Area based Income")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))
b
b<-ggplot(df4)+geom_bar(aes(factor(income)),fill="orange")+facet_wrap(~df4$area)+xlab("Income")+ggtitle("Area based income")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))
b

table(df4)

getwd()
#dwell size based on churn----------------------------
df4<-as.data.frame(churn3 %>% select(dwllsize,churn))
df4
df6<-subset(df4,dwllsize!="")
df6
df6[df6$churn==0,2]<-"unchurn"
df6[df6$churn==1,2]<-"churn"
df6
totals<-table(df6$dwllsize,df6$churn)
mosaicplot(totals,main="dwllsize based on churn",col=c("green","blue"),xlab="Dwllsize",ylab="churn")
#dual band based on churn over area----------------------------------------
churn3$dualband
table(churn3$dualband)
dual<-as.data.frame(churn3 %>% select(dualband,churn,area))
dual                
df7<-filter(dual,dualband!="",area!="")
df7
df7[df7$churn==0,2]<-"unchurn"
df7[df7$churn==1,2]<-"churn"
df7
b<-ggplot(df7)+geom_bar(aes(dualband,fill=churn),position="dodge")+facet_wrap(~df7$area)+xlab("Dual band")+ggtitle("Dual band based on churn over the area")
b
##lor based on churn----------------------------------
churn3$lor
df5<-churn3 %>% select(churn,lor)
df5
df5[df5$churn==0,1]<-"unchurn"
df5[df5$churn==1,1]<-"churn"

df6<-filter(df5,df5$lor!="NA")
df6

b<-ggplot(df6)+geom_bar(aes(factor(lor),fill=churn),position="dodge")+xlab("lor")+ggtitle("lor based on churn ")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))
b
#uniqsubscriber in household--------------------------
churn3$uniqsubs
df5<-churn3 %>% select(churn,uniqsubs,area)
df5
df5[df5$churn==0,1]<-"unchurn"
df5[df5$churn==1,1]<-"churn"

df6<-filter(df5,df5$area!="")
df6
table(df6$uniqsubs)
b<-ggplot(df6)+geom_bar(aes(factor(uniqsubs),fill=churn),position="dodge")+facet_wrap(~area)+xlab("uniqsubscriber in household")+ggtitle("uniqsubscriber in household based on churn ")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))
b
#histogram of billing adjusted totalrevenue life of the customer------------------------

churn3$adjrev

df5<-churn3 %>% select(adjrev,churn,area)
df5
df5[df5$churn==0,2]<-"unchurn"
df5[df5$churn==1,2]<-"churn"
df5
df6<-filter(df5,df5$area!="")
df6


ggplot(df6,aes(x=adjrev,y=..density..))+geom_histogram(fill="orange",binwidth = 500)+facet_wrap(~area)+ggtitle("Billing adjusted total revenue life of the customer")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+xlab("Billing adjusted total revenue life of the customer based")
ggplot(df6,aes(x=adjrev,y=..density..))+geom_histogram(fill="orange",binwidth = 500)+facet_wrap(~churn)+ggtitle("Billing adjusted total revenue life of the customer based on churn")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+xlab("Billing adjusted total revenue life of the customer based")

ggplot(df6,aes(x=adjrev,y=..density..,fill=churn))+geom_freqpoly(breaks=seq(0,5000,100))

#pie chart of mean overage revinue based on churn--------------------
churn3$dat
df5<-churn3 %>% select(datovr_Mean,vceovr_Mean,churn)
df5

df6<-filter(df5,datovr_Mean!="NA",vceovr_Mean!="NA",churn!="Na")
df6


df6[df6$churn==0,3]<-"unchurn"
df6[df6$churn==1,3]<-"churn"
df6
df7<-filter(df6,df6$churn=="churn")
df7
df8<-filter(df6,df6$churn=="unchurn")
df8
a<-sum(df7$datovr_Mean,df7$vceovr_Mean)##churn
a
b<-sum(df8$datovr_Mean,df7$vceovr_Mean)##unchurn
b
ovrrev_mean<-c(a,b)
ovrrev_mean
pie(ovrrev_mean,labels=paste0(round((prop.table(ovrrev_mean)*100),2),"%"),col=c("green","blue"),radius=1,main="Mean ovrerage revinue",sub="pie chart")
legend("topright",legend=c("churn","unchurn"),cex=0.7,fill=c("green","blue"))
###Mean number of attempt calls&block calls&complete calls---------------
a<-sum(churn3$drop_blk_Mean)
a
sum(churn3$blck_dat_Mean)+sum(churn3$blck_vce_Mean)+sum(churn3$drop_dat_Mean)+sum(churn3$drop_vce_Mean)
b<-sum(churn3$complete_Mean)
b
c<-sum(churn3$attempt_Mean)
c
call<-c(a,b,c)
call
pie(call,labels=paste0(round((prop.table(call)*100),2),"%"),col=c("green","blue","orange"),radius=1,main="Mean number of attempt&complete&drop calls",sub="pie chart")
legend("topright",legend=c("drop calls","complete calls","attempt calls"),cex=0.7,fill=c("green","blue","orange"))
####Histogram of attempted calls based on churn------------------
df5<-churn3 %>% select(attempt_Mean,churn,area)
df5
df5[df5$churn==0,2]<-"unchurn"
df5[df5$churn==1,2]<-"churn"
df5
df6<-filter(df5,df5$area!="")
df6


ggplot(df6,aes(x=attempt_Mean,y=..density..))+geom_histogram(fill="orange",binwidth = 200)+facet_wrap(~area)+ggtitle("Mean nunmber of Attempted calls")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+xlab("Attempted calls mean")
ggplot(df6,aes(x=attempt_Mean,y=..density..))+geom_histogram(fill="orange",binwidth = 200)+facet_wrap(~churn)+ggtitle("Mean nunmber of Attempted calls")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+xlab("Attempted calls mean")
####Histogram of dropped  calls based on churn---------------------------
df5<-churn3 %>% select(drop_blk_Mean,churn,area)
df5
df5[df5$churn==0,2]<-"unchurn"
df5[df5$churn==1,2]<-"churn"
df5
df6<-filter(df5,df5$area!="")
df6

ggplot(df6,aes(x=(drop_blk_Mean),y=..density..))+geom_histogram(fill="orange",binwidth = 30)+facet_wrap(~area)+ggtitle("Mean nunmber of Dropped calls")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+xlab("Dropped calls mean")
ggplot(df6,aes(x=drop_blk_Mean,y=..density..))+geom_histogram(fill="orange",binwidth = 30)+facet_wrap(~churn)+ggtitle("Mean nunmber of Dropped calls based on churn")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+xlab("Dropped calls mean")
##histogram of complete calls-------------------
df5<-churn3 %>% select(complete_Mean,churn,area)
df5
df5[df5$churn==0,2]<-"unchurn"
df5[df5$churn==1,2]<-"churn"
df5
df6<-filter(df5,df5$area!="")
df6

ggplot(df6,aes(x=(complete_Mean),y=..density..))+geom_histogram(fill="orange",binwidth = 60)+facet_wrap(~area)+ggtitle("Mean nunmber of complete calls")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+xlab("Dropped calls mean")
ggplot(df6,aes(x=complete_Mean,y=..density..))+geom_histogram(fill="orange",binwidth = 60)+facet_wrap(~churn)+ggtitle("Mean nunmber of complete  calls based on churn")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+xlab("Dropped calls mean")
##histogram of attempt range ----------------------------
library(tidyverse)
df7_1<-churn3 %>% filter(attempt_Range!="",churn!="")
df7_1
df7<-df7_1 %>% select(attempt_Range,churn)
df7
df7[df7$churn==0,2]<-"unchurn"
df7[df7$churn==1,2]<-"churn"
df7
df8<-df7$attempt_Range
df8
ggplot(df7,aes(x=attempt_Range,y=..density..))+geom_histogram(fill="orange",binwidth = 60)+facet_wrap(~churn)+ggtitle("Attempt range of calls based on churn")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+xlab("Attempt range")

### homeowner based on churn--------------------
df9<-churn3 %>% filter(ownrent!="",churn!="")
df9
df9_1<-df9 %>% select(ownrent,churn)
df9_1
df9_1[df9_1$churn==0,2]<-"unchurn"
df9_1[df9_1$churn==1,2]<-"churn"
df9_1
b<-ggplot(df9_1)+geom_bar(aes(factor(ownrent),fill=churn),position="dodge")+xlab("HOme owner and renter")+ggtitle("Home owner and renter based on churn ")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+ylim(0,40000)
b
##
df9<-churn3 %>% filter(prizm_social_one!="",churn!="",area!="")
df9
df9_1<-df9 %>% select(prizm_social_one,churn,area)
df9_1
df9_1[df9_1$churn==0,2]<-"unchurn"
df9_1[df9_1$churn==1,2]<-"churn"
df9_1[df9_1$prizm_social_one=="C",1]<-"City"
df9_1[df9_1$prizm_social_one=="R",1]<-"Rural"
df9_1[df9_1$prizm_social_one=="S",1]<-"Suburban"
df9_1[df9_1$prizm_social_one=="T",1]<-"Town"
df9_1[df9_1$prizm_social_one=="U",1]<-"urban"
df9_1
table(df9_1$prizm_social_one)
table(df9_1$churn)
table(df9_1$area)
b<-ggplot(df9_1)+geom_bar(aes(factor(prizm_social_one),fill=churn),position="dodge")+xlab("social group")+ggtitle("social group based on churn ")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+ylim(0,2000)+facet_wrap(~area)
b
b<-ggplot(df9_1)+geom_bar(aes(prizm_social_one,fill=churn),position="dodge")+xlab("social group of the customer")+ggtitle("social group based on churn ")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+ylim(0,20000)
b
####-----------------------------
library(tidyverse)
df=vector(length = 100000)
df[churn3$months>30]="More months in service"
df[churn3$months<=30]="Less months in service"
df
df1=as.data.frame(cbind(churn3$churn,df))
df1
colnames(df1) = c("churn","months_service")
df1
ggplot(df1)+geom_bar(aes(fill=churn,x=months_service),position = "dodge")+ylim(0,50000)+labs(title="Multiple Barplot of months in service and churn non-churn",subtitle = "Multiple barplot")
####car_buy based on income-------------------------
tab1=churn3%>%select(car_buy,income)
tab1
sum(is.na(tab1$income))
tab1_1=na.omit(tab1)
tab1_1
library(tidyverse)
ggplot(tab1_1)+geom_bar(mapping=aes(x=factor(income),fill=car_buy),position="dodge")+xlab("income")+labs(title="Car buy based on income",subtitle = "Multiple barplot")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))

####churn based on income-------------------------
tab2=churn3%>%select(churn,income)
tab2
tab2_1=tab2[tab2$churn==1,]
tab2_1


tab2_2=na.omit(tab2_1)
tab2_2
tab2_2[tab2_2$churn==1,1]="churn"
tab2_2

bar<-ggplot(tab2_2)+geom_bar(mapping=aes(x=factor(income)),fill="blue")+facet_wrap(~churn)+xlab("income")+labs(title="Churn based on income")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))
bar
bar+coord_polar()
tab2=churn3%>%select(churn,income)
tab2
tab2_1=tab2[tab2$churn==0,]
tab2_1


tab2_2=na.omit(tab2_1)
tab2_2
tab2_2[tab2_2$churn==0,1]="unchurn"
tab2_2

bar<-ggplot(tab2_2)+geom_bar(mapping=aes(x=factor(income)),fill="blue")+facet_wrap(~churn)+xlab("income")+labs(title="Churn based on income")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))
bar
bar+coord_polar()

##########known numbers of vehicles based on income----------------------------

tab3=churn3%>%select(numbcars,income)
tab3
tab3_1=na.omit(tab3)
tab3_1
tab3_1[tab3_1$numbcars==1,1]="single"
tab3_1
tab3_1[tab3_1$numbcars==2,1]="double"
tab3_1[tab3_1$numbcars==3,1]="tripple"
tab3_1

ggplot(tab3_1)+geom_bar(mapping=aes(x=factor(income),fill=numbcars),position="dodge")+xlab("income")+labs(title="Known number of vehicles based on income",subtitle = "multiple bar diagram")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))

#######months service based on income-------------------------------------
tab4=churn3%>%select(months,income)
tab4
tab4_1=na.omit(tab4)
tab4_1
ggplot(tab4_1)+geom_histogram(mapping=aes(x=months,y=..density..),fill="magenta",binwidth = 5)+facet_wrap(~income)+xlab("Total number of months in service")+ggtitle("Months service based on income level")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))



###number of handset issued based on income----------------------
tab5=churn3%>%select(income,phones)
tab5
tab5_1=na.omit(tab5)
tab5_1
tab5_2=tab5_1[tab5_1$phones<=10,]
tab5_2
ggplot(tab5_2)+geom_bar(mapping=aes(x=factor(phones)),fill="orange")+facet_wrap(~income)+xlab("phones")+labs(title="Number of handset issued based on income")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))

####marital status based on income------------------------------
library(tidyverse)
tab6=churn3%>%select(income,marital)
tab6

tab6_1=na.omit(tab6)
tab6_1
tab6_2=tab6_1[tab6_1$marital=="M"|tab6_1$marital=="S",]
tab6_2


ggplot(tab6_2)+geom_bar(mapping=aes(x=factor(income),fill=marital),position="dodge")+xlab("income")+labs(title="Marital status based on income",subtitle = "Multiple barplot")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))
###foreign travel based on income-----------------
tab7<-churn3 %>% select(income,forgntvl)
tab7
tab7_1<-na.omit(tab7)
tab7_1
tab7_1[tab7_1$forgntvl==0,2]<-"NO"
tab7_1[tab7_1$forgntvl==1,2]<-"YES"
tab7_1
ggplot(tab7_1)+geom_bar(mapping=aes(x=factor(income),fill=forgntvl),position="dodge")+xlab("income")+labs(title="foreign travel dummy variable  based on income",subtitle = "Multiple barplot")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+ylim(0,20000)
###




#total months in service based on churn-------------
df9<-churn3 %>% filter(months!="",churn!="")
df9
df9_1<-df9 %>% select(months,churn)
df9_1

df9_1[df9_1$churn==0,2]<-"unchurn"
df9_1[df9_1$churn==1,2]<-"churn"

df9_1
ggplot(df9_1)+geom_bar(mapping=aes(x=(months)),fill="red",width=0.2)+xlab("months")+facet_wrap(~churn)+labs(title="Total months in service based on churn",subtitle = "barplot")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))+ylim(0,6000)
#---------------
df9<-churn3 %>% filter(callwait_Mean!="",churn!="")
df9
df9_1<-df9 %>% select(callwait_Mean,churn)
df9_1

df9_1[df9_1$churn==0,2]<-"unchurn"
df9_1[df9_1$churn==1,2]<-"churn"

df9_1
ggplot(df9_1)+geom_freqpoly(mapping=aes(x=(callwait_Mean)),color="green")+facet_wrap(~churn)+xlab("call wait mean")+labs(title="mean number of waiting calls based on churn",subtitle = "frequency ppolygon ")+theme(plot.title = element_text(family='serif',hjust=0.45,size=20))


#----------
