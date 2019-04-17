#concatenating tables- "fill" includes columns that are different between the two
OG<-rbind.fill(OG.5,OG.8)

#renaming columns
setnames(OG, 
         old=c("Sample","Singlets.Lymphocytes.LD..CD3..Q1..CD4_...CD8..Count") ,
         new=c("StudyID","CD8.Pos"))

#splitting values of a variable by some parameter (only works for strings: factors & characters)
split = strsplit(as.character(OG$StudyID),":")

#create a new column with the a values from the split
OG$Donor = as.factor(sapply(splStudyID,"[",2))

# REORDER A FACTOR VARIABLE
OG$TEST = factor(OG$Antigen,levels=c("UN","PPD","WCL","PEP","SEB"))

#delete entire rows based on the value in a cell- must be a data.table
OG<- OG[!(Donor=="NA")]

#Melt your data table- id.vars are the variables which you want to "melt" by
#can melt a data.table or a data.frame 
melted<-melt(OG, id.vars=c("Antigen", "RE"))
#for some reason it stores all the values as character so coerce them to numeric to run stats
melted$value<-as.numeric(as.character(melted$value))

#summary statistics spit out as a data fram
stats<-as.data.frame(
    ddply(DATA, c("Antigen", "RE", "variable"), summarise,
          mean = mean(value), sd = sd(value),
          sem = sd(value)/sqrt(length(value))))

# trying to mass turn data to numeric from factor
jfhuigls <- data.frame(lapply(OG.RE, function(x) as.numeric(as.character(x))))
melted$value<-as.numeric(as.character(melted$value))

#do a T-test on two subsets of the data
t.test(subset(melted$value,melted$Antigen1 == "UN" & melted$variable=="IFN"),
       subset(melted$value,melted$Antigen1 == "SEB" & melted$variable=="IFN"))


#change individual cells based on their content
levels(TB$TBAntigen)[levels(TB$TBAntigen)==">10"] <- "10"
ELISA$Result[ELISA$Result>500]<-500
HIV$HIVResult[HIV$HIVResult==1]<-as.character("POS")

#remove columns
TB<-TB[,c(2,4,5,9,11:15):=NULL]

#subset data to make it easier to work with
NKS<-subset(Final,Final$Study=="NKS")
TBRU<-subset(Final,Final$Study=="TRU")

## create new column with NA as value
TBRU[,"TB.Status"]<-NA

## for each row in the data table
## update new column with value based on other column
for (i in 1:nrow(TBRU)){
    if(TBRU$QualResult[i]=="1") {TBRU$TB.Status[i]<-as.character("LTBI")}
    else {TBRU$TB.Status[i]<-as.character("Healthy Control")}
}

for (i in 1:length(Final$HIV.Status)){
    Final$HIV.Status[i] <- ifelse(is.na(Final$HIVResult[i]), as.character(Final$HIV.STATUS[i]),
                                  as.character(Final$HIVResult[i]))
}

#Aggregate data from the same donor in a data set and perform some function on it
##this makes a new data frame with the StudyID and the aggregated data
A = aggregate(AscarisPositive ~ StudyID, data = HEL, function(x) {
    
    if (1 %in% x) {
        return(1);
    } else {
        return(0);
    }
});

#merge two data sets. match rows by study IDs 
##but keep all rows regardless of whether the StudyID is in both sets
I<-merge(Disease,TB,by="StudyID",all=TRUE)

#Check for duplicated IDs
dups<-Final$StudyID[duplicated(Final$StudyID)]
#NK2081 NK2194 NK2269 NK2464 NK2667 NK2712 80008  80019 

#find common StudyIDs and remove the non common StudyIDs from a data set
Inter<- intersect(HEL$StudyID,TB$StudyID)
CleanTB<-TB[TB$StudyID %in% Inter,];

#no clue figure these out
s<-split(Final, Final$HelmithPositive)
i<-interaction(Final$HelmithPositive, Final$QualResult)
S<-str(split(Final,list(Final$HelmithPositive, Final$QualResult), drop=TRUE))

#graph
qplot(test$SchistosomaIntensity,test$QFT, col=test$SchistosomaPositive.y)

#correlation test
# the [] provides a restriction on the data used in the correlation
cor.test((test$SchistosomaIntensity)[test$SchistosomaPositive==1],test$QFT[test$SchistosomaPositive==1])

# linear model
lm((test$SchistosomaIntensity)[test$SchistosomaPositive==1]~test$QFT[test$SchistosomaPositive==1])

#ANOVA
aov((test$SchistosomaIntensity)[test$SchistosomaPositive==1]~test$QFT[test$SchistosomaPositive==1])

###power for proportions
trans.arcsine <- function(x){
    asin(sign(x) * sqrt(abs(x)))
}
h=2*trans.arcsine(b/d)-2*trans.arcsine(a/c)
pwr.2p.test(h=h,n=NULL,sig.level=0.05,power=.8)

for (i in 1:length(Cyt$IFNg)) {
    Cyt$IFNg[i]<-ifelse(Cyt$Stim[i]=="UN",Cyt$IFNg[i],(Cyt$IFNg[i]-Cyt$IFNg[Cyt$Stim=="UN"]))
}