###I'm trying to write a for-loop that combines the information in columns 1-3
#such that if TB.1 is not NA then TB.Final takes that value
#if TB.1 is NA then TB.Final takes the value in TB.2
#if TB.2 is NA then TB.Final takes the value in TB.3

#this is just a fake data set since it's easier than finding one to use
TB.1<-c("LTBI", "LTBI", "LTBI", "LTBI", "LTBI", 
        "ACTIVE", "ACTIVE", "ACTIVE", "HEALTHY CONTROL", "HEALTHY CONTROL",
        NA,NA,NA,NA,NA)
TB.2<-c(NA, NA, "LTBI", "LTBI", "HEALTHY CONTROL", 
        "ACTIVE", "ACTIVE", NA, "HEALTHY CONTROL", "HEALTHY CONTROL",
        "HEALTHY CONTROL","ACTIVE","LTBI",NA,NA)
TB.3<-c(NA, "ACTIVE", NA, NA, "HEALTHY CONTROL", 
        NA, NA, NA,"HEALTHY CONTROL", NA,
        "HEALTHY CONTROL","ACTIVE","LTBI","LTBI", "HEALTHY CONTROL")
library(data.table)
TB<-as.data.table(cbind(TB.1,TB.2,TB.3))
TB[,"TB.Final"]<-NA

for (i in 1:length(TB$TB.Final)){ #go down every row of this data set
        if(!is.na(TB$TB.1[i])){ #If the first column you want to pull from is not NA
                TB$TB.Final[i] <- TB$TB.1[i]} #then pull the value in that row/column pair and stick it in the new column
        #if it is NA then go to this function
        else if(!is.na(TB$TB.2[i])){ #if the second column you trust is not NA
                TB$TB.Final[i] <- TB$TB.2[i]} #pull that value
        #if it is also NA then go to this function
        else{ #no if because you are out of options
                TB$TB.Final[i] <- TB$TB.3[i]} #pull the value from the third choice column and stick it in
}
