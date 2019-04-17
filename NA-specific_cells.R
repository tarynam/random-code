#This generates a fake data set to play with
names<-c("ID1","ID2","ID3", "ID4", "ID5", "ID6", "ID7", "ID8", "ID9", "ID10")
data<-matrix(data=rep(1:8, 5), nrow=10)
data<-as.data.table(cbind(names,data))
names(data)<-c("ID","CXCR3_MFI","blank","blank2","CXCR3_Freq")

#Create a list of column names that you are interested in by using grep along the names of the data set
important_cols = grep('CXCR3', names(data)) #this just gives you a set of column indexes
for (i in 1:length(data)){ #for every row in the data set
        if(data$ID[i] %in% c("ID1", "ID4", "ID5")){ #if the value in the ID column is in this list
                for(j in important_cols){ #go through all the columns indexed by important_cols
                        data[i,j] <- NA; #and make that row column pair NA
                }        
        }
}


