# Lines 1-37 generate two dataframes: we want to make certain cells in fake_mfi NA based on a corresponding 
# condition in fake_count which we link using a set of "keys" which are the same in both tables
# here the "keys" in fake_count are TH1 and TH2
# the corresponding columns in fake_mfi start wtih TH1 and TH2
# for every donor/stim combo where a count is below a threshold in fake_count, 
# we want to make the corresponding donor/stim combo NA for the columns linked to that count
    donor<-c(rep("john", 6), rep("mary", 6), rep("jack", 6), rep("suzy", 6))
    stim<-rep(c("un","w","p","s","sw","sb"),4)
    TH1<-sample(20:200, 24)
    TH2<-sample(20:200, 24)
    
    TH1_ifng<-sample(100:1000, 24)
    TH1_tnfa<-sample(100:1000, 24)
    TH1_IL4<-sample(100:1000, 24)
    TH1_IL13<-sample(100:1000, 24)
    TH2_ifng<-sample(100:1000, 24)
    TH2_tnfa<-sample(100:1000, 24)
    TH2_IL4<-sample(100:1000, 24)
    TH2_IL13<-sample(100:1000, 24)
    
    fake_count<-data.frame(cbind(donor,stim, TH1, TH2))
    fake_mfi<-data.frame(cbind(donor,stim,
                               TH1_ifng,TH1_tnfa,TH1_IL4,TH1_IL13,
                               TH2_ifng,TH2_tnfa,TH2_IL4,TH2_IL13))
    remove(donor, stim, TH1, TH2,TH1_ifng,TH1_tnfa, TH1_IL4,TH1_IL13,TH2_ifng,TH2_tnfa,TH2_IL4,TH2_IL13)
    #can only use data frames to use bracketed string column indexing so make sure this is a data frame not a data table
    
    #when binding data together the output data becomes facgtors and we need them to be numeric
    #this function is described elsewhere so I won't go through it
    class_change<-function(datatable){
        y<-dplyr::select(datatable, donor, stim)
        x<-dplyr::select(datatable, -donor, -stim)
        for(i in 1:length(x)){x[[i]]<-as.numeric(as.character(x[[i]]))}
        df<-cbind(y, x)
    }
    fake_count<-class_change(fake_count)
    fake_mfi<-class_change(fake_mfi)

###BRUTE FORCE RESHAPE MERGE
#works but you end up with a new different shape datatable
    melt<-melt(fake_count, id.vars=c("donor","stim")) #turns the data table long format so each subset/count combo is a row
    melt<-dplyr::rename(melt, count=value, cell_subset=variable) #renames so it's easier to follow later
    melt_mfi<-melt(fake_mfi, id.vars=c("donor","stim")) #again long format table so each 
    splitter<-function(data,column,splitter,one=NULL,two=NULL){
        library(data.table)
        
        labels<-tstrsplit(data[,column], splitter)
        data[,one]<-labels[[1]]
        data[,two]<-labels[[2]]
        return(data)
    } #this function is described elsewhere
    
    #these two rows split the variable column into two separate columns and removes the og variable column
    melt_mfi<-splitter(melt_mfi, "variable", "_", one="cell_subset", two="attribute") 
    melt_mfi<-select(melt_mfi, donor, stim, cell_subset, attribute, value)
    
    #full_join merges the two tables based on all shared columns giving us a new long format table
    #each donor/stim/cell subset combo has a count in that cell subset, an attribute to be measured, and the measurement
    # there are 4 attributes for every cell subset
    test<-full_join(melt, melt_mfi)
    for(r in 1:nrow(test)){ #for every row of the new long format data
        if(test$count[r]<50){ #if the count is below a threshold
            test$value[r]<-NA #make the corresponding measurement NA
        }
    }
    test2<-tidyr::spread(test, attribute,value) #spread just reformats the data to be less long format

###FOR LOOP
    #for this we need a single column that is a unique row identifier
    fake_count$Sample<-paste(fake_count$donor, fake_count$stim)
    fake_mfi$Sample <- paste(fake_mfi$donor, fake_mfi$stim)
    #I'm also going to duplicate the fake_mfi so we can compare at the end
    DF<-fake_mfi
    
    keynames<-c("TH1", "TH2") #create a list with all your "keys" can specificy them or use colnames command
        for(key in keynames){ #this indexes across a list of character strings
            print(key) #for each key string so you can see it working
            remove<-subset(fake_count, fake_count[,key]<50)$Sample #subset the count data where the key column value <20
            #and return a list of the Sample IDs for that subset
            print(length(remove)) #print the length of the list
            
            #This is where it gets hard to read but I'm just using normal index notation data[row, column]
            #the rows are any row where the Sample ID is in the list we generated called "remove"
            #the columns are any column that contains the key tagged with a "_" in the name
            #then I set all those row column pairs to NA
            DF[ (DF$Sample %in% remove) , grep(paste(key,"_",sep=""), names(DF)) ]<-NA 
            
        }
    
 ##CHECK THE FOR LOOP
    #for me
    for(key in keynames){
        print(key)
        #bind together the columns whose names contain the key word
        #from the reference table, the original data table and the new NA data table together
        test<-data.frame(cbind(fake_count[grep(key, names(fake_count))], 
                               fake_mfi[grep(paste(key,"_",sep=""), names(fake_mfi))],
                               DF[grep(paste(key,"_",sep=""), names(DF))]))
        print(length(which(test[,1]<50 #see how many rows have counts under our threshold in the reference table
                           &! is.na(test[,2:5])))) #and still have numbers in the original table
        print(length(which(test[,1]<50 &! is.na(test[,6:9])))) #but are NA in the new table
    }
