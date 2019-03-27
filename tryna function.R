#takes as arguments the datatable name, the column name
#and the character you want to split by
#one, two, three and four are where you name the new column which receive the
#first-fourth set of terms after the split as the data
#set to null so that if you only have 1 or 2 terms it doesn't matter
splitter<-function(data,column,splitter,one=NULL,two=NULL,three=NULL,four=NULL){
        #import the package necessary to the function
        library(data.table)
        
        labels<-tstrsplit(data[,column], splitter)
        data[,one]<-labels[[1]]
        data[,two]<-labels[[2]]
        data[,three]<-labels[[3]]
        data[,four]<-labels[[4]]
        return(data) #gives you back the data table with the new columns
}

pvals<-function(data, column){
    
    #This whole first chunk is specific to my flow data so you may want to take it out
    data<-filter(data, SM!="N") 
    data$SM<-factor(data$SM, levels=c("X","SM"))
    data$TB<-factor(data$TB, levels=c("HC","LTBI","TB"))
    
    #Split the data by TB status so that within each level of TB you can compare 
    #SM- to SM+
    x<-split(data,data$TB)
    #does a wilcox test between SM- and SM+ for whatever column you call in the column argument
    A<-lapply(x, function(g) wilcox.test(g[,column]~g[,"SM"]))
    
    
    #this one is trickier because wilcox.test can only have 2 levels so you have to
    #split by SM and then subset in each wilcox.test to only include 2 levels of TB
    y<-split(data,data$SM) 
    B<-lapply(y, function(g) wilcox.test((g[,column]~g[,"TB"]), subset=g$TB %in% c("HC", "LTBI")))
    C<- lapply(y, function(g) wilcox.test((g[,column]~g[,"TB"]), subset=g$TB %in% c("LTBI", "TB")))
    D<- lapply(y, function(g) wilcox.test((g[,column]~g[,"TB"]), subset=g$TB %in% c("HC", "TB")))

    #extract the p value itself from each wilcox test
    pvals<-c(A$HC$p.value, A$LTBI$p.value, A$TB$p.value,
             B$SM$p.value, C$SM$p.value,  D$SM$p.value,
             B$X$p.value,  C$X$p.value,   D$X$p.value
    )
    #adjust the p values to account for multiple comparisons using 2 methods
    bonf_adj_pvals<-round(p.adjust(pvals, method="bonferroni"), 4)
    fdr_adj_pvals<-round(p.adjust(pvals, method="fdr"), 4)
    
    #create a set of labels so you know which p value belongs to which comparison
    labels<-c("HC SM+ to HC SM-", "LTBI SM+ to LTBI SM-", "TB SM+ to TB SM-",
              "HC SM+ to LTBI SM+", "LTBI SM+ to TB SM+", "HC SM+ to TB SM+",
              "HC SM- to LTBI SM-", "LTBI SM- to TB SM-", "HC SM- to TB SM-"
    )
    
    #bind them together into a table
    table<-(cbind(labels, round(pvals, 4), bonf_adj_pvals, fdr_adj_pvals))
    colnames(table)<-c("Comparison", 
                       paste(column,"p-values"," "), #so you know what column a table is from
                       "Bonferonni Corrected p-values", "FDR Corrected p-vales")
    table
}

base.plot <- function(DF, group1, group2, value) {
        p <- ggplot(DF, aes(x=group1, y=value, col=group1))
        p <- p + theme_bw()
        p <- p + theme(legend.position="bottom")
        p <- p + geom_jitter(width=.1,height=0, shape=1,size=2)
        p <- p + scale_color_brewer(palette="Set1", guide=guide_legend(ncol=6, title=NULL))
        p <- p + xlab("") + ylab("")
        return(p)
}

replace.btwn.tables<-function(datatable1, datatable2, id.var, ref.var, new.var, new.vals){
    library(dplyr)
    inter <- intersect(datatable1[,id.var], datatable2[,ref.var])
    tbl1a <- subset(datatable1, datatable1[,id.var] %in% inter)
    tbl1b <- subset(datatable1, !(datatable1[,id.var] %in% inter))
    tbl2  <- subset(datatable2, datatable2[,ref.var] %in% inter)
    tbl1a[,new.var] <- replace(x = tbl1a[,new.var],
                           list = (tbl1a[,id.var] %in% tbl2[,ref.var]),
                           values = as.character(tbl2[,new.vals]))
    new.table<-rbind(tbl1a, tbl1b)
}

class_change<-function(datatable){
    y<-dplyr::select(datatable, Sample, Donor, TB, SM, Stim)
    x<-dplyr::select(datatable, -Sample, -Donor, -TB, -SM, -Stim)
    for(i in 1:length(x)){x[[i]]<-as.numeric(as.character(x[[i]]))}
    df<-cbind(y, x)
}

numeric.only <- function(X,...){
    returnCols <- names(X)
    a<-sapply(X, is.numeric)
    print(returnCols[a == "TRUE"])
}

# All of this generates a fake data table that contains a "key" variable (i.e. donor)
# a condition variable (i.e. "stim") and a set of columns of output data
# the goal is for every donor, in each column of output data subtract out the value in that column
# that corresponds to a specific value in the condition column
donor<-c(rep("john", 6), rep("mary", 6), rep("jack", 6), rep("suzy", 6))
stim<-rep(c("un","w","p","s","sw","sb"),4)
ifng<-rnorm(24, 2)
tnfa<-rnorm(24,10)
IL4<-rnorm(24,5)
IL5<-rnorm(24,7)
IL10<-rnorm(24,13)
IL13<-rnorm(24,19)
IL17<-rnorm(24,17)
IL21<-rnorm(24,11)
IL22<-rnorm(24,3)

fake<-data.frame(cbind(donor,stim,ifng,tnfa,IL4, IL5, IL10, IL13, IL17, IL21, IL22))
remove(donor,stim,ifng,tnfa,IL4, IL5, IL10, IL13, IL17, IL21, IL22)
#can only use data frames to use bracketed string column indexing so make sure this is a data frame not a data table

#when binding data together the output data becomes facgtors and we need them to be numeric
fake<-class_change(fake)
#we want to copy the original data table to reference back to and check our math
fakecopy<-fake

#This is going to generate the list of columns names that we want to iterate over
numeric.only <- function(X,...){
    returnCols <- names(X)
    a<-sapply(X, is.numeric)
    print(returnCols[a == "TRUE"])
}


bazooka<-numeric.only(fake)
fake<-arrange(fake, donor)


subtractun<-function(datatable, splitvar, keyvar, condition){
    dt<-split(datatable, datatable[, splitvar]) #generate a list of donor specific mini data frames 
    #split --> lists in alphabetical order not by original key order
    #To solve this I arranged the original data frame in alphabetical order
    #can we instead arrange the split to match the original order

    #for each column name in the list of column names
    for(col in colnms){
        #this is a check to make sure it goes through all the columns
        print(col) 
        #dt.r will make a list of length of dt where each item in the list is the new column for a particular donor
        dt.r<-lapply(dt, function(g)
        #take the values in each column and subtract out the value indicated by an "un" the stim column 
        (g[,col] - subset(g, g[,keyvar]==condition)[,col]))
        #unlist the list so you get a single column and insert it back into the original data frame
        datatable[,col]<-unlist(dt.r)
    }
    datatable
}


