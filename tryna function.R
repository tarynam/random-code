#documentation and examples for these functions are in their individual script files

class_change<-function(datatable){
    y<-dplyr::select(datatable, Sample, Donor, TB, SM, Stim)
    x<-dplyr::select(datatable, -Sample, -Donor, -TB, -SM, -Stim)
    for(i in 1:length(x)){x[[i]]<-as.numeric(as.character(x[[i]]))}
    df<-cbind(y, x)
}

numeric_only <- function(X,...){
    returnCols <- names(X) #gets the columns names of a datatable
    a<-sapply(X, is.numeric) #applies over the columns indicated by X the function is.numeric which is T/F
    print(returnCols[a == "TRUE"]) #prints the subset of columns names where the index of a has TRUE
}

replace.btwn.tables<-function(datatable1, datatable2, id.1, id.2, new.var, new.vals){
    library(dplyr) #to overwrite namespace issues
    inter <- intersect(datatable1[,id.1], datatable2[,id.2]) 
    
    tbl1a <- subset(datatable1, datatable1[,id.1] %in% inter)
    tbl1a <- arrange(tbl1a, tbl1a[,id.1])
    tbl2  <- subset(datatable2, datatable2[,id.2] %in% inter)
    tbl2 <- arrange(tbl2, tbl2[,id.2])
    
    
    tbl1a[,new.var] <- replace(x = tbl1a[,new.var], #replace the column you want in table 1
                               list = (tbl1a[,id.1] %in% tbl2[,id.2]), #link id variables in both tables
                               values = as.character(tbl2[,new.vals])) #with these values from table 2
    
    tbl1b <- subset(datatable1, !(datatable1[,id.1] %in% inter))

    new.table<-rbind(tbl1a, tbl1b)
}

splitter<-function(data,column,splitter,one=NULL,two=NULL,three=NULL,four=NULL,five=NULL){
    library(data.table)
    
    labels<-tstrsplit(data[,column], splitter)
    if(!is.null(one)){data[,one]<-labels[[1]]} #If I indicate a column name (aka one is not Null)
    if(!is.null(two)){data[,two]<-labels[[2]]} #then create a new column in the table with the column name
    if(!is.null(three)){data[,three]<-labels[[3]]} #and fill it with the corresponding vector from the split
    if(!is.null(four)){data[,four]<-labels[[4]]}
    if(!is.null(five)){data[,five]<-labels[[5]]}
    
    return(data) #gives you back the data table with the new columns
}

background.subtract<-function(df, splitvar, keyvar, condition){
    library(dplyr)
    df<-arrange(df, df[,splitvar])
    dt<-split(df, df[, splitvar]) 
    
    colnms<-numeric_only(df)
    
    for(col in colnms){
        print(col) 
        dt.r<-lapply(dt, function(g)
            (g[,col] - subset(g, g[,keyvar]==condition)[,col]))
        df[,col]<-unlist(dt.r)
    }
    df
}