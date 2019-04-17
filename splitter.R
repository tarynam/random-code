#takes as arguments the datatable name, the column name and the character you want to split by
# such that NK2081_TB split by the "_" would give you NK2081 and TB
#one, two, three etc. are the new column names that receive the corresponding index from the split
#so one="Donor" would get NK2081 and two="TB" would get TB
#set to null so that if you only have 1 or 2 terms it doesn't matter 

splitter<-function(data,column,splitter,one=NULL,two=NULL,three=NULL,four=NULL,five=NULL){
    #import the package necessary to the function
    library(data.table)
    
    labels<-tstrsplit(data[,column], splitter) #tstrsplit takes a vector of character strings  
    #splits the each index of the vector by the split term (as many as there are)
    #returns a set of vectors where the first index is the term up to the first split term
    
    #These each function the same way
    if(!is.null(one)){data[,one]<-labels[[1]]} #If I indicate a column name (aka one is not Null)
    if(!is.null(two)){data[,two]<-labels[[2]]} #then create a new column in the table with the column name
    if(!is.null(three)){data[,three]<-labels[[3]]} #and fill it with the corresponding vector from the split
    if(!is.null(four)){data[,four]<-labels[[4]]}
    if(!is.null(five)){data[,five]<-labels[[5]]}
    
    return(data) #gives you back the data table with the new columns
}

####EXAMPLE
words<-as.matrix(c(rep("ba-zoo-ka", 6),rep("bub-ble-gum",6)))
test<-data.frame(cbind(LETTERS, words)) #names the column with our strings as V2
t1<-splitter(test, "V2", "-", one="first syllable")
t2<-splitter(test, "V2", "-", three="third syllable")

