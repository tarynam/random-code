numeric_only <- function(X,...){
    returnCols <- names(X) #gets the columns names of a datatable
    a<-sapply(X, is.numeric) #applies over the columns indicated by X the function is.numeric which is T/F
    print(returnCols[a == "TRUE"]) #prints the subset of columns names where the index of a has TRUE
}
