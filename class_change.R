#so far it only works for my data because I specify but looking to make it generic

class_change<-function(datatable){
    #this just splits the input data table into 2 tables
    #one (y) will have all my variables that I want as factors/characters
    #the other (x) will have all the ones I want as numeric
    y<-dplyr::select(datatable, Sample, Donor, TB, SM, Stim)
    x<-dplyr::select(datatable, -Sample, -Donor, -TB, -SM, -Stim)
    #this loops through all the columns in x and makes them numeric
    #have to specificy as.character or else you will get the number of the factor level
    for(i in 1:length(x)){x[[i]]<-as.numeric(as.character(x[[i]]))}
    #then you stitch them back together and return the data table
    df<-cbind(y, x)
}