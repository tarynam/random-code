install.packages("dplyr")
install.packages("data.table")


#used to generate a list of character strings that are the names of the columns containing numeric data
numeric.only <- function(X,...){
    returnCols <- names(X)
    a<-sapply(X, is.numeric)
    print(returnCols[a == "TRUE"])
} #explained elsewhere

#takes a dataframe (v. important its a dataframe), 
#splitvar is a character string of a column name that you split your data table by (aka donor)
#keyvar is the name of column that contains the value that determines which row gets subtracted (aka stim)
#condition is a character string with the key to indicate what value in keyvar is being subtracted (aka unstim)

background.subtract<-function(df, splitvar, keyvar, condition){
    library(dplyr)
    df<-arrange(df, df[,splitvar]) #arrange the data frame in alphabetical order by splitvar because split returns
    #a list that is sorted alphabetically
    dt<-split(df, df[, splitvar]) #generate a list of mini data frames for each level of splitvar
    
    colnms<-numeric.only(df) ## **********YOU WILL NEED TO CHAGNE THIS FOR REAL DATA ******
    
    #colnms is a list of character strings indicating which columns to subtract background from
    #here I use a function to give me all the names of numeric columns but I could also specify
    #for each column name in the list of column names
    for(col in colnms){
        #this is a check to make sure it goes through all the columns
        print(col) 
        #dt.r will make a list of length of dt where each item in the list is the new column for a particular donor
        dt.r<-lapply(dt, function(g)
            #take the values in each column and subtract out the value indicated by an "un" the stim column 
            (g[,col] - subset(g, g[,keyvar]==condition)[,col]))
        #unlist the list so you get a single column and insert it back into the original data frame
        df[,col]<-unlist(dt.r)
    }
    df
}


###EXAMPLE
#generate fake data
donor<-c(rep("john", 6), rep("mary", 6), rep("jack", 6), rep("suzy", 6))
stim<-rep(c("un","w","p","s","sw","sb"),4)
luminex<-data.frame(cbind(donor, stim))
luminex$ifng<-sample(0:1000, 24)
luminex$il2<-sample(0:500, 24)
luminex$tnfa<-sample(0:2000, 24)
luminex$il13<-sample(0:5000, 24)

test<-background.subtract(luminex, "donor", "stim", "un")       
       
       
       