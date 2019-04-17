#These are two functions that allow you to replace specific cells in one table with the values in another table
#for example if a handful of values are missing in one table or you have to update some information
# datatable1 is the table you want to replace the values in 
# from values in datatable 2
# id.1 and id.2 are the names of the columns in table 1 and table 2 that contain shared ID information 
    #like donor/patient/sample #doesn't have to have the same column name... just needs to be the linking variable
# new.var is the name of the column in table 1 that you want to replace values in
# new.vals is the name of the column you will be taking values from in table 2

replace.btwn.tables<-function(datatable1, datatable2, id.1, id.2, new.var, new.vals){
    library(dplyr) #to overwrite namespace issues
    
    #finds the intersection between the two tables based on a shared ID variable
    inter <- intersect(datatable1[,id.1], datatable2[,id.2]) 
    
    #this subsets both data tables to include only the rows that contain the shared IDs
    tbl1a <- subset(datatable1, datatable1[,id.1] %in% inter)
    tbl1a <- arrange(tbl1a, tbl1a[,id.1])
    tbl2  <- subset(datatable2, datatable2[,id.2] %in% inter)
    tbl2 <- arrange(tbl2, tbl2[,id.2])
    

    tbl1a[,new.var] <- replace(x = tbl1a[,new.var], #replace the column you want in table 1
                               list = (tbl1a[,id.1] %in% tbl2[,id.2]), #link id variables in both tables
                               values = as.character(tbl2[,new.vals])) #with these values from table 2
    
    # grabs the remaining non adjusted rows fromt he original table 1
    tbl1b <- subset(datatable1, !(datatable1[,id.1] %in% inter))
    
    #binds the two parts of table 1 together
    new.table<-rbind(tbl1a, tbl1b)
}

#EXAMPLE
correct<-mtcars
correct$carname<-row.names(mtcars)
incorrect<-correct
incorrect[3:9,4]<-NA
test1<-replace.btwn.tables(incorrect, correct, "carname", "carname", "hp", "hp")

#doesn't have to have the same names
incorrect<-dplyr::rename(incorrect, label=carname, horse=hp)
test2<-replace.btwn.tables(incorrect, correct, "label", "carname", "horse", "hp")

#even works when the tables are in different orders
incorrect<-arrange(incorrect, cyl)
test3<-replace.btwn.tables(incorrect, correct, "label", "carname", "horse", "hp")


