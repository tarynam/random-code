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
