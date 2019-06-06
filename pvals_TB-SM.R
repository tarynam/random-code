pvals_6<-function(data, column){
    library(dplyr)
    library(knitr)
    #This whole first chunk is specific to my flow data so you may want to take it out
    data<-filter(data, SM!="N") 
    data$SM<-factor(data$SM, levels=c("X","SM"))
    data$TB<-factor(data$TB, levels=c("HC","LTBI","TB"))
    
    #Split the data by TB status so that within each level of TB you can compare 
    #SM- to SM+
    x<-split(data,data$TB)
    #does a wilcox test between SM- and SM+ for whatever column you call in the column argument
    A<-lapply(x, function(g) wilcox.test(g[,column]~g[,"SM"], exact=FALSE))
    
    
    #this one is trickier because wilcox.test can only have 2 levels so you have to
    #split by SM and then subset in each wilcox.test to only include 2 levels of TB
    y<-split(data,data$SM) 
    C<- lapply(y, function(g) wilcox.test((g[,column]~g[,"TB"]), subset=g$TB %in% c("LTBI", "TB"), exact=FALSE))

    #extract the p value itself from each wilcox test
    pvals<-c(A$HC$p.value, A$LTBI$p.value, A$TB$p.value,
             C$SM$p.value, C$X$p.value
    )
    #adjust the p values to account for multiple comparisons using 2 methods
    bonf_adj_pvals<-c("NA", round(p.adjust(pvals[2:5], method="bonferroni"), 4))
    fdr_adj_pvals<-c("NA", round(p.adjust(pvals[2:5], method="fdr"), 4))
    
    #create a set of labels so you know which p value belongs to which comparison
    labels<-c("HC SM+ to HC SM-", "LTBI SM+ to LTBI SM-", "TB SM+ to TB SM-",
              "LTBI SM+ to TB SM+","LTBI SM- to TB SM-"
    )
    
    #bind them together into a table
    table<-data.frame((cbind(round(pvals, 4), fdr_adj_pvals, bonf_adj_pvals)), row.names=labels)
    table$V1<-as.numeric(as.character(table$V1))
    table$fdr_adj_pvals<-as.numeric(as.character(table$fdr_adj_pvals))
    table$bonf_adj_pvals<-as.numeric(as.character(table$bonf_adj_pvals))
    table<-subset(table, table$V1<0.05)
    colnames(table)<-c("p-value","FDR Corrected", "Bonferroni")
    table
    }

pvals_4<-function(data, column){
    library(dplyr)
    library(knitr)
    #This whole first chunk is specific to my flow data so you may want to take it out
    data<-filter(data, TB!="N", TB!="HC") 
    data$SM<-factor(data$SM, levels=c("X","SM"))
    data$TB<-factor(data$TB, levels=c("LTBI","TB"))
    
    #Split the data by TB status so that within each level of TB you can compare 
    #SM- to SM+
    x<-split(data,data$TB)
    #does a wilcox test between SM- and SM+ for whatever column you call in the column argument
    A<-lapply(x, function(g) wilcox.test(g[,column]~g[,"SM"]))
    
    
    #this one is trickier because wilcox.test can only have 2 levels so you have to
    #split by SM and then subset in each wilcox.test to only include 2 levels of TB
    y<-split(data,data$SM) 
    C<- lapply(y, function(g) wilcox.test(g[,column]~g[,"TB"]))
    
    #extract the p value itself from each wilcox test
    pvals<-c(A$LTBI$p.value, A$TB$p.value,
             C$SM$p.value, C$X$p.value
    )
    #adjust the p values to account for multiple comparisons using 2 methods
    bonf_adj_pvals<-(round(p.adjust(pvals, method="bonferroni"), 4))
    fdr_adj_pvals<-(round(p.adjust(pvals, method="fdr"), 4))
    
    #create a set of labels so you know which p value belongs to which comparison
    labels<-c("LTBI SM+ to LTBI SM-", "TB SM+ to TB SM-",
              "LTBI SM+ to TB SM+","LTBI SM- to TB SM-"
    )
    
    #bind them together into a table
    table<-data.frame((cbind(round(pvals, 4), fdr_adj_pvals, bonf_adj_pvals)), row.names=labels)
    table$V1<-as.numeric(as.character(table$V1))
    table$fdr_adj_pvals<-as.numeric(as.character(table$fdr_adj_pvals))
    table$bonf_adj_pvals<-as.numeric(as.character(table$bonf_adj_pvals))
    table<-subset(table, table$V1<0.05)
    colnames(table)<-c("p-value","FDR Corrected", "Bonferroni")
    table
}
