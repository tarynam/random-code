#this fixes export issues that arise from typos, filename issues, extra unecessary lines and weird symbols
clean_flowjo_output<-function(datatable){
    library(dplyr)
    #each sample line in flowjo has a numeric tag and it's unnecessary 
    ##--> gsub finds all of them and replaces them with blank space
    datatable$Sample<-gsub("1: |2: |3: |4: |5: |6: ", "", datatable$Sample) 
    datatable$Sample<-gsub("001_", "", datatable$Sample) 
    ##this fixes typos so that the _ is present in all sample names so the splitter function works
    datatable$Sample<-gsub("XUN", "X_UN", datatable$Sample)
    datatable$Sample<-gsub("XPMA", "X_PMA", datatable$Sample)
    datatable$Sample<-gsub("XWCL", "X_WCL", datatable$Sample)
    datatable$Sample<-gsub("XPEP", "X_PEP", datatable$Sample)
    datatable$Sample<-gsub("XSEA", "X_SEA", datatable$Sample)
    datatable$Sample<-gsub("XSWAP", "X_SWAP", datatable$Sample)
    #again fixing typos where I accidentally had spaces in the filenames
    datatable$Sample<-gsub("_ ", "_", datatable$Sample)
    datatable$Sample<-gsub(" _", "_", datatable$Sample)
    #flowjo exports the mean and Std for each individual workspace which we don't need
    #we also don't need the column names which is what the rows that say "Sample" in the Sample column have
    datatable<-filter(datatable,!(Sample=="Mean"))%>%
        filter(!Sample=="StdDev")%>%
        filter(!Sample=="Sample")
    #I think this symbol arises when there aren't any cells to create a summary statistic in flowjo
    #so basically it's an NA
    datatable[datatable=="¥"]<-NA
    datatable[datatable=="•"]<-NA
    #now return the datatable without all that crap
    datatable
    
}

#this is an easy way to make sure all the samples that Cheryl and I have decided to exclude get excluded from 
#alllll the files. It also allows me to keep a running list of excluded samples
remove_samples<-function(datatable){
    library(dplyr) #load it in the function to overcome namespace issues
    #since the sample name is unique, you can make a list of the ones you want excluded
    excluded_samples<-c("NK2126_HC_X_SEA_005.fcs", 
                        "NK2450_HC_SM_SEA_047.fcs" , 
                        "NK2402_LTBI_SM_SWAP_024.fcs",
                        "NK2342_TB_X_SWAP_006.fcs",
                        "NK2136_TB_X_WCL_003.fcs",
                        "NK2168_HC_SM_WCL_040.fcs",
                        "NK2421_LTBI_SM_PEP_010.fcs",
                        "NK2063_TB_X_SEA_005.fcs",
                        "NK2115_LTBI_X_PEP_010.fcs",
                        "NK2115_LTBI_X_WCL_009.fcs",
                        "NK2171_HC_X_WCL_009.fcs",
                        "NK2162_HC_X_SEA_023.fcs")
    #then you filter the datatable so that any row that has that sample name is removed
    datatable<-filter(datatable, !(Sample %in% excluded_samples))%>%
        filter(Donor!="NK2325")
    datatable
}

fix.names<-function(datatable){
    datatable$Sample<-paste(
        tstrsplit(datatable$Sample,"_")[[1]],
        tstrsplit(datatable$Sample,"_")[[2]],
        tstrsplit(datatable$Sample,"_")[[3]],
        tstrsplit(datatable$Sample,"_")[[4]], 
        sep="_")
    datatable
}


changeTB<-function(datatable){
    datatable$TB[datatable$Donor=="NK2183"]<-as.character("HC")
    datatable
}

