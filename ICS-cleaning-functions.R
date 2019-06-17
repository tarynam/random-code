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
    excluded_samples<-c("NK2126_HC_X_SEA", 
                        "NK2450_HC_SM_SEA" , 
                        "NK2402_LTBI_SM_SWAP",
                        "NK2342_TB_X_SWAP",
                        "NK2136_TB_X_WCL",
                        "NK2168_HC_SM_WCL",
                        "NK2421_LTBI_SM_PEP",
                        "NK2063_TB_X_SEA",
                        "NK2115_LTBI_X_PEP",
                        "NK2115_LTBI_X_WCL",
                        "NK2171_HC_X_WCL",
                        "NK2162_HC_X_SEA",
                        "HD382_N_N_SEA")
    #then you filter the datatable so that any row that has that sample name is removed
    datatable<-filter(datatable, !(Sample %in% excluded_samples))%>%
        filter(Donor!="NK2586") #data wildly unrelable in ICS
    datatable
}

remove_donors<-function(datatable){
    library(dplyr) #load it in the function to overcome namespace issues
    datatable<-filter(datatable, Donor!="NK2325") #No SM data in primary files
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
    datatable$SM[datatable$Donor=="NK2341"]<-as.character("X")
    datatable$SM[datatable$Donor=="NK2225"]<-as.character("X")
    datatable
}

changeHD<-function(datatable){
    datatable$Donor[datatable$Donor=="HD474"]<-as.character("HD454")
    datatable$Donor[datatable$Donor=="HD448"]<-as.character("HD488")
    datatable
}



