remove_samples<-function(datatable){
    library(dplyr) #load it in the function to overcome namespace issues
    #since the sample name is unique, you can make a list of the ones you want excluded
    excluded_samples<-c("NK2095_LTBI_X.SEA")
    #then you filter the datatable so that any row that has that sample name is removed
    datatable<-filter(datatable, !(Sample %in% excluded_samples))#data wildly unrelable in ICS
    datatable
}

remove_donors<-function(datatable){
    library(dplyr) #load it in the function to overcome namespace issues
    datatable<-filter(datatable, Donor!="NK2325") #No SM data in primary files
    datatable
}

changeTB<-function(datatable){
    datatable$TB[datatable$Donor=="NK2183"]<-as.character("HC")
    datatable$TB[datatable$Donor=="NK2358"]<-as.character("TB")
    datatable$SM[datatable$Donor=="NK2341"]<-as.character("X")
    datatable$SM[datatable$Donor=="NK2225"]<-as.character("X")
    datatable
}

clean_ognames<-function(datatable){
    datatable$Donor<-gsub("B", "", datatable$Donor)
    datatable$Donor<-gsub("T|TRU", "", datatable$Donor)
    datatable
}

cleanvariablenames<-function(datatable){
    newnames<-gsub("_freq", "", names(datatable))
    names(datatable)<-newnames
    datatable
}

split_og<-function(df){
    library(data.table)
    l<-tstrsplit(df$Sample, "_")
    df$Donor<-l[[1]]
    df$TB<-l[[2]]
    df$SM<-l[[3]]
    df$SM<-gsub(".PEP|.WCL|.SEB|.UN|.SEA|.SWAP", "", df$SM)
    df$Stim<-l[[3]]
    df$Stim<-gsub("X.|SM.", "", df$Stim)
    df
}

sum_boolean<-function(df){
    library(dplyr)
    df2<-dplyr::mutate(df,
            TbetCXCR3=  rowSums(df[,grep("t_._3_.", names(df))]),
            TbetCCR4=  rowSums(df[,grep("t_._._4", names(df))]),
            GATA3CXCR3=  rowSums(df[,grep("._g_3_.", names(df))]),
            GATA3CCR4=  rowSums(df[,grep("._g_._4", names(df))])
)
    df2
}

sum_boolean2<-function(df){
    library(dplyr)
    df2<-dplyr::mutate(df,
        CXCR3.Tbetprop = 100*TbetCXCR3/Tbet,
        CCR4.Tbetprop = 100*TbetCCR4/Tbet,
        CXCR3.GATA3prop= 100*GATA3CXCR3/GATA3,
        CCR4.GATA3prop= 100*GATA3CCR4/GATA3
    )
    df2
}

OG_background.subtract<-function(df, splitvar, keyvar, condition, col){
    library(dplyr)
    df<-arrange(df, df[,splitvar])
    dt<-split(df, df[, splitvar]) 
    dt.r<-lapply(dt, function(g)
            (g[,col] - subset(g, g[,keyvar]==condition)[,col]))
    df[,col]<-unlist(dt.r)
    df[df<0]<-0
    df
}
