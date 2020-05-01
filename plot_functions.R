library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(data.table)
library(reshape2)
library(knitr)
library(cowplot)

mutate_TB<-function(df){
    df$disease<-paste(df$TB, df$SM, sep=" ")
    df$disease<-factor(df$disease, levels=c("HC X", "HC SM", "LTBI X", "LTBI SM", "TB X", "TB SM"))
    df$TB<-gsub("LTBI", "QFT+", df$TB)
    df<-filter(df, SM!="N", TB!="HC") 
    df$SM<-factor(df$SM, levels=c("X","SM"))
    df$TB<-factor(df$TB, levels=c("QFT+","TB"))
    df
}

filter_tb<-function(df){
    df$SM<-gsub("SM", "SM+", df$SM)
    df$SM<-gsub("X", "SM-", df$SM)
    df<-filter(df, SM!="N", TB=="TB")
    df$SM<-factor(df$SM, levels=c("SM-","SM+"))
    df$TB<-factor(df$TB, levels=c("TB"))
    df$disease<-paste(df$TB, df$SM, sep=" ")
    df$disease<-factor(df$disease, levels=c("TB SM-", "TB SM+"))
    names(df)<-gsub("Tbet", "T-bet", names(df))
    df
}

filter_ltbi<-function(df){
    df$SM<-gsub("SM", "SM+", df$SM)
    df$SM<-gsub("X", "SM-", df$SM)
    df<-filter(df, SM!="N", TB=="LTBI")
    df$SM<-factor(df$SM, levels=c("SM-","SM+"))
    df$disease<-paste(df$TB, df$SM, sep=" ")
    df$disease<-factor(df$disease, levels=c("LTBI SM-", "LTBI SM+"))
    names(df)<-gsub("Tbet", "T-bet", names(df))
    df
} 

filter_hc<-function(df){
    df$SM<-gsub("SM", "SM+", df$SM)
    df$SM<-gsub("X", "SM-", df$SM)
    df<-filter(df, SM!="N", TB=="HC")
    df$SM<-factor(df$SM, levels=c("SM-","SM+"))
    df$disease<-paste(df$TB, df$SM, sep=" ")
    df$disease<-factor(df$disease, levels=c("HC SM-", "HC SM+"))
    names(df)<-gsub("Tbet", "T-bet", names(df))
    df
}

plot_filter<-function(datatable){
    library(dplyr)
    df<-filter(datatable, TB!="N")
    df$TB<-factor(df$TB, levels=c("HC","LTBI","TB"))
    df$SM<-gsub("SM", "SM+", df$SM)
    df$SM<-gsub("X", "SM-", df$SM)
    df$SM<-factor(df$SM, levels=c("SM-","SM+"))
    df$disease<-paste(df$TB, df$SM, sep=" ")
    df$disease<-factor(df$disease, levels=c("HC SM-", "HC SM+",
                                            "LTBI SM-", "LTBI SM+",
                                            "TB SM-", "TB SM+"))
    names(df)<-gsub("Tbet", "T-bet", names(df))
    df
}


filter_all<-function(datatable){
    library(dplyr)
    df<-datatable
    df$TB<-factor(df$TB, levels=c("N","HC","LTBI","TB"))
    df$SM<-gsub("SM", "SM+", df$SM)
    df$SM<-gsub("X", "SM-", df$SM)
    df$SM<-factor(df$SM, levels=c("N","SM-","SM+"))
    df$disease<-paste(df$TB, df$SM, sep=" ")
    df$disease<-gsub("N N", "Naive", df$disease)
    df$disease<-factor(df$disease, levels=c("Naive","HC SM-", "HC SM+",
                                            "LTBI SM-", "LTBI SM+",
                                            "TB SM-", "TB SM+"))
    names(df)<-gsub("Tbet", "T-bet", names(df))
    df
}

plot_compass<-function(DF, yval){
    library(dplyr)
    DF$disease<-paste(DF$TB, DF$SM, sep=" ")
    DF$disease<-factor(DF$disease, levels=c("HC X", "HC SM", "LTBI X", "LTBI SM", "TB X", "TB SM"))
    
    g<-ggplot(DF, aes(x=disease, y=DF[,yval]))
    g+  geom_boxplot(aes(fill=TB, alpha=SM), size=1, position=position_dodge(width = 1), outlier.shape=NA)+
        geom_jitter(width=.1,height=0, shape=16,size=2)+
        scale_alpha_manual(values=c(0.5,1))+
        scale_fill_manual(values = c("#1a9850" , "#2166ac", "#b2182b"))+
        theme_classic()+ theme(legend.position="none")+
        theme(text = element_text(size=20), axis.text.x = element_text(angle=90, vjust=0.6)) + 
        theme(plot.title = element_text(hjust = 0.5)) +
        stat_compare_means(comparisons = my_comparisons, p.adjust="bonferroni")
}

plot_with_all_stats<-function(DF, yval){
    
    DF<-dplyr::filter(DF, TB != "N")
    DF$disease<-paste(DF$TB, DF$SM, sep=" ")
    DF$disease<-factor(DF$disease, levels=c("HC X", "HC SM", "LTBI X", "LTBI SM", "TB X", "TB SM"))
    
    my_comparisons <- list(c("HC SM", "HC X"),c("LTBI SM", "LTBI X"), c("TB SM", "TB X"), 
                           c("TB SM", "LTBI SM"), c("TB X", "LTBI X"),
                           c("HC SM", "LTBI SM"), c("HC X", "LTBI X"))        
   
    g<-ggplot(DF, aes(x=disease, y=DF[,yval]))
    g+  geom_boxplot(aes(fill=TB, alpha=SM), size=1, position=position_dodge(width = 1), outlier.shape=NA)+
        geom_jitter(width=.1,height=0, shape=16,size=2)+
        scale_alpha_manual(values=c(0.5,1))+
        scale_fill_manual(values = c("#1a9850" , "#2166ac", "#b2182b"))+
        theme_classic()+ theme(legend.position="none")+
        theme(text = element_text(size=16)) + 
        theme(plot.subtitle = element_text(hjust = 0.5)) +
        stat_compare_means(comparisons = my_comparisons, p.adjust="bonferroni")
}

plot_3<-function(DF, yval){
    
    DF<-dplyr::filter(DF, TB != "N")
    
    my_comparisons <- list(c("HC","LTBI"), 
                           c("TB", "LTBI"), 
                           c("HC", "TB"))      
    
    g<-ggplot(DF, aes(x=TB, y=DF[,yval]))
    g+  geom_boxplot(aes(fill=TB), size=1, position=position_dodge(width = 1), outlier.shape=NA)+
        geom_jitter(width=.1,height=0, shape=16,size=2)+
        scale_alpha_manual(values=c(0.5,1))+
        scale_fill_manual(values = c("#1a9850" , "#2166ac", "#b2182b"))+
        theme_classic()+ theme(legend.position="none")+
        theme(text = element_text(size=16)) + 
        theme(plot.subtitle = element_text(hjust = 0.5)) +
        stat_compare_means(comparisons = my_comparisons, p.adjust="bonferroni")
}

plot_3<-function(DF, yval, xval){

    my_comparisons <- list(c("N", "HC"),
                           c("N", "LTBI"),
                           c("N", "TB"),
                           c("HC","LTBI"), 
                           c("TB", "LTBI"), 
                           c("HC", "TB"))      
    
    g<-ggplot(DF, aes(x=DF[,xval], y=DF[,yval]))
    g+  geom_boxplot(aes(fill=DF[,xval]), size=1, position=position_dodge(width = 1), outlier.shape=NA)+
        geom_jitter(width=.1,height=0, shape=16,size=2)+
        scale_alpha_manual(values=c(0.5,1))+
        scale_fill_manual(values =c("N" = "#da70d6","HC" = "#00AFBB", "LTBI" = "#E7B800","TB"="#FC4E07",
                                    "X" = "#00AFBB", "SM" = "#E7B800"))+
        theme_classic()+ theme(legend.position="none")+
        theme(text = element_text(size=16)) + 
        theme(plot.subtitle = element_text(hjust = 0.5)) +
        stat_compare_means(comparisons = my_comparisons)
}


plot_cell<-function(DF, yval){
    
    my_comparisons <- list(c("CD4","CD8"), 
                           c("CD8", "GD"), 
                           c("CD4", "GD"))      
    
    g<-ggplot(DF, aes(x=celltype, y=DF[,yval]))
    g+  geom_boxplot(aes(fill=TB), size=1, position=position_dodge(width = 1), outlier.shape=NA)+
        geom_jitter(width=.1,height=0, shape=16,size=2)+
        scale_fill_manual(values =c("N" = "#da70d6","HC" = "#00AFBB", "LTBI" = "#E7B800","TB"="#FC4E07"))+
        theme_classic()+ theme(legend.position="none")+
        theme(text = element_text(size=16)) + 
        theme(plot.subtitle = element_text(hjust = 0.5)) +
        stat_compare_means(comparisons = my_comparisons, label="p.signif")
}

triple_boolean_plot<-function(datatable){
    library(dplyr)
    DF<-datatable
    Boolean<-dplyr::select(DF, Donor, TB, SM, Stim, G_4_13_T, G_4_x_T, G_x_13_T, G_4_13_x, x_4_13_T, G_4_x_x, G_x_13_x, x_4_x_T, x_x_13_T, G_x_x_T , G_x_x_x, x_x_x_T,  x_4_13_x, x_4_x_x, x_x_13_x)%>%
        dplyr::filter(TB!="N")
    melt<-melt(Boolean,id.vars=c("Donor","TB","SM", "Stim"))
    data1<-filter(melt, variable %in% c("G_4_13_T", "G_4_x_T", "G_x_13_T", "G_4_13_x", "x_4_13_T","G_4_x_x", "G_x_13_x","x_4_x_T","x_x_13_T"))
    data2<-filter(melt, variable %in% c("G_x_x_T" , "G_x_x_x","x_x_x_T"))
    data3<-filter(melt, variable %in% c("x_4_13_x","x_4_x_x", "x_x_13_x"))
    
    g1<-ggplot(data1, aes(x=variable, y=value, fill=TB, alpha=SM))+
        geom_boxplot(size=1, position=position_dodge(width = 1)) + 
        scale_fill_manual(values =c("#1a9850" , "#2166ac", "#b2182b"))+
        scale_alpha_manual(values=c(0.5,1))+
        theme_classic()+ theme(legend.position="none")+
        theme(strip.text.y = element_blank())+
        theme(text = element_text(size=20), axis.text.x = element_blank())+
        labs(y="Cytokine+ CD4+ Cells (%)", x="", title="TH1/2")+
        theme(plot.title = element_text(hjust = 0.5)) +
        facet_grid(TB~., scale="fixed")+
        stat_compare_means(label = "p.signif", p.adjust.method = "fdr", hide.ns = TRUE, size = 8, 
                           method="wilcox.test", paired = FALSE)
    
    g2<-ggplot(data2, aes(x=variable, y=value, fill=TB, alpha=SM))+
        geom_boxplot(size=1, position=position_dodge(width = 1)) + 
        scale_fill_manual(values =c("#1a9850" , "#2166ac", "#b2182b"))+
        scale_alpha_manual(values=c(0.5,1))+
        theme_classic()+ theme(legend.position="none")+
        theme(strip.text.y = element_blank())+
        theme(text = element_text(size=20), axis.text.x = element_blank()) + 
        labs(y="", x="", title="TH1")+
        theme(plot.title = element_text(hjust = 0.5)) +
        facet_grid(TB~., scale="fixed")+
        stat_compare_means(label = "p.signif", p.adjust.method = "fdr", hide.ns = TRUE, size = 8, 
                           method="wilcox.test", paired = FALSE)
    
    
    g3<-ggplot(data3, aes(x=variable, y=value, fill=TB, alpha=SM))+
        geom_boxplot(size=1, position=position_dodge(width = 1)) + 
        scale_fill_manual(values =c("#1a9850" , "#2166ac", "#b2182b"))+
        scale_alpha_manual(values=c(0.5,1))+
        theme_classic()+ theme(legend.position="none")+
        theme(text = element_text(size=16), axis.text.x = element_blank()) + 
        labs(y="", x="", title="TH2")+
        theme(plot.title = element_text(hjust = 0.5)) +
        facet_grid(TB~., scale="fixed")+
        stat_compare_means(label = "p.signif", p.adjust.method = "fdr", hide.ns = TRUE, size = 8, 
                           method="wilcox.test", paired = FALSE)
    
    plot<-grid.arrange(g1, g2, g3, ncol=3, widths=c(8,4,4),
                       top=text_grob("Cytokine Production", size=24))
}

plot_6<-function(data, xval, yval){
    ggplot(data, aes(x=data[,xval], y=data[,yval]))+
        geom_boxplot(size=1, position=position_dodge(width = 1), outlier.shape = NA, aes(fill=TB, alpha=SM)) +
        geom_jitter(width=.1,height=0, shape=16,size=2)+
        scale_fill_manual(values =c("#1a9850" , "#2166ac", "#b2182b"))+
        scale_alpha_manual(values=c(0.5,1))+
        scale_x_discrete(labels=c("SM-", "SM+"))+
        theme_classic()+ theme(legend.position="none")+
        theme(axis.text.x = element_text(angle=90, vjust=0.6)) + 
        theme(text = element_text(size=12), axis.text.x = element_text(angle=90, vjust=0.6)) +   
        facet_grid(~TB, scale="free")+
        stat_compare_means(label = "p.format", p.adjust.method = "fdr", hide.ns = TRUE, size = 4, 
                           method="wilcox.test", paired = FALSE, label.y= (max(data[,yval])*1.1))
}

plot_2<-function(data, xval, yval){
    my_comparisons <- list(c("LTBI SM", "LTBI X"), c("TB SM", "TB X"), 
                           c("TB SM", "LTBI SM"), c("TB X", "LTBI X"))
    ggplot(data, aes(x=data[,xval], y=data[,yval]))+
        geom_boxplot(size=1, position=position_dodge(width = 1), outlier.shape = NA, aes(fill=TB, alpha=SM)) +
        geom_jitter(width=.1,height=0, shape=16,size=2)+
        scale_fill_manual(values =c("#2166ac", "#b2182b"))+
        scale_alpha_manual(values=c(0.5,1))+
        scale_x_discrete(labels=c("QFT+SM-", "QFT+SM+", "TB SM-","TB SM+"))+
        theme_classic()+ theme(legend.position="none")+
        theme(plot.title = element_text(hjust = 0.5))+
        theme(text = element_text(size=14)) +   
        stat_compare_means(comparisons=my_comparisons, aes(label=..p.adj..), 
                           label = "p.format", p.adjust.method = "fdr", hide.ns = TRUE, size = 5, 
                           method="wilcox.test", paired = FALSE, na.rm=TRUE)+
        labs(title="", 
             x="",
             y=yval)}

plot_compare<-function(data, xval, yval){
    ##Compares Total responses to Mtb-specific responses not paired
    ##requires melted data where xval == 
    ggplot(data, aes(x=data[,xval], y=data[,yval]))+
        geom_boxplot(size=1, position=position_dodge(width = 1), outlier.shape = NA, aes(fill=TB, alpha=SM)) +
        geom_jitter(width=.1,height=0, shape=16,size=2)+
        scale_fill_manual(values =c("#2166ac", "#b2182b"))+
        scale_alpha_manual(values=c(0.5,1))+
        theme_classic()+ theme(legend.position="none")+
        theme(plot.title = element_text(hjust = 0.5))+
        theme(text = element_text(size=20), axis.text.x = element_text(angle=30, vjust=0.6)) + 
        theme(strip.text.x = element_blank())+
        facet_grid(~TB+SM, scale="free")+
        stat_compare_means(label = "p.format", p.adjust.method = "fdr", hide.ns = TRUE, size = 6, 
                           method="wilcox.test", paired = FALSE, 
                           label.y = 1.1*(max(data[,yval], na.rm = TRUE)))+
        labs(y=paste(yval, "+ CD4+ (%)"), 
             x="")
}

plot_pbmc<-function(data, yval){
    data[,yval]<-as.numeric(data[,yval])
    ggplot(data, aes(x=SM, y=data[,yval]))+    
    geom_boxplot(size=1, position=position_dodge(width = 1), outlier.shape = NA, aes(fill=TB, alpha=SM)) +
    geom_jitter(width=.1,height=0, shape=16,size=2)+
    scale_alpha_manual(values=c(0.5,0.5,1))+
    scale_fill_manual(values = c("#e0e0e0", "#1a9850" , "#2166ac", "#b2182b"))+
    theme_classic()+ theme(legend.position="none")+
    theme(text = element_text(size=14)) +   
    facet_grid(~TB, scales="free")+
    labs(x="",
         y="")
}

plot_pbmc.pairs<-function(data){
    ggplot(data, aes(x=variable, y=value, color=TB, alpha=SM))+
        geom_point(shape=16,size=2)+
        geom_line(aes(group=Donor))+
        scale_alpha_manual(values=c(0.5,0.5,1))+
        scale_color_manual(values = c("#e0e0e0", "#1a9850" , "#2166ac", "#b2182b"))+
        theme_classic()+ 
        theme(legend.position="none")+
        scale_x_discrete(labels=c("Pre", "Post"))+
        theme(text = element_text(size=20)) + 
        facet_grid(SM~TB, scales="free")+
        stat_compare_means(label = "p.format", p.adjust.method = "fdr", hide.ns = TRUE, size = 6, 
                           method="wilcox.test", paired = TRUE, 
                           label.y = 1.1*(max(data$value, na.rm = TRUE)))+
        labs(x="", y="")
}

plot_stim<-function(data, yval){
    ggplot(data, aes(y=data[,yval], x=Stim, col=TB))+
        geom_point(size=3, alpha=0.5)+geom_line(aes(group=Donor))+
        scale_color_manual(values =c("#2166ac", "#b2182b"))+
        theme_bw()+ theme(legend.position="none")+
        theme(text = element_text(size=20)) + 
        facet_grid(SM~TB)+
        stat_compare_means(comparisons = list(c("PMA", "PEP"), c("PMA","WCL"))
            , na.rm=TRUE, size=4, label.y.npc = .9)+
        labs(y= yval)
}


baseplot<-function(data, xval, yval, colors){
    ggplot(data, aes(data[,xval], data[,yval], fill=SM))+
        geom_boxplot(size=1, position=position_dodge(width = 1), outlier.shape = NA)+
        scale_fill_manual(values = colors)+
        theme_classic()+ theme(legend.position="none")+
        theme(panel.border = element_rect(fill = NA, colour = "black"))+
        theme(text = element_text(size=20),
              axis.text.x = element_text(size = 16),
              strip.text = element_text(size = 20))+
        theme(plot.title = element_text(hjust = 0.5, size=20)) +
        labs(x="", y="")
}

booleanplot<-function(data, xval, yval, colors){
    ggplot(data, aes(data[,xval], data[,yval], fill=SM))+
        geom_boxplot(size=1, position=position_dodge(width = 1), outlier.shape = NA)+
        scale_fill_manual(values = colors)+
        theme_classic()+ 
        theme(legend.position="none")+
        theme(text = element_text(size=20), 
              axis.text.x = element_text(size=20), 
              strip.text = element_text(size = 20))+
        theme(plot.title = element_text(hjust = 0.5, size=20)) +
        stat_compare_means(method="wilcox.test", label="p.signif", hide.ns = TRUE, size=10)+
        labs(x="", y="")
}

booleanplot2<-function(data, xval, yval, colors){
    ggplot(data, aes(data[,xval], data[,yval], fill=disease))+
        geom_boxplot(size=1, position=position_dodge(width = 1), outlier.shape = NA)+
        scale_fill_manual(values = colors)+
        theme_classic()+ 
        theme(legend.position="none")+
        theme(text = element_text(size=20), 
              axis.text.x = element_text(size=20),
              strip.text = element_text(size = 20))+
        theme(plot.title = element_text(hjust = 0.5, size=20)) +
        labs(x="", y="")
}

allplot<-function(data, xval, yval, colors){
    ggplot(data, aes(data[,xval], data[,yval], fill=disease))+
        geom_boxplot(size=1, position=position_dodge2(width=0.2, padding = 0.2, preserve = "single"), outlier.shape = NA)+
        scale_fill_manual(values = colors)+
        theme_classic()+ 
        theme(legend.position="none") +
        theme(panel.border = element_rect(fill = NA, colour = "black"))+
        facet_wrap(~variable)+
        theme(text = element_text(size=20),
              axis.text.x = element_text(size = 16),
              strip.text = element_text(size = 20))+
        theme(plot.title = element_text(hjust = 0.5, size=20))+
        labs(x="", y="")
}

cyt_groups <- as_labeller(c("IFNg" = "IFNγ", 
                            "TNFa" = "TNFα", "IL4" = "IL-4", "IL13" = "IL-13"))

all_comps<-list(c("HC SM+", "HC SM-"),c("LTBI SM+", "LTBI SM-"), c("TB SM+", "TB SM-")) 

healthy.cols<- c("SM+" = "#1a9850", "SM-" ="#91cf60")
ltbi.cols<- c("SM+" = "#2166ac", "SM-" = "#85ABD1")
tb.cols<- c("SM+" = "#b2182b", "SM-"="#E59EA6")
all.cols<- c("Naive" = "#C8D0D8","HC SM+" = "#1a9850", "HC SM-" ="#91cf60",
             "LTBI SM+" = "#2166ac", "LTBI SM-" = "#85ABD1",
             "TB SM+" = "#b2182b", "TB SM-"="#E59EA6")
