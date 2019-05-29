library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(data.table)
library(reshape2)
library(knitr)
library(cowplot)

plot_filter<-function(datatable){
    library(dplyr)
    data<-filter(datatable, TB!="N")
    data$TB<-factor(data$TB, levels=c("HC","LTBI","TB"))
    data$SM<-factor(data$SM, levels=c("X","SM"))
    data
}

my_comparisons <- list(c("HC SM", "HC X"), c("LTBI SM", "LTBI X"), c("TB SM", "TB X"), 
                       c("TB SM", "LTBI SM"), c("TB X", "LTBI X"))

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

plot_with_all_stats<-function(DF, antigen, yval){
    
    DF<-dplyr::filter(DF, TB != "N" & Stim == antigen)
    DF$disease<-paste(DF$TB, DF$SM, sep=" ")
    DF$disease<-factor(DF$disease, levels=c("HC X", "HC SM", "LTBI X", "LTBI SM", "TB X", "TB SM"))
    
    g<-ggplot(DF, aes(x=disease, y=DF[,yval]))
    g+    geom_boxplot(aes(fill=TB, alpha=SM), size=1, position=position_dodge(width = 1), outlier.shape=NA)+
        geom_jitter(width=.1,height=0, shape=16,size=2)+
        scale_alpha_manual(values=c(0.5,1))+
        scale_fill_manual(values = c("#1a9850" , "#2166ac", "#b2182b"))+
        theme_classic()+ theme(legend.position="none")+
        theme(text = element_text(size=20), axis.text.x = element_text(angle=90, vjust=0.6)) + 
        theme(plot.title = element_text(hjust = 0.5)) +
        stat_compare_means(comparisons = my_comparisons, p.adjust="bonferroni")
}

triple_boolean_plot<-function(datatable, antigen){
    library(dplyr)
    DF<-dplyr::filter(datatable, Stim==antigen)
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
                       top=text_grob(paste("Cytokine Production from", antigen, sep=" "), size=24))
}

plot_hc<-function(data, xval, yval){
    ggplot(data, aes(x=data[,xval], y=data[,yval]))+
        geom_boxplot(size=1, position=position_dodge(width = 1), outlier.shape = NA, aes(fill=TB, alpha=SM)) +
        geom_jitter(width=.1,height=0, shape=16,size=2)+
        scale_fill_manual(values="#1a9850")+
        scale_alpha_manual(values=c(0.5,1))+
        scale_x_discrete(labels=c("SM-", "SM+"))+
        theme_classic()+ 
        theme(legend.position="none")+
        theme(plot.subtitle = element_text(hjust = 0.5))+
        theme(axis.text.x = element_text(angle=90, vjust=0.6)) + 
        theme(text = element_text(size=12), axis.text.x = element_text(angle=90, vjust=0.6)) +   
        stat_compare_means(label = "p.format", p.adjust.method = "fdr", hide.ns = TRUE, size = 4, 
                           method="wilcox.test", paired = FALSE, label.y= (max(data[,yval])*1.1),
                           label.x.npc = "center")+
        labs(subtitle=paste("Frequency of", yval, "+ CD4+ T cells"), 
             caption="",
             x="",
             y=paste("Frequency of ", yval, "+ CD4+ (%)"))
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
    ggplot(data, aes(x=data[,xval], y=data[,yval]))+
        geom_boxplot(size=1, position=position_dodge(width = 1), outlier.shape = NA, aes(fill=TB, alpha=SM)) +
        geom_jitter(width=.1,height=0, shape=16,size=2)+
        scale_fill_manual(values =c("#2166ac", "#b2182b"))+
        scale_alpha_manual(values=c(0.5,1))+
        scale_x_discrete(labels=c("SM-", "SM+"))+
        theme_classic()+ theme(legend.position="none")+
        theme(axis.text.x = element_text(angle=90, vjust=0.6)) + 
        theme(text = element_text(size=20), axis.text.x = element_text(angle=90, vjust=0.6)) +   
        facet_grid(~TB, scale="free")+
        stat_compare_means(label = "p.format", p.adjust.method = "fdr", hide.ns = TRUE, size = 4, 
                           method="wilcox.test", paired = FALSE, label.y= (max(data[,yval], na.rm=TRUE)*1.1), na.rm=TRUE)+
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
        theme(text = element_text(size=20), axis.text.x = element_text(angle=30, vjust=0.6)) + 
        theme(strip.text.x = element_blank())+
        facet_grid(~TB+SM, scale="free")+
        stat_compare_means(label = "p.format", p.adjust.method = "fdr", hide.ns = TRUE, size = 6, 
                           method="wilcox.test", paired = FALSE, 
                           label.y = 1.1*(max(data[,yval], na.rm = TRUE)))+
        labs(y=paste(yval, "+ CD4+ (%)"), 
             x="")
}
