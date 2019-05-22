library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(data.table)
library(reshape2)
library(knitr)

plot_filter<-function(datatable){
    library(dplyr)
    data<-filter(datatable, TB!="N")
    data$TB<-factor(data$TB, levels=c("HC","LTBI","TB"))
    data$SM<-factor(data$SM, levels=c("X","SM"))
    data
}

triple_boolean_plot<-function(datatable, antigen){
    DF<-filter(datatable, Stim==antigen)
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
        theme_bw()+ theme(legend.position="none")+
        theme(strip.text.y = element_blank())+
        theme(text = element_text(size=20), axis.text.x = element_text(angle=90, vjust=0.6))+
        labs(y="Cytokine+ CD4+ Cells (%)")+
        facet_grid(TB~., scale="fixed")+
        stat_compare_means(label = "p.signif", p.adjust.method = "fdr", hide.ns = TRUE, size = 8, 
                       method="wilcox.test", paired = FALSE)

    g2<-ggplot(data2, aes(x=variable, y=value, fill=TB, alpha=SM))+
        geom_boxplot(size=1, position=position_dodge(width = 1)) + 
        scale_fill_manual(values =c("#1a9850" , "#2166ac", "#b2182b"))+
        scale_alpha_manual(values=c(0.5,1))+
        theme_bw()+ theme(legend.position="none")+
        theme(strip.text.y = element_blank())+
        theme(text = element_text(size=20), axis.text.x = element_text(angle=90, vjust=0.6)) + 
        labs(y="")+
        facet_grid(TB~., scale="fixed")+
        stat_compare_means(label = "p.signif", p.adjust.method = "fdr", hide.ns = TRUE, size = 8, 
                       method="wilcox.test", paired = FALSE)


    g3<-ggplot(data3, aes(x=variable, y=value, fill=TB, alpha=SM))+
        geom_boxplot(size=1, position=position_dodge(width = 1)) + 
        scale_fill_manual(values =c("#1a9850" , "#2166ac", "#b2182b"))+
        scale_alpha_manual(values=c(0.5,1))+
        theme_bw()+ theme(legend.position="none")+
        theme(text = element_text(size=20), axis.text.x = element_text(angle=90, vjust=0.6)) + 
        labs(y="")+
        facet_grid(TB~., scale="fixed")+
        stat_compare_means(label = "p.signif", p.adjust.method = "fdr", hide.ns = TRUE, size = 8, 
                       method="wilcox.test", paired = FALSE)
    
    plot<-grid.arrange(g1, g2, g3, ncol=3, widths=c(8,4,4),
                   top=text_grob(paste("Cytokine Production from", antigen, sep=" "), size=20))
}

plot_6<-function(data, xval, yval){
    ggplot(data, aes(x=data[,xval], y=data[,yval]))+
        geom_boxplot(size=1, position=position_dodge(width = 1), outlier.shape = NA, aes(fill=TB, alpha=SM)) +
        geom_jitter(width=.1,height=0, shape=16,size=2)+
        scale_fill_manual(values =c("#1a9850" , "#2166ac", "#b2182b"))+
        scale_alpha_manual(values=c(0.5,1))+
        scale_x_discrete(labels=c("SM-", "SM+"))+
        theme_bw()+ theme(legend.position="bottom")+
        theme(axis.text.x = element_text(angle=90, vjust=0.6)) + 
        theme(text = element_text(size=12), axis.text.x = element_text(angle=90, vjust=0.6)) +   
        facet_grid(~TB, scale="free")+
        stat_compare_means(label = "p.format", p.adjust.method = "fdr", hide.ns = TRUE, size = 4, 
                           method="wilcox.test", paired = FALSE, label.y= (max(data[,yval])*1.1))+
        labs(subtitle=paste("Frequency of", yval, "+ CD4+ T cells"), 
             caption="",
             x="",
             y=paste(yval, "+ CD4+ (%)"))
}

plot_2<-function(data, xval, yval){
    ggplot(data, aes(x=data[,xval], y=data[,yval]))+
        geom_boxplot(size=1, position=position_dodge(width = 1), outlier.shape = NA, aes(fill=TB, alpha=SM)) +
        geom_jitter(width=.1,height=0, shape=16,size=3)+
        scale_fill_manual(values =c("#2166ac", "#b2182b"))+
        scale_alpha_manual(values=c(0.5,1))+
        scale_x_discrete(labels=c("SM-", "SM+"))+
        theme_bw()+ theme(legend.position="none")+
        theme(axis.text.x = element_text(angle=90, vjust=0.6)) + 
        theme(text = element_text(size=16), axis.text.x = element_text(angle=90, vjust=0.6)) +   
        facet_grid(~TB, scale="free")+
        stat_compare_means(label = "p.format", p.adjust.method = "fdr", hide.ns = TRUE, size = 4, 
                           method="wilcox.test", paired = FALSE, label.y= (max(data[,yval], na.rm=TRUE)*1.1), na.rm=TRUE)+
        labs(title="", 
             x="",
             y=yval)}

plot_compare<-function(data, xval, yval){
    ggplot(data, aes(x=data[,xval], y=data[,yval]))+
        geom_boxplot(size=1, position=position_dodge(width = 1), outlier.shape = NA, aes(fill=TB, alpha=SM)) +
        geom_jitter(width=.1,height=0, shape=16,size=3)+
        scale_fill_manual(values =c("#2166ac", "#b2182b"))+
        scale_alpha_manual(values=c(0.5,1))+
        theme_bw()+ theme(legend.position="none")+
        theme(text = element_text(size=20), axis.text.x = element_text(angle=30, vjust=0.6)) + 
        theme(strip.text.x = element_blank())+
        facet_grid(~TB+SM, scale="free")+
        stat_compare_means(label = "p.format", p.adjust.method = "fdr", hide.ns = TRUE, size = 4, 
                           method="wilcox.test", paired = FALSE, 
                           label.y = 1.1*(max(data[,yval], na.rm = TRUE)))+
        labs(y=paste(yval, "+ CD4+ (%)"), 
             x="")
}

