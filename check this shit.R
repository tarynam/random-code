plot_2<-function(data, xval, yval){
    ggplot(data, aes(x=data[,xval], y=data[,yval]))+
        geom_boxplot(size=1, position=position_dodge(width = 1), outlier.shape = NA, aes(fill=TB, alpha=SM)) +
        geom_jitter(width=.1,height=0, shape=16,size=3)+
        scale_fill_manual(values =c("#2166ac", "#b2182b"))+
        scale_alpha_manual(values=c(0.5,1))+
        scale_x_discrete(labels=c("SM-", "SM+"))+
        theme_bw()+ theme(legend.position="none")+
        theme(axis.text.x = element_text(angle=90, vjust=0.6)) + 
        theme(text = element_text(size=20), axis.text.x = element_text(angle=90, vjust=0.6)) +   
        facet_grid(~TB, scale="free")+
        stat_compare_means(label = "p.format", p.adjust.method = "fdr", hide.ns = TRUE, size = 4, 
                           method="wilcox.test", paired = FALSE, label.y= (max(data[,yval], na.rm = TRUE)*1.1))
}

plot_6<-function(data, xval, yval){
    ggplot(data, aes(x=data[,xval], y=data[,yval]))+
        geom_boxplot(size=1, position=position_dodge(width = 1), outlier.shape = NA, aes(fill=TB, alpha=SM)) +
        geom_jitter(width=.1,height=0, shape=16,size=3)+
        scale_fill_manual(values =c("#1a9850" , "#2166ac", "#b2182b"))+
        scale_alpha_manual(values=c(0.5,1))+
        scale_x_discrete(labels=c("SM-", "SM+"))+
        theme_bw()+ theme(legend.position="none")+
        theme(axis.text.x = element_text(angle=90, vjust=0.6)) + 
        theme(text = element_text(size=20), axis.text.x = element_text(angle=90, vjust=0.6)) +   
        facet_grid(~TB, scale="free")+
        stat_compare_means(label = "p.format", p.adjust.method = "fdr", hide.ns = TRUE, size = 4, 
                           method="wilcox.test", paired = FALSE, label.y= (max(data[,yval], na.rm = TRUE)*1.1))
}

df<-filter(phenotype, Stim=="WCL", TB!="HC")
df$SM<-factor(df$SM, levels=c("X", "SM"))
grid.arrange(plot_2(df, "SM", "IFNg_freq")+labs(y="IFNg+ Proliferating CD4+ T cells (%)", x=""),
             plot_2(df, "SM", "TNFa_freq")+labs(y="TNFa+ Proliferating CD4+ T cells (%)", x=""),
             plot_2(df, "SM", "IL4_freq")+labs(y="IL-4+ Proliferating CD4+ T cells (%)", x=""),
             ncol=3
             
)

grid.arrange(plot_2(df, "SM", "CXCR3_freq")+labs(y="CXCR3+ Proliferating CD4+ T cells (%)", x=""),
             plot_2(df, "SM", "CCR4_freq")+labs(y="CCR4+ Proliferating CD4+ T cells (%)", x=""),
             plot_2(df, "SM", "Tbet_freq")+labs(y="Tbet+ Proliferating CD4+ T cells (%)", x=""),
             plot_2(df, "SM", "GATA3_freq")+labs(y="GATA3+ Proliferating CD4+ T cells (%)", x=""),
             ncol=4
             
)

df<-filter(CD4_summary, Stim=="PEP")
df$SM<-factor(df$SM, levels=c("X", "SM"))
df$factor<-0
df$factor[(df$OG_freq>1)]<-1
df1<-filter(df, factor==0)
df2<-filter(df, factor==1)
grid.arrange(
    ggplot(df2, aes(x=SM, y=OG_freq))+
    geom_boxplot(size=1, position=position_dodge(width = 1), outlier.shape = NA, aes(fill=TB, alpha=SM)) +
    geom_jitter(width=.1,height=0, shape=16,size=3)+
    scale_fill_manual(values =c("#1a9850" , "#2166ac", "#b2182b"))+
    scale_alpha_manual(values=c(0.5,1))+
    theme_bw()+ theme(legend.position="none")+
    theme(text = element_text(size=20), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +   
    facet_grid(~TB, scale="free")+
    labs(y="", x="")
    ,
    ggplot(df1, aes(x=SM, y=OG_freq))+
    geom_jitter(width=.1,height=0, shape=16,size=3)+
    scale_fill_manual(values =c("#1a9850" , "#2166ac", "#b2182b"))+
    scale_alpha_manual(values=c(0.5,1))+
    scale_x_discrete(labels=c("SM-", "SM+"))+
    theme_bw()+ theme(legend.position="none")+
    theme(strip.background = element_blank(), strip.text.x = element_blank())+
    theme(text = element_text(size=20), axis.text.x = element_text(angle=90, vjust=0.6)) +   
    facet_grid(~TB, scale="free")+
    labs(y="", x="")
    ,
    heights=c(8,4),
    padding = unit(0.1, "line"))




plot_6(df[(df$OG_freq<1),], "SM", "OG_freq")+labs(y="Proliferating CD4+ T cells (%)", x="")
plot_6(df[(df$OG_freq>1),], "SM", "OG_freq")+labs(y="Proliferating CD4+ T cells (%)", x="")


grid.arrange(plot_6(df, "SM", "IFNg_freq")+labs(y="IFNg+ Proliferating CD4+ T cells (%)", x=""),
             plot_6(df, "SM", "TNFa_freq")+labs(y="TNFa+ Proliferating CD4+ T cells (%)", x=""),
             plot_6(df, "SM", "IL4_freq")+labs(y="IL-4+ Proliferating CD4+ T cells (%)", x=""),
             ncol=3
             
)

grid.arrange(plot_6(df, "SM", "CXCR3_freq")+labs(y="CXCR3+ Proliferating CD4+ T cells (%)", x=""),
             plot_6(df, "SM", "CCR4_freq")+labs(y="CCR4+ Proliferating CD4+ T cells (%)", x=""),
             plot_6(df, "SM", "Tbet_freq")+labs(y="Tbet+ Proliferating CD4+ T cells (%)", x=""),
             plot_6(df, "SM", "GATA3_freq")+labs(y="GATA3+ Proliferating CD4+ T cells (%)", x=""),
             ncol=4
             
)




cd4$celltype<-"CD4"
cd8$celltype<-"CD8"
gd$celltype<-"CD4-CD8-"
sm_data<-rbindlist(list(cd4, cd8, gd), fill=TRUE)
sm_data<-dplyr::filter(sm_data, Stim=="SEA"|Stim=="SWAP", SM=="SM")
sm_data<-mutate(sm_data, new = (G_4_13_T+
                                    G_4_13_x+
                                    G_4_x_T+
                                    G_4_x_x+
                                    G_x_13_T+
                                    G_x_13_x+
                                    G_x_x_T+
                                    G_x_x_x+
                                    x_4_13_T+
                                    x_4_13_x+
                                    x_4_x_T+
                                    x_4_x_x+
                                    x_x_13_T+
                                    x_x_13_x+
                                    x_x_x_T)
)

sm_data$celltype<-factor(sm_data$celltype, levels=c("CD4", "CD8","CD4-CD8-"))
ggplot(sm_data, aes(x=TB, y=new, fill=TB))+
    geom_boxplot(size=1, position=position_dodge(width = 1), outlier.shape = NA, aes(fill=TB))+
    geom_jitter(width=.1,height=0, shape=16,size=3)+
    scale_fill_manual(values =c("#1a9850" , "#2166ac", "#b2182b"))+
    theme_bw()+ theme(legend.position="none")+
    theme(text = element_text(size=16)) +   
    facet_grid(celltype~Stim, scale="free")+
    stat_compare_means(label = "p.format", p.adjust.method = "fdr", hide.ns = TRUE, size = 4, 
                       method="anova", paired = FALSE)+
    labs(title="", 
         x="",
         y="Cytokine Frequency (%)")

DF<-filter(CD4, Stim=="PEP", TB=="LTBI"|TB=="TB")
df<-filter(phen, Stim=="PEP", TB=="LTBI"|TB=="TB")

plot_test<-function(data, xval, yval){
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
                           method="wilcox.test", paired = FALSE, 
                           label.y = 1.1*(max(data[,yval], na.rm = TRUE)))+
        labs(y="", x="")
}

grid.arrange(
    plot_test(DF, "SM", "CXCR3")+ylab("CXCR3+ CD4+ T cells (%)"),
    plot_test(DF, "SM", "CCR4")+ylab("CCR4+ CD4+ T cells (%)"),
    ncol=2,
    top=text_grob("Total CD4+ T Cells", size=20))

grid.arrange(
    plot_test(df, "SM", "Cyt_CXCR3")+ylab("CXCR3+ CD4+ T cells (%)"),
    plot_test(df, "SM", "Cyt_CCR4")+ylab("CCR4+ CD4+ T cells (%)"),
    ncol=2,
    top=text_grob("Mtb-Specific CD4+ T Cells", size=20))

grid.arrange(
    plot_test(DF, "SM", "Tbet")+ylab("Tbet+ CD4+ T cells (%)"),
    plot_test(DF, "SM", "GATA3")+ylab("GATA3+ CD4+ T cells (%)"), 
    ncol=2,
    top=text_grob("Total CD4+ T Cells", size=20))

grid.arrange(
    plot_test(df, "SM", "Cyt_Tbet")+ylab("Tbet+ CD4+ T cells (%)"),
    plot_test(df, "SM", "Cyt_GAT3")+ylab("GATA3+ CD4+ T cells (%)"),
    ncol=2,
    top=text_grob("Mtb-Specific CD4+ T Cells", size=20))


df<-filter(OG_summary, Stim=="SWAP"|Stim=="SEA", SM=="SM")
Boolean<-dplyr::select(df, Donor, TB, Stim, CD4_freq, CD8_freq, GD_freq)
melt<-melt(Boolean,id.vars=c("Donor","TB", "Stim"))


ggplot(melt, aes(x=variable, y=value))+
    geom_boxplot(size=1, position=position_dodge(width = 1), outlier.shape = NA, aes(fill=Stim)) +
    geom_jitter(width=.1,height=0, shape=16,size=3)+
    theme_bw()+ theme(legend.position="none")+
    labs(y="Proliferating CD3+ Cells (%)")+
    scale_x_discrete(labels=c("CD4+", "CD8+", "GD+"))+
    theme(text = element_text(size=16)) +   
    facet_wrap(~Stim, scales="free")+
    stat_compare_means(label = "p.format", p.adjust.method = "fdr", hide.ns = TRUE, size = 4, 
                       method="anova", paired = FALSE)



