
datarf=tiblarge %>%
    mutate(complex=anticipation + governance + hydrology) %>% 
    unique() %>% 
  select(local,
         year,
         curation,
         length,
         deathtoll,
         HDI,
         population,
         density,
         complex,
         mean_views_f3months) 
  response="complex"
  ind=which(colnames(datarf)==response)
  colnames(datarf)[ind]="response"
  datarf= datarf %>% 
    na.omit() %>%
    as.data.frame()
  myrf=rfsrc(response~., 
             data=datarf,
             importance=TRUE,
             nodesize=10)
  smp.o <- subsample(myrf)
  oo <- extract.subsample(smp.o, alpha = 0.001)
  datimp= oo$var.jk.sel.Z %>%
    rownames_to_column("vars") %>%
    mutate(signif_og=signif) %>% 
    mutate(signif=signif_stars(pvalue)) %>% 
    mutate(basis=case_when(vars %in% c("length",
                                       "curation",
                                       "mean_views_f3months") ~"web",
                           vars %in% c("local","year")~"real world and web",
                           TRUE~"real world")) %>% 
    mutate(colors=case_when(basis=="web"~col_WD,
                            basis=="real world"~col_DFO,
                            basis=="real world and web"~col_DFO_WD))
    
  ggplot(datimp, aes(x=forcats::fct_reorder(vars,mean), y= mean,color=basis)) +
    geom_point(size=2)+
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.3)+
    coord_flip()+
    scale_color_manual(values=datimp$colors,breaks=datimp$basis)
  partial <- plot.variable(myrf,
                           xvar = var_import$rowname,
                           partial = TRUE, sorted = FALSE,
                           show.plots = FALSE)
  gg_p <- gg_partial(partial)
  
  # generate a list of gg_partial objects, one per xvar.
  plots=vector("list",length=9)
  plots[[1]]=ggplot(var_import,
                    aes(x=forcats::fct_reorder(rowname,depth),
                        y=depth))+
    geom_bar(stat="identity")+
    scale_y_sqrt()+
    coord_flip()+
    ylab("importance")+
    xlab("")+
    theme(legend.position="none")
  for(i in 1:nrow(var_import)){
    if(names(gg_p)[i]=="local"){
      dat=gg_p[[i]]
      dat=tibble::tibble(y=gg_p[[i]][,1],
                         x=gg_p[[i]][,2]) %>% 
        mutate(x=case_when(x==0~FALSE,
                           x==1~TRUE))
      plots[[i+1]]=ggplot(dat, aes(x=x,y=y))+
        geom_boxplot()
    }else{
      dat=gg_p[[i]] %>% as.data.frame()
      colnames(dat)=c("y","x","se")
      plots[[i+1]]=ggplot(dat, aes(x=x,y=y))+
        geom_path()+
        geom_point()+
        geom_line(aes(x=x,y=y-se),col="grey")+
        geom_line(aes(x=x,y=y+se),col="grey")
    }
    if(names(gg_p)[i] %in% c("population", 
                             "length",
                             "density",
                             "deathtoll",
                             "mean_views_f3months")){
      plots[[i+1]]=plots[[i+1]]+
        scale_x_log10()
    }
    plots[[i+1]]=plots[[i+1]]+
      xlab(names(gg_p)[i])+
      ylab("")
  }
  result=ggpubr::ggarrange(plots[[1]],
                           plots[[2]],
                           plots[[3]],
                           plots[[4]],
                           plots[[5]],
                           plots[[6]],
                           plots[[7]],
                           plots[[8]],
                           plots[[9]],
                           labels=c("a","b","c","d","e","f","g","h","i"),
                           nrow=3,ncol=3)
result