library(ggRandomForests)
library(randomForestSRC)

plot_random_forest=function(data, response, remove=NULL){
  # format data as datarf without id and NAs
  datarf=data %>% 
    select(-article) %>% 
    na.omit() %>% 
    unique()
  # isolate response variable
  ind=which(colnames(datarf)==response)
  colnames(datarf)[ind]="response"
  # remove some columns if explicitly asked through argument
  if(!is.null(remove)){
    datarf=datarf[,which(!(colnames(datarf) %in% remove))]
  }
  datarf= datarf %>% 
    na.omit() %>%
    as.data.frame()
  # run random forest model
  myrf=rfsrc(response~., 
             data=datarf,
             importance=TRUE,
             nodesize=20)
  print(myrf)
  smp.o <- subsample(myrf,verbose=FALSE)
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
                            basis=="real world and web"~col_DFO_WD)) %>% 
    arrange(desc(mean)) %>% 
    mutate(vars_signif=paste0(vars, " (",signif,")"))
  

  partial <- plot.variable(myrf,
                           xvar = datimp$vars,
                           partial = TRUE, 
                           sorted = FALSE,
                           show.plots = FALSE)
  gg_p <- gg_partial(partial)
  
  # generate a list of gg_partial objects, one per xvar.
  plots=vector("list",length=9)
  plots[[1]]=ggplot(datimp, 
                    aes(x=forcats::fct_reorder(vars_signif,mean), y= mean,color=basis)) +
    geom_point(size=2)+
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.3)+
    coord_flip()+
    scale_color_manual(values=datimp$colors,breaks=datimp$basis)+
    theme(legend.position="none")+
    ylab("importance")+xlab("")
  for(i in 1:nrow(datimp)){
    indvar=which(names(gg_p)==datimp$vars[i])
    if(datimp$vars[i]=="local"){
      dat=gg_p[[indvar]]
      dat=tibble::tibble(y=gg_p[[indvar]][,1],
                         x=gg_p[[indvar]][,2]) %>% 
        mutate(x=case_when(x==0~FALSE,
                           x==1~TRUE))
      plots[[i+1]]=ggplot(dat, aes(x=x,y=y))+
        geom_boxplot(fill=datimp$colors[i])
    }else{
      dat=gg_p[[indvar]] %>% as.data.frame()
      colnames(dat)=c("y","x","se")
      plots[[i+1]]=ggplot(dat, aes(x=x,y=y))+
        geom_path(col=datimp$colors[i])+
        geom_point(col=datimp$colors[i])+
        geom_line(aes(x=x,y=y-se),col="grey")+
        geom_line(aes(x=x,y=y+se),col="grey")
    }
    if(datimp$vars[i] %in% c("population", 
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
  return(result)
}
