library(ggRandomForests)
library(randomForestSRC)

plot_random_forest=function(response, remove=NULL){
  
  datarf=wp_pages_complete %>% 
    select(local,
           year,
           curation,
           length,
           deathtoll,
           HDI,
           population,
           density,
           mean_views_f3months)
  
  ind=which(colnames(datarf)==response)
  colnames(datarf)[ind]="response"
  if(!is.null(remove)){
    datarf=datarf[,which(!(colnames(datarf) %in% remove))]
  }
  datarf= datarf %>% 
    na.omit() %>%
    as.data.frame()
  myrf=rfsrc(response~., 
             data=datarf,importance=TRUE,nodesize=20)
  var_importance <- var.select(myrf,method="md")
  # oo <- subsample(myrf, verbose = FALSE)
  # vimpCI <- extract.subsample(oo)$var.jk.sel.Z
  
  datimp=tibble::tibble(vars=var_importance$topvars,
                        vals=var_importance$varselect$vimp) %>% 
    arrange(desc(vals)) %>% 
    mutate(basis=case_when(vars %in% c("length",
                                       "curation",
                                       "mean_views_f3months") ~"web",
                           vars %in% c("local","year")~"real world and web",
                           TRUE~"real world")) %>% 
    mutate(colors=case_when(basis=="web"~col_WD,
                            basis=="real world"~col_DFO,
                            basis=="real world and web"~col_DFO_WD))
  
  partial <- plot.variable(myrf,
                           xvar = datimp$vars,
                           partial = TRUE, sorted = FALSE,
                           show.plots = FALSE)
  gg_p <- gg_partial(partial)
  
  # generate a list of gg_partial objects, one per xvar.
  plots=vector("list",length=9)
  plots[[1]]=ggplot(datimp,
                    aes(x=forcats::fct_reorder(vars,vals),
                        y=vals,
                        fill=basis))+
    geom_bar(stat="identity")+
    coord_flip()+
    ylab("importance")+
    xlab("")+
    scale_fill_manual(values=c(col_DFO,col_DFO_WD,col_WD))+
    theme(legend.position="none")
  for(i in 1:nrow(datimp)){
    if(names(gg_p)[i]=="local"){
      dat=gg_p[[i]]
      dat=tibble::tibble(y=gg_p[[i]][,1],
                         x=gg_p[[i]][,2]) %>% 
        mutate(x=case_when(x==0~FALSE,
                           x==1~TRUE))
      plots[[i+1]]=ggplot(dat, aes(x=x,y=y))+
        geom_boxplot(fill=datimp$colors[i])
    }else{
      dat=gg_p[[i]] %>% as.data.frame()
      colnames(dat)=c("y","x","se")
      plots[[i+1]]=ggplot(dat, aes(x=x,y=y))+
        geom_path(col=datimp$colors[i])+
        geom_point(col=datimp$colors[i])+
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
  return(result)
}