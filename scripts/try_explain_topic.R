wp_segments=readRDS("data/wp_segments.RDS")
tib_classes=wp_segments %>%
  select(class,class_name,color) %>%
  unique()
tib_count_class=wp_segments %>% 
  group_by(article,class_name) %>% 
  tally()
tib=crossing(wp_pages %>% select(article) %>% unique(),
             tib_classes %>% select(class_name)) %>% 
  left_join(tib_count_class,by=c("article","class_name")) %>% 
  mutate(n=replace_na(n,0)) %>% 
  group_by(article) %>% 
  mutate(ntot=sum(n)) %>% 
  ungroup() %>% 
  mutate(ntotcat=cut(ntot,breaks=quantile(ntot,seq(0,1,by=0.25))),
         prop=n/ntot) %>%
  ungroup() %>% 
  na.omit()



tiblarge=tib %>% 
  mutate(prop=n/ntot) %>% 
  select(article,class_name,prop) %>% 
  na.omit() %>% 
  pivot_wider(names_from=class_name,values_from=prop)
tiblarge=left_join(tiblarge,
                   wp_pages_complete %>%
                     select(article, length,curation,deathtoll,
                            HDI,year,local,population,density,mean_views_f3months))
  datarf=tiblarge %>%
    mutate(complex=anticipation + governance + hydrology) %>% 
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
             importance=TRUE,nodesize=20)
  var_importance <- var.select(myrf,method="md", verbose=FALSE)
  var_import= var_importance$varselect %>%  rownames_to_column()%>% as_tibble() %>% arrange(depth)
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
  return(result)
}