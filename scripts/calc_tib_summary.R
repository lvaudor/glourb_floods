calc_tib_summary=function(tib,group1,group2,y,round=3,
                          d1name="d1",d2name="d2"){
  if(!is.null(d1name)){d1name=paste0(d1name,"=")}else{d1name=""}
  if(!is.null(d2name)){d2name=paste0(d2name,"=")}else{d2name=""}
  group1=enquo(group1)
  group2=enquo(group2)
  y=enquo(y)
  # Select only the relevant variables
  tib0=tib %>%
    select(group1=!!group1,group2=!!group2,vary=!!y) %>% 
    na.omit()
  tibt=tib0 %>% 
    group_by(group1,group2) %>% 
    tally() %>% 
    mutate(data1=purrr::map(group1,function(val){tib0})) %>%
    mutate(data2=purrr::map(group1,function(val){tib0 %>% filter(group1==val)})) %>% 
    ungroup()
  # data1 is a tibble with a list column data that contains the whole data (all group1 levels)
  # data2 is a tibble with a list column data that contains the data for each group1 level (all group2 levels)
  
  tib1=tibt %>% 
    select(group1,data1) %>% 
    unique()%>% 
    mutate(data=purrr::pmap(.l=list(tib=data1,varx="group1",refval=group1),.f=replace_varx))%>%
    mutate(no_group1=purrr::map_dbl(data,~median(.x$vary,na.rm=TRUE)),
           signif_group1=purrr::map_dbl(data,~wilcox.test(vary~x, data=.x)$p.value),
           diff_group1=purrr::map_dbl(data,~compare_medians(.x,general=TRUE))) %>%
    mutate(signif_group1_c=signif_stars(signif_group1)) %>%
    mutate(label_group1=paste0(d1name,round(diff_group1,round),signif_group1_c)) %>%
    select(-data) %>%
    ungroup() %>% 
    select(-data1)
  tib2=tibt %>% 
    select(group1,group2,data2) %>% 
    unique()%>% 
    mutate(data=purrr::pmap(.l=list(tib=data2,varx="group2",refval=group2),.f=replace_varx))%>%
    mutate(no_group2=purrr::map_dbl(data,~median(.x$vary,na.rm=TRUE)),
           signif_group2=purrr::map_dbl(data,~wilcox.test(vary~x, data=.x)$p.value),
           diff_group2=purrr::map_dbl(data,~compare_medians(.x,general=TRUE))) %>%
    mutate(signif_group2_c=signif_stars(signif_group2)) %>%
    mutate(label_group2=paste0(d2name,round(diff_group2,round),signif_group2_c)) %>%
    select(-data) %>%
    ungroup() %>% 
    select(-data2)
  tib=full_join(tib1,tib2,by=c("group1"))
  tib3=tib0  %>% 
    group_by(group1,group2) %>%
    summarise(n=n(),
              median=median(vary,na.rm=TRUE))
  tib=full_join(tib,tib3,by=c("group1","group2"))
  return(tib)
}



# replace_varx replaces varx with TRUE if the group is xval and FALSE otherwise
replace_varx=function(tib,varx,refval){
  refval=enquo(refval)
  ind=which(colnames(tib)==varx)
  colnames(tib)[ind]="x"
  tib=tib %>%   
    mutate(x=case_when(x==!!refval~TRUE,
                       x!=!!refval~FALSE))
  return(tib)
}
