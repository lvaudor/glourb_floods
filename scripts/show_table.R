show_table=function(table,provide_key=NA){
  result_class=summarise_all(table,class)%>%
    pivot_longer(everything(),
                 names_to="var",values_to="class")
  set.seed(123)
  ind=sample(1:nrow(table),1)
  result_random=table[ind,] %>%
    summarise_all(as.character) %>% 
    pivot_longer(everything(),
                 names_to="var",values_to="example")
  result_ndistinct=summarise_all(table,~length(unique(.x))) %>% 
    pivot_longer(everything(),
                 names_to="var",values_to="n_distinct")
  result_key=tibble::tibble(var=result_class$var,
                            key="") %>% 
    mutate(key=case_when(var %in% provide_key ~"*",
                         TRUE~""))

  result=bind_cols(result_key,
                   result_class %>% select(-var),
                   result_ndistinct %>% select(-var),
                   result_random %>% select(-var)) %>% 
    mutate(example=case_when(var %in% c("text","textt")~paste0(stringr::str_sub(example,1,100),"... [truncated]"),
                             TRUE~example)) %>% 
    mutate(example=case_when(var %in% c("text","textt")~stringr::str_replace_all(example,"\n"," - "),
                             TRUE~example))
  return(result)
}
