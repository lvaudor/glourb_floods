get_revs=function(lang, title){
  f=function(x){
    ind=which(map_lgl(x,is.null))
    if(length(ind)>0){x[ind]=NA}
    x[["comment"]]=x[["comment"]][[1]]
    if(length(x$user)==1){
      x$user=list(id=NA,name=NA)
    }
    if(length(x$user)==2){
      if(is.null(x$user$name)){name=NA}else{name=x$user$name}
      if(is.null(x$user$id)){id=NA}else{id=x$user$id}
      x$user=list(id=id,name=name)
    }
    return(x)
  }
  url=paste0("https://",lang,
             ".wikipedia.org/w/rest.php/v1/page/",
             URLencode(title),
             "/history")
  revs_raw=GET(url)
  if(revs_raw$status_code<=200){
    revs=revs_raw %>%
      content() %>%
      .$revisions %>% 
      map(f) %>% 
      map(as_tibble) %>%
      bind_rows()
    while("older" %in% names(content(revs_raw))){
      revs_raw=GET(content(revs_raw)$older)
      revs_tmp=revs_raw %>%
        content() %>%
        .$revisions %>%
        map(f) %>% 
        map(as_tibble) %>%
        bind_rows()
      revs=bind_rows(revs,revs_tmp)
    }
    revs=revs %>% 
      group_by(id) %>% 
      mutate(type=c("user_id","user_name")) %>% 
      tidyr::pivot_wider(names_from=type,values_from=user) %>% 
      tidyr::unnest(cols = c(user_id, user_name))
  }else{revs=tibble(id=NA,
                    timestamp=NA,
                    minor=NA,
                    size=NA,
                    comment=NA,
                    delta=NA,
                    user_id=NA,
                    user_name=NA)}
  return(revs)
}
