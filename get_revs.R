get_revs=function(lang, title){
  f=function(x){
    ind=which(map_lgl(x,is.null))
    if(length(ind)>0){x[[ind]]=NA}
    return(x)
  }
  url=paste0("https://api.wikimedia.org/core/v1/wikipedia/",
             lang,
             "/page/",
             URLencode(title),
             "/history")
  revs_raw=GET(url)
  revs=revs_raw %>%
    content() %>%
    .$revisions %>%
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
    mutate(timestamp=lubridate::ymd_hms(timestamp)) %>% 
    mutate(day=lubridate::round_date(timestamp,"day"),
           year=lubridate::year(timestamp))
  return(revs)
}
