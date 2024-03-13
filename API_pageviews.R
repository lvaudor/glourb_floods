library(httr)
library(jsonlite)

wp_pages=readRDS("data/wp_pages.RDS")

get_page_views=function(title,lang){
  # Définir la plage de dates pour laquelle vous souhaitez récupérer les statistiques
  start_date <- "20000101"
  end_date <- "20240131"
  title=URLencode(title)
  string1="https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article"
  string2="all-access/all-agents"
  # Construire l'URL de l'API Wikimedia pour récupérer les statistiques de consultation
  url=glue::glue("{string1}/{lang}.wikipedia/{string2}/{title}/daily/{start_date}/{end_date}")
  response= httr::GET(url)
  
  # Vérifier si la requête a réussi
  if (httr::status_code(response) == 200) {
    # Convertir les données JSON en un objet R
    content=httr::content(response, "text", encoding = "UTF-8")
    data_views = jsonlite::fromJSON(content)[[1]] %>% 
      dplyr::select(timestamp,views)
  } else {
    cat("Erreur lors de la récupération des données.")
    cat(title)
    cat(lang)
    return(NA)
  }  
  return(data_views)
}

wp_pages_views=wp_pages %>%
  dplyr::select(article,title,lang) %>% 
  dplyr::mutate(data=purrr::map2(.x=title,.y=lang, get_page_views)) %>% 
  tidyr::unnest(c(data))

saveRDS(wp_pages_views,"data/wp_pages_views")
