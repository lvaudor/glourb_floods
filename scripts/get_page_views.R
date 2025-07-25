#' get information about the views of the article
#'
#' @param title 
#' @param lang 
#' @return a tibble with information about the articles' views (timestamp,views)
#' @export
#'
#' @examples
#' get_page_views("Liber_cure_cocorum","fr")
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
    warning(paste("Problem while collecting views data for article '",
                title,
                " and language ",
                lang,
                "."))
    return(NA)
  }  
  return(data_views)
}
