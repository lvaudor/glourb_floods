library(httr)
library(jsonlite)

# Définir le titre de l'article Wikipedia pour lequel vous souhaitez récupérer les statistiques
article_title <- "Inondation de Vaison-la-Romaine en septembre 1992"

# Définir la plage de dates pour laquelle vous souhaitez récupérer les statistiques
start_date <- "20240101"
end_date <- "20240131"

# Construire l'URL de l'API Wikimedia pour récupérer les statistiques de consultation
url <- paste0("https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/en.wikipedia/all-access/all-agents/",
              URLencode(article_title), "/daily/", start_date, "/", end_date)

# Faire la requête HTTP GET pour récupérer les données
response <- GET(url)

# Vérifier si la requête a réussi
if (status_code(response) == 200) {
  # Convertir les données JSON en un objet R
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  
  # Afficher les statistiques de consultation
  print(data)
} else {
  cat("Erreur lors de la récupération des données.")
}
