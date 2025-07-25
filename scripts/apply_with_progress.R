apply_with_progress = function(.x, .y = NULL, .f, unlist=TRUE) {
  progressr::handlers("txtprogressbar")  # utile dans RStudio ou terminal

  with_progress({
    p = progressr::progressor(steps = length(.x))

    f_wrapped = function(xi, yi = NULL) {
      p()
      if (is.null(yi)) {
        purrr::safely(.f, otherwise = NA)(xi)$result
      } else {
        purrr::safely(.f, otherwise = NA)(xi, yi)$result
      }
    }

    result = if (is.null(.y)) {
      furrr::future_map(.x, f_wrapped)
    } else {
      furrr::future_map2(.x, .y, f_wrapped)
    }
    if(unlist){
      return(unlist(result))}else{
      return(result)
     }
})
}
# 
# 
# # Exemple avec deux colonnes
# 
# library(tidyverse)
# library(progressr)
# data = tibble::tibble(x = 1:10, y = 11:20)
# 
# data = data %>%
#   mutate(result = apply_with_progress(.x = x,
#                                       .y = y,
#                                       .f = function(a, b) a + b))
# 
# data = tibble::tibble(x = 1:10)

# # Exemple avec une colonne
# 
# data = data %>%
#   mutate(result = apply_with_progress(.x = x,
#                                       .f = function(a) a^2))

