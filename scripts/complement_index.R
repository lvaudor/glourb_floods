---
title: "Untitled"
---

```{r nb_corr}
nb_wm=wm_comp %>% sf::st_drop_geometry() %>% group_by(flood) %>% tally() %>% nrow()
nb_wm_in_dfo=wm_dfo_corr %>% filter(!is.na(floodcorr)) %>% group_by(flood) %>% tally() %>% nrow()
nb_dfo=dfo_comp %>% group_by(flood) %>% tally() %>% nrow()
nb_dfo_in_wd=wm_dfo_corr %>% group_by(floodcorr) %>% tally() %>% nrow()
```

Based on the dates of observations for the DFO data base, `r nb_wm` out of the `r nrow(wd_events)` flood events in our Wikidata base might fall into it.

Out of `r nb_wm` flood events documented in the WD comparison data subset, `r nb_wm_in_dfo` have at least one correspondence in the DFO comparison data subset (`r round(nb_wm_in_dfo/nb_wm *100,2)`%).

Out of `r nb_dfo` flood events documented in the DFO comparison data subset, `r nb_dfo_in_wd` have at least one correspondence in the WD comparison data subset (`r round(nb_dfo_in_wd/nb_dfo *100,2)`%).

## Map

```{r leaf_wm_map, fig.width=10,fig.height=10, warning=FALSE, message=FALSE}
# wm_map=readRDS("data/wm_map.RDS")
# # Définition d'une échelle colorée 
# # (en fonction de date de sortie) 
# pal <- colorNumeric(c("red", "green", "blue"),
#                     c(1648,1900,1950,1980,2000,2010,2023)) 
# # Création de la carte 
# leaf_wm_map=leaflet(wm_map) %>% # déf carte 
#   addProviderTiles(providers$Esri.WorldTopoMap) %>% # ajout fond de carte
#   addCircleMarkers(col=~pal(year),
#                    popup = ~popup
#                    ) 
# leaf_wm_map
```

## Map comparison to DFO

We tried to find the correspondence between events documented in Wikidata and events documented in the DFO dataset. To do that, for each WD event, we searched for all events in the DFO dataset occurring less than a year and 30 days apart and less than 400 kms away OR occurring less than a year and 30 days apart and in the same country. The limit of one year and 30 days accounts for the sometimes low precision of date (e.g., year) in Wikidata.

```{r map_comp, warning=FALSE, message=FALSE, fig.width=10, fig.height=12}
# library(leaflet) 
# # Définition d'une échelle colorée 
# # (en fonction de date de sortie) 
# # Création de la carte 
# joined_events=readRDS("data/joined_events.RDS")
# pal=colorFactor(c("red","blue"), domain=c("wd","dfo"))
# comp_map=leaflet(wm_dfo)  %>%
#   addProviderTiles(providers$Esri.WorldTopoMap) %>% # ajout fond de carte
#   addCircleMarkers(col=~pal(source),
#                    popup =~popup,
#                    radius=3) %>% 
#   addPolylines(data=joined_events,color="green")
#   

```

```{r}
# #| label: fig-map_comp
# #| fig-cap: "Comparison of events defined in DFO (red) and Wikidata/Wikipedia (blue). The attempts at finding the correspondence between a Wikidata/Wikipedia event and a corresponding one in the DFO dataset correspond to the lines."
# comp_map
```

*oui je sais ces résultats sont assez vilains pour le moment. Je vais essayer de trouver une meilleure représentation + il faudra discuter les raisons pour lesquelles il est difficile de faire une correspondance -définition des événements différent, souvent dans Wikipedia plusieurs événements sont regroupés par exemple "Inondations de telle année en Europe centrale"-)*
