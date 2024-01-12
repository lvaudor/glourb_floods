dfo_comp=readRDS("data/dfo_comp.RDS")
wd_full=readRDS("data/wd_full.RDS")
corrections=tibble::tribble( ~raw,~clean,
                             "El Savador","El Salvador",
                             "Bangaldesh","Bangladesh",
                             "Bangledesh","Bangladesh",
                             "Boliva","Bolivia",
                             "Bosnia and Herzogovina","Bosnia and Herzegovina",
                             "Bosnia-Herzegovenia","Bosnia and Herzegovina",
                             "Bosnia-Herzegovina","Bosnia and Herzegovina",
                             "Britain","United Kingdom of Great Britain and Ireland",
                             "Britain, Ireland","United Kingdom of Great Britain and Ireland",
                             "England","United Kingdom of Great Britain and Ireland",
                             "UK","United Kingdom of Great Britain and Ireland",
                             "Unitd Kingdom","United Kingdom of Great Britain and Ireland",
                             "Scotland","United Kingdom of Great Britain and Ireland",
                             "Burkino Faso","Burkina Faso",
                             "Burma","Myanmar",
                             "Burma/Myanmar","Myanmar",
                             "Camaroun","Cameroon",
                             "Congo","Republic of the Congo",
                             "Congo Republic","Republic of the Congo",
                             "DR Congo","Democratic Republic of the Congo",
                             "Democratic  Republic of the Congo","Democratic Republic of the Congo",
                             "Democratic  Republic Congo","Democratic Republic of the Congo",
                             "Democratic Republic of Congo","Democratic Republic of the Congo",
                             "Cote D'Iavoir","Ivory Coast",
                             "Cote d'Ivoire","Ivory Coast",
                             "China","People's Republic of China",
                             "Guatamala","Guatemala",
                             "USA","United States of America",
                             "USA.","United States of America",
                             "Viet Nam","Vietnam",
                             "Zimbawe","Zimbabwe",
                             "Venezulea","Venezuela",
                             "Uruguay,","Uruguay",
                             "Serbia and Montenegro","Serbia",
                             "Serbia-Montenegro","Serbia",
                             "Madascar","Madagascar",
                             "Philipines","Philippines",
                             "Philippine","Philippines",
                             "Phillipines","Philippines",
                             "Phillippines","Philippines",
                             "South Sudan","Sudan",
                             "Sudan and Eritrea","Sudan"
                             )

                             
                             
correct=function(vstring){
  result=tibble::tibble(vstring=vstring) %>% 
    left_join(corrections, by=c("vstring"="raw")) %>% 
    mutate(vstring=case_when(!is.na(clean)~clean,
                             TRUE~vstring))
  return(result$vstring)
}

dfo_rc=dfo_comp%>% 
  mutate(country_label=str_replace(country_label,"\\s*$","")) %>% 
  mutate(country_label=str_replace(country_label,"^\\s*","")) %>% 
  mutate(country_label=correct(country_label)) %>% 
  mutate(flood=case_when((flood=="4842" & country_label=="Mozambique")~"4842b",
                         TRUE~flood)) %>% 
  mutate(flood_label=case_when(flood=="4842b"~"4842b",
                               TRUE~flood_label))

tt=full_join(wd_full %>% select(flood,country_label) %>% unique() %>% group_by(country_label) %>% summarise(nwd=n()),
             dfo_rc %>% group_by(country_label) %>% summarise(ndfo=n()),
             by="country_label")
tt
dfo_compc=sf::st_as_sf(dfo_rc,wkt="coords") %>% 
  group_by(flood,flood_label) %>% 
  summarise(flood_label=unique(flood_label),
            country_label=unique(country_label),
            deathtoll=mean(deathtoll),
            start=min(start),
            end=max(end),
            date=mean(date)) %>% 
  sf::st_centroid() %>% 
  ungroup() 
saveRDS(dfo_compc,"data/dfo_compc.RDS")
