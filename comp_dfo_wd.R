if(!file.exists("data/wm_dfo_comparison.RDS")){

library(tidyverse)
countries=readRDS("data/countries.RDS")
wp_pages=readRDS("data/wp_pages.RDS")
#wp_segments=readRDS("data/wp_segments.RDS")
dfo_comp=readRDS("data/dfo_comp.RDS") %>%
  mutate(flood_dfo=flood) %>%
  select(-flood,-flood_label) %>% 
  sf::st_drop_geometry()
wm_dfo_corr=readRDS("data/wm_dfo_corr.RDS") %>%
  select(flood,floodcorr) %>% 
  sf::st_drop_geometry()

dfo_comp=left_join(dfo_comp ,
                   wm_dfo_corr,
                   by=c("flood_dfo"="floodcorr")) %>% 
  mutate(deathtoll_dfo=deathtoll,
         country_label_dfo=country_label) %>% 
  select(-start,-end,-date,-deathtoll,-country_label) 

wd_events=readRDS("data/wd_events.RDS") %>% 
  left_join(readRDS("data/wd_raw.RDS") %>% 
              select(flood,deathtoll) %>% 
              unique(),
            by="flood",
            relationship = "many-to-many") %>% 
  left_join(countries %>% select(country,HDI),"country",relationship = "many-to-many") %>% 
  mutate(HDI_wd=HDI) %>% 
  select(-HDI) %>% 
  unique()
wd_events=wd_events %>% 
  full_join(dfo_comp,by="flood",
            relationship = "many-to-many")


tib_segs=wd_events %>% 
  left_join(countries %>% 
              select(country_label,HDI) %>% 
              unique(),
            by=c("country_label_dfo"="country_label")) %>%
  left_join(wp_pages,by=c("flood"),relationship = "many-to-many")
tib_segs=tib_segs %>%  
  group_by(flood) %>% 
  summarise(total_length=sum(length),
            local_length=sum(length[which(local==TRUE)])) %>%
  mutate(prop_local=local_length/total_length) %>% 
  ungroup()

wm_dfo_comparison=wd_events %>% 
  full_join(tib_segs,by="flood") %>% 
  mutate(total_length=case_when(is.na(flood)~0,
                                is.na(total_length)~0,
                                TRUE~total_length),
         prop_local=case_when(is.na(flood)~0,
                              is.na(total_length)~0,
                              TRUE~prop_local),
         local_length=case_when(is.na(flood)~0,
                                is.na(total_length)~0,
                                TRUE~local_length)) %>% 
  left_join(countries %>% select(country_label,HDI) %>% unique(),
            by=c("country_label_dfo"="country_label"),
            relationship="many-to-many") %>% 
  mutate(HDI=case_when(is.na(HDI)~HDI_wd,
                       TRUE~HDI)) %>% 
  mutate(HDI_class=cut(HDI,
                      quantile(countries$HDI,seq(0,1,by=0.25),
                               na.rm=TRUE,include.lowest=TRUE)),
         include.lowest=TRUE) %>% 
  mutate(in_data=case_when(is.na(flood) & !is.na(flood_dfo)~"dfo",
                           !is.na(flood) & is.na(flood_dfo)~"wd",
                           !is.na(flood) & !is.na(flood_dfo)~"in_both")) %>% 
  mutate(deathtoll=case_when(!is.na(deathtoll_dfo)~deathtoll_dfo,
                             is.na(deathtoll_dfo)~deathtoll))

saveRDS(wm_dfo_comparison,"data/wm_dfo_comparison.RDS")
}