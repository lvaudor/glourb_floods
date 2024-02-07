library(tidyverse)
datapath="data"
wm_dfo_corr=readRDS("data/wm_dfo_corr.RDS") %>% 
  select(flood,deathtoll_corr,country_corr)
wd_events=readRDS(glue::glue("{datapath}/wd_events.RDS")) %>% 
  left_join(readRDS(glue::glue("{datapath}/wd_raw.RDS")) %>% 
              select(flood,deathtoll) %>% 
              unique(),
            by="flood") %>% 
  left_join(wm_dfo_corr,by="flood",
            relationship="many-to-many") %>% 
  mutate(deathtoll_source=case_when(!is.na(deathtoll)~"wd",
                                    is.na(deathtoll) & !is.na (deathtoll_corr)~"dfo")) %>% 
  mutate(deathtoll=case_when(!is.na(deathtoll)~deathtoll,
                             is.na(deathtoll) & !is.na(deathtoll_corr)~deathtoll_corr))

wp_pages=readRDS(glue::glue("{datapath}/wp_pages.RDS"))
wp_segments=readRDS(glue::glue("{datapath}/wp_segments.RDS"))
wp_revisions=readRDS(glue::glue("{datapath}/wp_revisions.RDS"))
countries=readRDS(glue::glue("{datapath}/countries.RDS"))

all_dat=wd_events %>% 
  left_join(countries, by="country",relationship = "many-to-many") %>% 
  left_join(wp_pages,by="flood",relationship = "many-to-many") %>% 
  left_join(wp_segments,by="article",relationship = "many-to-many") %>%  
  mutate(local=purrr::map2_lgl(lang,language_code,~.x %in% .y))

dat=all_dat %>% 
  group_by(flood,deathtoll,deathtoll_source, HDI,year) %>% 
  tidyr::nest() %>%
  mutate(nsegments=purrr::map_dbl(data,nrow)) %>% 
  mutate(data=purrr::map(data,~filter(.x,local==TRUE))) %>% 
  mutate(nsegments_local=purrr::map_dbl(data,nrow)) %>% 
  select(-data) %>%
  ungroup() %>% 
  mutate(prop_local=nsegments_local/nsegments) %>% 
  mutate(HDIclass=cut(HDI,
                      quantile(HDI,seq(0,1,by=0.25),na.rm=TRUE,include.lowest=TRUE)),
         include.lowest=TRUE) %>% 
  select(flood,deathtoll,deathtoll_source,nsegments,HDIclass,prop_local,year) %>% 
  na.omit()
  
ggplot(dat %>% filter(year>2000), aes(x=deathtoll+1, y=nsegments))+
  geom_point(aes(color=prop_local, shape=deathtoll_source))+
  scale_x_log10()+
  facet_grid(rows=vars(HDIclass))+
  geom_smooth(method="lm")+
  scale_y_sqrt()

mylm=lm(sqrt(nsegments)~log(deathtoll+1)*HDIclass+0,data=dat)
anova(mylm)

events_deathtoll=wd_events %>% select(flood,deathtoll) %>% unique()
wp_pages=wp_pages %>% 
  left_join(wd_events %>% select(flood,deathtoll) %>% unique(),by="flood",relationship="many-to-many")
truc=wp_pages%>% 
  mutate(mentions_death=stringr::str_detect(textt,"dead|death|decease|kill")) %>%
  mutate(text_death=str_extract(textt,".{50}(dead|death|decease|kill).{50}")) %>% 
  mutate(number_dead=str_extract(text_death,
                                 "(\\d|\\,)*(?=(\\w|\\s)*(dead|death|decease|kill))")) %>% 
  filter(mentions_death) %>%
  select(flood,deathtoll,text_death,number_dead) %>% 
  filter(!is.na(text_death) & is.na(deathtoll))


View(truc)

