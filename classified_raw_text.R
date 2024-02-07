wp_lemma=readRDS("data/wp_pages.RDS") %>%
  tidytext::unnest_tokens(output="word",input="textt",token="words") %>%
  left_join(lexicon_en,by=c("word")) %>%
  mutate(keep=(type!="sw" & lemma!="flood")) %>% 
  #filter(keep) %>% 
  group_by(flood,flood_label,article) %>%
  mutate(lemma=case_when(keep~lemma,
                         !keep~"")) %>% 
  mutate(keep=as.numeric(keep)) %>% 
  mutate(num_lemma=case_when(is.na(keep)~0,
                   TRUE~keep)) %>% 
  mutate(num_lemma=cumsum(num_lemma)) %>% 
  mutate(num_segment=ceiling(num_lemma/50)) %>% 
  summarise(text_all=paste0(word, collapse=" "),
            text_sig=paste0(lemma,collapse=" "),
            .groups="drop")

wp_lemma %>%
  filter(article=="https://en.wikipedia.org/wiki/2020_Central_Vietnam_floods") %>% 
  select(text_all,text_sig) %>% 
  View()

wp_segments %>% 
  filter(article=="https://en.wikipedia.org/wiki/2020_Central_Vietnam_floods") %>% 
  select(segment_source,class,class_name)

which(names(corpus)=="https://en.wikipedia.org/wiki/2020_Central_Vietnam_floods_1_1")
corpus[[1]]
