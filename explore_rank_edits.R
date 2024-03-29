library(tidyverse)
set.seed(123)
wp_revisions=readRDS("data/wp_revisions.RDS")
f=function(x){
  if(is.na(x)){return(NA)}
  if(x==0){y=0}
  if(x>0){y=log(x+1)}
  if(x<0){y=-log(-x+1)}
  return(y)
}
wp_revisions=wp_revisions %>% 
  group_by(article) %>% 
  mutate(rank=1:n()) %>% 
  ungroup() %>% 
  mutate(crank=cut(rank,quantile(rank,seq(0,1,by=0.1)),include.lowest=TRUE)) %>% 
  mutate(felta=purrr::map_dbl(delta,f))
tibsum=wp_revisions %>% 
  group_by(crank) %>% 
  summarise(meanfelta=median(felta,na.rm=TRUE),
            meandelta=mean(delta,na.rm=TRUE))
ggplot(tib, aes(x=crank, y = felta))+
  geom_boxplot()+
  geom_point(data=tibsum,col="red")#+


wp_revisions=wp_revisions %>% 
  left_join(tibsum %>% select(crank,meandelta),by=c("crank")) %>% 
  mutate(sizepred=cumsum(meandelta))

wp_p=wp_revisions %>% 
  group_by(article) %>% 
  summarise(size=last(size),
            sizepred=last(sizepred))
ggplot(wp_p, aes(x=size,y=sizepred))+
  geom_point()+
  scale_x_log10()+scale_y_log10()+
  geom_smooth()+
  geom_abline(slope=1,intercept=0,col="red")
#  scale_y_continuous(limits=c(0,5))
#   mutate(signdelta=delta>0) %>% 
#   group_by(crank) %>%
#   summarise(n=n(),
#             npos=length(which(signdelta==TRUE)),
#             meandelta=mean(delta,na.rm=TRUE)) %>% 
#   mutate(prop=npos/n)

tib

ggplot(tib, aes(rank,delta))+geom_smooth(method="loess")

ggplot(tib, aes(x=crank, y=delta))+
  geom_boxplot()+
  scale_y_continuous(limits=c(-20,20))
