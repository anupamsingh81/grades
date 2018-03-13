library(digitizeR)

app <- wpd.launch()



plot(res_2005$V1,res_2005$V2)

library(tidyverse)

res_2005$V1

res_2005 = 
res_2005 %>% rename(marks=V1,percent=V2) %>% 

 ggplot(aes(marks,percent))+     #stat_smooth()
  geom_line()

res_2005 %>% filter(V1<81)

pnorm(60,mean(res_2005$V1),sd(res_2005$V1))

mean(res_2005$V1)

r1=res_2005 %>% rename(marks=V1,percent=V2) %>% mutate(percent=abs(as.integer(100*percent)),marks=floor(marks))

jj=map2(.x=r1$marks,.y=r1$percent,.f=rep)

kk=jj %>% unlist()

median(kk)

