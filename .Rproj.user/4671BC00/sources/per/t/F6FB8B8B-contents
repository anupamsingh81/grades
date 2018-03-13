devtools::install_github("ankitrohatgi/digitizeR")

#https://github.com/ankitrohatgi/digitizeR

library(digitizeR)
app <- wpd.launch()

wpd.close(app)

library(tidyverse)
ak  =c(2004:2015)

ak1=paste0(ak,".csv")

library(tidyverse)

gg=function(x){
 x= read_csv(x,col_names = FALSE)

}
gg(ak1[1])
 

am=
  map_df(ak1,gg)

names(am)=c("marks","percent")

length(read_lines('2004.csv'))

# function to red lines 
rl =function(x){length(read_lines(x))}

rl(ak1[1])

lengthr=map_dbl(ak1,rl)

lengthr

# combining length and repeat to vector
am$year=map2(ak,lengthr,rep) %>% unlist %>% as.vector

summary(am$percent)

am$percent=ifelse(am$percent<0,0,am$percent)

am$marks=if_else(am$marks<0,0,am$marks)


am=am%>% mutate(number=as.integer(100*percent))


jj=map2(am$marks,am$number,rep) %>% unlist %>% as.vector()

kl=am %>% group_by(year) %>% summarize(tot=sum(number))

kk = map2(kl$year,kl$tot,rep) %>% unlist %>% as.vector()

pp = tibble(marks=jj,year=kk)

options(scipen=999)

library(ggridges)

pp$year = as.character(pp$year)

ggplot(pp, aes(x = marks, y = year,fill=year)) + geom_density_ridges2() 


ggplot(pp, aes(x = marks, y = year,fill=year)) + geom_density_ridges2() +xlim(70,100)


pp$grade = cut_interval(pp$marks,n=10)

summary(pp$grade)

summary(pp$grade)

pp %>% group_by(year) %>% count()

########purrcent 

pp  %>% group_by(grade,year) %>% 
  summarise(n=n()) %>% 
  group_by(year) %>% 
  mutate(percent=round(100*n/sum(n),2)) %>% arrange(desc(year)) %>% arrange(desc(grade)) %>% ungroup() %>% 
  slice(1:48) %>% 
  group_by(year) %>% #View()
 # mutate(purr=percent) %>% ungroup() %>% slice(1:72)
  
  ggplot(aes(year,percent,color=grade,group=grade))+geom_line()
  
  
  
  
  pp  %>% group_by(grade,year) %>% 
    summarise(n=n()) %>% 
    group_by(year) %>% 
    mutate(percent=round(100*n/sum(n),2)) %>% arrange(desc(year)) %>% arrange(desc(grade)) %>% ungroup() %>% 
    #slice(1:48) %>% 
    group_by(year) %>% 
    mutate(puro=cumsum(percent)) %>% ungroup() %>% #slice(1:48) %>% 
    ggplot(aes(year,puro,color=grade,group=grade))+geom_line()
  
  
  library(ggthemes)
  
  # area plot (automatically sums percent)
  
  pp  %>% group_by(grade,year) %>% 
    summarise(n=n()) %>% 
    group_by(year) %>% 
    mutate(percent=round(100*n/sum(n),2)) %>% arrange(desc(year)) %>% arrange(desc(grade)) %>% ungroup() %>% 
    #slice(1:48) %>% 
    group_by(year) %>% 
    mutate(puro=cumsum(percent)) %>% ungroup() %>% slice(1:72) %>% View()
    ggplot(aes(x=as.numeric(year),y=percent,fill=grade)) + # as.numeric
    geom_area()+
    #ggplot(aes(y=puro,x=year))+geom_area(stat="identity")+
    scale_fill_discrete(name="grade",labels=c("A1","A2","B1","B2"))+
    theme_economist()
  
  ##cumulative percent 
  pp  %>% group_by(grade,year) %>% 
    summarise(n=n()) %>% 
    group_by(year) %>% 
    mutate(percent=round(100*n/sum(n),2)) %>% arrange(desc(year)) %>% arrange(desc(grade)) %>% ungroup() %>% 
    #slice(1:48) %>% 
    group_by(year) %>% 
    mutate(puro=cumsum(percent)) %>% ungroup() %>% slice(1:48) %>% 
    ggplot(aes(year,puro,color=grade,group=grade))+geom_line()+
    scale_color_discrete(name="grage",labels=c("A1","A2","B1","B2"))+
    theme_economist()
  
  #animation
  pp  %>% group_by(grade,year) %>% 
    summarise(n=n()) %>% 
    group_by(year) %>% 
    mutate(percent=round(100*n/sum(n),2)) %>% arrange(desc(year)) %>% arrange(desc(grade)) %>% ungroup() %>% 
    #slice(1:48) %>% 
    group_by(year) %>% 
    mutate(puro=cumsum(percent)) %>% ungroup() %>% slice(1:48) %>% 
    ggplot(aes(year,puro,color=grade,frame=year))+geom_path(aes(cumulative = TRUE, group = grade))+
    scale_color_discrete(name="grage",labels=c("A1","A2","B1","B2"))+
    theme_economist()
  
 p1= 
 
 
 library(gganimate)
 library(animation)
  
 ani.options(ani.width=800, ani.height=1000) # animattion width height
 gm =gganimate(p1)
 
 gganimate_save(gm,"pmr.gif")
 
 ####oddsr
 
 
 pk= pp  %>% group_by(grade,year) %>% 
   summarise(n=n()) %>% 
   group_by(year) %>% 
   mutate(percent=round(100*n/sum(n),2)) %>% arrange(desc(year)) %>% arrange(desc(grade)) %>% ungroup() %>% 
   #slice(1:48) %>% 
   group_by(year) %>% 
   mutate(puro=cumsum(percent)) %>% ungroup() %>% slice(1:72) 
 
 ninty= pk %>% filter(grade=="(90,100]") %>% arrange(year) %>% mutate(relativerisk=puro/puro[1]) %>% mutate(grade=as.character(grade))
 
 levels(pk$grade)
 
 
 
 
 #####################
 filter_func <- function( filter_exp) {
   filter_exp_enq <- enquo(filter_exp)
   
   cat(filter_exp)
   
   pk %>% filter(grade==!!filter_exp_enq)%>% arrange(year) %>% mutate(relativerisk=puro/puro[1])
   
 }
 
 filter_func("(90,100]")
 
 #works i was printing 90,100 as 90-100 , wasted my 30 mins
 
 
 library(rlang)
 dplfl= function(x,y){
   x =sym(x)
   y=sym(y)
   pk %>% filter((!!x)==(!!y))%>% ## double bracket important around !!
      arrange(year) %>% mutate(relativerisk=puro/puro[1])
   
   
 }
 
 dplfl(year,2004)
 
 cl = sym("puro")
 cls = sym("7.8")
 
 kna=paste0("puro","<","7.8")
 kna
 
 
 filterf <- function( x) {
   kma= paste0("\"",x,"\"")
   cat(kma)
   #pk %>% filter_(kma)%>% mutate(relativerisk=puro/puro[1])
   
 }
 
 filterf("(90,100]")
 
 pk %>% filter(grade=="(90,100]")
 
levels(pm$grade)=c("90","80","70","60","50","33")

pmer$grade = recode_factor(pmer$grade,"(90-100]"="90")
 
 write.csv(pk,"pk1.csv")
 pmer=read.csv("pk1.csv",stringsAsFactors = FALSE)
 pmer %>% mutate(grade=as.character(grade)) %>% filter(grade=="(90-100]")
               
 filterf("(90-100]")
 
 #########useless..
 
 filter_func("(90,100]")
 av =  levels(pk$grade)
 
 av= av[5:10]
 
 pk
 
 av
 ?map
pka = map(.x=av,.f=filter_func) %>% reduce(full_join) %>% arrange(desc(grade)) 


# reduce to combine multiple data frmaw

colm=rev(c("red","green","blue","orange","yellow","black")) # reversed because order opposite

##plot

?scale_color_discrete

library(hrbrthemes)
library(tidyverse)


colm
colm=rev(colm)
levels(pka$grade)
#####hrbr

pka %>% # slice(1:48) %>% 
  ggplot(aes(year,relativerisk,color=grade,group=grade))+geom_line() +
scale_color_discrete(name="GRADE",labels=c("90+","80+","70+","60+","50+","33+"))+
  scale_color_manual(values=colm,name="GRADES",labels=c("33+","50+","60+","70+","80+","90+"))+
  #theme_ipsum(grid="Y")+
  theme(legend.position="bottom", legend.box = "horizontal") 

#panel.grid.major = element_blank())#panel.grid.minor = element_blank())


########bluebg
  
pka %>% # slice(1:48) %>% 
  ggplot(aes(year,relativerisk,color=grade,group=grade))+geom_line() +
  #scale_color_manual(values=colm)+
  #scale_color_discrete(name="GRADE",labels=c("90+","80+","70+","60+","50+","33+"))+
  scale_color_manual(values=colm,name="GRADES",labels=c("90+","80+","70+","60+","50+","33+"))+

theme(
  panel.background = element_rect(fill = "lightblue",
                                  colour = "lightblue",
                                  size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)+
  theme(legend.position="bottom", legend.box = "horizontal",
panel.grid.major = element_blank())
  
  
  library(devtools)

devtools::install_github("hrbrmstr/hrbrthemes")
  
+

  theme_economist()
  
?scal


 