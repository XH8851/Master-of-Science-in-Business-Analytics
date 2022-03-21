# Who sufferred?

# Loading Data --------

setwd("...")
library(readxl)
df=read_excel('Terrorism.xlsx')

# Variable Overview
names(df)



# Geographic view of events---------

library(ggplot2)
library(dplyr)
library(maps)

df1=select(df, eventid, latitude, longitude)
df1$latitude=as.numeric(df1$latitude)
df1$longtitude=as.numeric(df1$longitude)
df1=filter(df1, df1$latitude<1000&df1$longtitude<1000)

ggplot(df1, aes(x=longitude, y=latitude))+
  borders("world",size=.7)+
  geom_point(color='#7E0C04', alpha=.02)+
  scale_y_continuous(breaks=NULL)+
  scale_x_continuous(breaks=NULL)+
  labs(title='The World Map of Terrorist Attacks in 1998-2017',
       subtitle='Most attack events happened in Europe, Asia, Middle East, Africa and South America.')



# The time-series analysis of attacks --------

df2=select(df, eventid, iyear, region, region_txt)
df2$eventid=as.character(df2$eventid)
df2_group=aggregate(eventid~iyear+region+region_txt, df2, length)
ggplot(df2_group, aes(x=iyear, y=eventid, colour=factor(region_txt)))+
  geom_line(size=.4)+
  geom_vline(aes(xintercept=2014), colour='#7E0C04', linetype='dashed')+
  xlab('Year')+
  ylab('Attack Frequency')+
  scale_x_continuous(breaks=seq(2000, 2017, 5))+
  scale_y_continuous(breaks=seq(0, 7000, 1000))+
  labs(title='Annual Attack Frequency in Regions',
       subtitle='In 1998-2017, the annual attack frequency remained high in both Eastern Europe and South Asia.\nSub-saharan Africa, Southeast Asia and Middle East & North Africa also suffered a lot.')+
  labs(color='Region')



# Target victims ---------

df3=select(df, eventid, iyear, targtype1, targtype1_txt)
df3_group=aggregate(eventid~iyear+targtype1+targtype1_txt, df3, length)
ggplot(df3_group,aes(x=iyear, y=eventid, color=factor(targtype1_txt)))+
  geom_bar(stat='identity', position='fill')+
  scale_y_continuous(labels=scales::percent)+
  xlab('Year')+
  ylab('Attack Frequency')+
  labs(title='Annual Frequency (%) of Target Victims',
       subtitle='All suffered, especially government and police. \nCitizens suffered most.',
       color='Target Type')

# Weapons Targeting Citizen

library(treemapify)

df4=select(df, eventid, iyear,targtype1, weaptype1, weaptype1_txt)
df4=df4[df4$targtype1==14,]
df4_group=aggregate(eventid~weaptype1+weaptype1_txt, df4, length)
df4_group$weaptype1_txt=as.factor(df4_group$weaptype1_txt)
names(df4_group)[names(df4_group)=='eventid'] = 'Weapon Frequency'
ggplot(df4_group, aes(area=`Weapon Frequency`, label = weaptype1_txt, fill=`Weapon Frequency`))+
  geom_treemap()+
  geom_treemap_text(fontface='italic',place='centre', colour='white')+
  scale_fill_distiller(type='seq',palette=14,direction=1)+
  labs(title='The Frequency of Weapons Targeting Citizens in 1998-2017',
       subtitle='Citizens suffered from explosives and firearms mostly.  \n')
  

