
# Data analysis -----------------------------------------------------------


library(plyr)
library(ggplot2)

benthic<-read.csv("post_bleaching_benthic_dataset_for_analysis.csv",header = T,stringsAsFactors = F)

#for now you don't need to include zone but when Reunion data comes in you may

test<-ddply(benthic,c(1:7),summarise,
            total=sum(mean_cover,na.rm = T),
            n=length(Year))

codes<-read.csv("benthic_codes_all.csv",header=T,stringsAsFactors = F)

benthic$level1_code<-codes$Level1_code[match(benthic$benthic_code, codes$Code)]   #add the number of transects to each site

ben_lev1<-ddply(benthic,c(1:7,14),summarise,
                cover=sum(mean_cover,na.rm = T))

ben_lev1$Reef.zone[which(ben_lev1$Reef.zone=="")]<-NA

ben_lev1$Period<-factor(ben_lev1$Period,levels=c("Pre","Post"))     #USED FOR PLOT 1 AND 5

ben_lev2<-ddply(ben_lev1,c(1,2,7,8),summarise,
                mean_cover=mean(cover,na.rm = T),
                sd=sd(cover,na.rm = T))

# #To prepare the necessary hard coral and algae dataframes ---------------

#for those stations that have both FA and HC data

alg_lev1<-ben_lev1[which(ben_lev1$level1_code=='ALG'|ben_lev1$level1_code=='AMAC'|ben_lev1$level1_code=='ATRF'|ben_lev1$level1_code=='AHAL' ),]

alg2<-ddply(alg_lev1,1:7,summarise,
            FA=sum(cover,na.rm = T))

alg2$site.id<-paste(alg2$Year,alg2$Country,alg2$Site,alg2$Station,alg2$Reef.zone)

#now need the HC equivalent for each station
ben_HC<-ben_lev1[which(ben_lev1$level1_code=='HC'),]

ben_HC$site.id<-paste(ben_HC$Year,ben_HC$Country,ben_HC$Site,ben_HC$Station,ben_HC$Reef.zone)

alg2$HC <- ben_HC$cover[match(alg2$site.id, ben_HC$site.id)]

#ALG2 is for the trendline graphs and contains only sites with both fa and hc data

#start with alg2 that was created for the trend lines with both FA and HC plotted

alg2$Period<-factor(alg2$Period,levels=c("Pre","Post"))

alg2<-alg2[,c(1:7,9,8,10)]  #used for PLOT 2

#to clean alg2 to ensure that only those stations that have both pre and post data and fa and hc data are included, because previously they were not

alg2$site.id<-paste(alg2$Country,alg2$Site,alg2$Station,sep="_")

pre<-alg2[which(alg2$Period=='Pre'),]
post<-alg2[which(alg2$Period=='Post'),]

pre_sites<-unique(pre[c("Country", "Site", "Station")])
post_sites<-unique(post[c("Country", "Site", "Station")])

ftable(pre_sites$Country)
ftable(post_sites$Country)

# pre_sites$site.id<-paste(pre_sites$Country,pre_sites$Site,pre_sites$Station,pre_sites$Reef.zone,sep="_")
# post_sites$site.id<-paste(post_sites$Country,post_sites$Site,post_sites$Station,post_sites$Reef.zone,sep="_")

# post<-k[which(k$Period=="Post"),]
# pre<-k[which(k$Period=='Pre'),]

common <- intersect(pre$site.id, post$site.id)

y<-match(alg2$site.id,common,nomatch = 0)
r_alg<-alg2[which(y!=0),]

#r_alg is now the clean, useful alternative to alg2

pre2<-r_alg[which(r_alg$Period=='Pre'),]
post2<-r_alg[which(r_alg$Period=='Post'),]

pre_sites2<-unique(pre2[c("Country", "Site", "Station")])
post_sites2<-unique(post2[c("Country", "Site", "Station")])

ftable(pre_sites2$Country)
ftable(post_sites2$Country)

#it works

alg2<-r_alg  #so that we dont have to change code below

#to make it wide to long with a benthic_category column with both hc and fa
library(tidyr)
data_long <- gather(alg2, benthic_category, mean_cover, FA:HC, factor_key=TRUE)

algae_stations_trend<-unique(alg2[c("Country", "Site", "Station")])
alg_st_tr1<-algae_stations_trend[order(algae_stations_trend[1],algae_stations_trend[2],algae_stations_trend[3]),]


#At some point need to decide for the pre-bleaching and post-bleaching
#To filter the data 
#Step 1: for each site/station, select the most recent value for Hard coral and Fleshy Algae -in each Period
#Step 2: for pre-bleaching have a cut-off Year before which data is not considered
#Step 3: Select only those sites that have data for both periods after this cut-off year

data_long<-data_long[which(data_long$Year>=2012),]

#remove stations which likely have erronous data - recent cover and post cover too different

data_long1<-data_long[-which(data_long$Station=='Iweni'|data_long$Station=='Mpunguti-Upper'|data_long$Station=='Tausi'),]


algae_r<-ddply(data_long1,c("Country","Site","Station","Period","benthic_category"),summarise,
               recent_year=max(Year),
               no_years_benthic=length(unique(Year)),
               recent_cover=tail(mean_cover,n=1),
               ave_cover=mean(mean_cover))

#this loop will help use mean cover across years for pre bleaching period, and recent cover values for post-bleaching period
for (i in 1:nrow(algae_r)){
  if (algae_r$Period[i]=='Pre'){
algae_r$cover[i]<-algae_r$ave_cover[i]}
  else {algae_r$cover[i]<-algae_r$recent_cover[i]}
}

#because of the steps taken to produce alg2, there should be an algae value for each hard coral value for each site, station ,year
ftable(algae_r$benthic_category) #just to be sure - should be equal

#step 2 - for accuracy purposes lets remove data earlier than 2012

algae_r1<-algae_r[which(algae_r$recent_year>=2012),]

#step 3- create a dataset that only contains those sites with both pre and post
#zone for seychelles stations is not consistent for pre vs post so dont include in lin below

#this basically filters out all those stations that only have data for one period and not both
#filter has to be set to n>2. if n>4, then watamu 3 sites surveyed twice post, and chumbe (cause of zone differences) are 4 sites that come out as issues - by removing 'zone' when producing algae_r corrects for this. 

library(dplyr)
algae_r2 <-  algae_r1 %>% group_by(paste(algae_r1$Site,algae_r1$Station,sep="")) %>% filter(n()>2) #

algae_r2$benthic_category<-factor(algae_r2$benthic_category,c("HC","FA"))   #USED FOR PLOT 3

sites<-unique(algae_r2[c("Country", "Site", "Station")])    #number of sites involved in the analysis
ftable(sites$Country)


#to get table with overall national mean for coral and algae cover pre and post with standard errors
national_ave<-ddply(algae_r2,c("Country","Period","benthic_category"),summarise,
                    mean_cover=mean(recent_cover),
                    se=sd(recent_cover)/sqrt(length(Site)))

national <- national_ave%>%
  group_by(Country,benthic_category) %>%
  arrange(Period) %>%
  mutate(pct.chg = ((mean_cover - lag(mean_cover))/lag(mean_cover))*100)

national2<-national[order(national$Country,national$benthic_category),]

# national2<-national[-which(is.na(national$pct.chg)),]
# national3<-national2[,c(1,3,5)]


#regional
#To create a seperate dataset from ben_lev1 with just the pre-bleaching data from latest year for each station and the post-bleac
#data equivalent 

#sites with data in both periods from 2012 onwards - just hard coral cover
#r2 is just hard coral sites - used for Seychelles analysis and any other pre vs post that is only coral
r<-ddply(ben_lev1,c("Country","Site","Station","Reef.zone","Period","level1_code"),summarise,
         recent_year=max(Year),
         no_years_benthic=length(unique(Year)),
         recent_Coral=tail(cover,n=1))

#step 2 - for plotting purposes lets remove data earlier than 2012 for example

r1<-r[which(r$recent_year>=2012),]

#step 3- create a dataset that only contains those sites with both pre and post
#zone for seychelles stations is not consistent for pre vs post so dont include in lin below

r1<-r1[which(r1$level1_code=='HC'),]

#this basically filters out all those stations that only have data for one period and not both
library(dplyr)
r2 <-  r1 %>% group_by(paste(r1$Site,r1$Station,sep="")) %>% filter(n()>1) #
r2   #USED FOR PLOT 4 AND SEYCHELLES INNER VS OUTER PLOTS

#at this point when we can use r2 to try and get the difference in coral cover for each station

#these are all stations that have data for both periods - the latest values in each period only as well
r2$site.id<-paste(r2$Site,r2$Station,r2$Reef.zone,sep="_")


r2$Period<-factor(r2$Period,levels=c("Pre","Post"))
r3 <- r2%>%
  group_by(site.id) %>%
  arrange(Station,Period) %>%
  mutate(pct.chg = ((recent_Coral - lag(recent_Coral))/lag(recent_Coral))*100) %>%
  mutate(year_gap = recent_year - lag(recent_year))%>%
  mutate(post_coral_cover=recent_Coral) %>%
  mutate(pre_coral_cover=lag(recent_Coral))


r4<-r3[-which(is.na(r3$pct.chg)),]
# r2<-r2[which(r2$diff!=0),]
r5<-r4[,c(1,2,3,4,15,14,13,12)]    

r6<-r5[order(r5$Country,r5$pct.chg),]



# #find actual average values pre and post
# 
# r3<-ddply(r2,c("Period"),summarise,
#           ave_cover=mean(recent_Coral,na.rm = T))
# ((r3[2,2]-r3[1,2])/r3[1,2])*100    #calculate percentage change in coral cover


# File saving -------------------------------------------------------------


write.csv(ben_lev2[which(ben_lev2$Country=="Kenya"),],"Kenya_summarised_benthic_data.csv",row.names = F)

write.csv(ben_lev1[which(ben_lev1$Country=="Kenya"),],"Kenya_summarised_benthic_data_sites_level1.csv",row.names = F)

write.csv(alg_st_tr1,"All_stations_used_in_FA_HC_trend_graphs.csv",row.names = F)

write.csv(sites,"All_stations_used_in_FA_HC_bar_graphs.csv",row.names = F)

write.csv(national2,"national_averages_of_algae_and_coral_pre_vs_post.csv",row.names = F)

write.csv(r6,"hard_coral_cover_changes_at_each_station.csv",row.names = F)

# Plot 1: HC trend with Station lines - per country ---------------------------------------------


p<-NA
p<-ggplot(data=ben_lev1[which(ben_lev1$level1_code=='HC'),],aes(x=Year,y=cover,group=Period,colour=Period,shape=Period))
p <- p + geom_line(aes(group=Station),size=1,alpha=0.2)
p<- p + stat_summary(geom="ribbon", fun.data=mean_cl_boot, 
                     fun.args=list(conf.int=0.95), fill="grey",alpha=0.5,colour=NA)
p <- p +  stat_summary(geom="line", fun.y=mean, linetype="solid", size=1.2,alpha=1)
p<- p + stat_summary(geom="point", fun.y=mean,size=2,fill='white')
p <- p + theme_bw()+theme(plot.title=element_text(size=9),
                          axis.line = element_line(size=1,colour = "black"),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.border = element_blank(),
                          panel.background = element_blank(),
                          text = element_text(size=9,face="bold"),
                          axis.ticks.length=unit(0.2,"cm"),
                          axis.text = element_text(colour='black')) 
p<- p + ylab("Hard Coral Cover (%)")
p<- p + scale_x_continuous(breaks=seq(1992,2017,by=1),
                           labels = c(1992, rep("",3),1996,rep("",3),2000,rep("",3),2004,rep("",3),2008,rep("",3),2012,rep("",3),2016,rep("",1)))
# limits = c(1992,2016))
p<- p + scale_y_continuous(breaks = seq(0,100,by=5),limits=c(0,100),
                           labels=c(0,"",10,"",20,"",30,"",40,"",50,"",60,"",70,"",80,"",90,"",100))
p<- p + expand_limits(y=0)
# p<-p + ylim(y=100)
# p<-p+ expand_limits(x=2016)
p<- p + facet_wrap(~Country, scales="free_x", ncol=3)+theme(                 strip.text.x = element_blank(),
                                                                             strip.background = element_blank(),
                                                                             panel.grid.major = element_blank(),
                                                                             panel.grid.minor = element_blank(),
                                                                             panel.border = element_blank(),
                                                                             panel.background = element_blank(),
                                                                             plot.margin = unit(c(0.5,0.5,0,0.5), "lines"))
p<- p + theme(legend.key = element_blank(),
              legend.position="bottom",
              legend.key.height=unit(0.2, "cm"),
              plot.margin = unit(c(0,0.1,0.1,0.1), "lines"),
              #               legend.margin=unit(0.5,"cm"),
              legend.text=element_text(size=8,colour = 'black'))
p<-p+scale_colour_manual("Period", 
                         breaks = c("Pre", "Post"),
                         values = c("Pre" = "purple", "Post" = "blue"),
                         labels = c("Pre"="Pre", "Post"='Post (July 2016 on)'))

p<-p+scale_shape_manual("Period", 
                        breaks = c("Pre", "Post"),
                        values = c("Pre" = 21, "Post" = 22),
                        labels = c("Pre"="Pre", "Post"='Post (July 2016 on)'))

print(p)

s<-dlply(ben_lev1[which(ben_lev1$level1_code=='HC'),], .(Country), function(x) p %+% x)
g<-unique(ben_lev1[which(ben_lev1$level1_code=='HC'),]$Country)

for (i in 1:length(unique(ben_lev1[which(ben_lev1$level1_code=='HC'),]$Country))){
  jpeg(paste(g[i],"HC_station_lines.jpeg"), width = 4, height = 3.5, units = 'in', res = 300)
  print(s[i])
  dev.off()
}


# Plot 2: HC & FA -TREND LINE national plots ------------------------------------------------


p<-NA 

p<- ggplot(alg2, aes(x=Year,group=Period))
p<-p +  stat_summary(aes(y=HC, colour='HC',shape='HC',linetype=Period),geom="smooth", fun.y=mean, size=1)
p<-p +  stat_summary(geom="ribbon", fun.data=mean_cl_boot,fun.args=list(conf.int=0.95), aes(y=HC,fill="HC"),alpha=0.3)
p<- p+  stat_summary(geom="point", fun.y=mean, fill='white', aes(y=HC,shape='HC',color='HC'),size=2)


p<- p +  stat_summary(aes(y=FA, colour='FA',linetype=Period),geom="smooth", fun.y=mean, size=1)
p<- p +  stat_summary(aes(y=FA,fill='FA'),geom="ribbon", fun.data=mean_cl_boot, 
                      fun.args=list(conf.int=0.95),alpha=0.3)
p<-p +  stat_summary(aes(y=FA,color='FA',shape='FA'),geom="point", fun.y=mean,size=2, fill='white')

p <- p + theme_bw()+theme(plot.title=element_blank(),
                          axis.line = element_line(size=1,colour = "black"),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.border = element_blank(),
                          panel.background = element_blank(),
                          text = element_text(size=9,face="bold"),
                          axis.ticks.length=unit(0.2,"cm"),
                          axis.text = element_text(colour="black"))
p<- p + theme(legend.key = element_blank(),
              legend.position="bottom",
              legend.key.height=unit(0, "cm"),
              # legend.justification="left",
              legend.spacing =unit(0,"cm"),
              legend.text = element_text(size=8))

#p<- p + ggtitle(e[i])
p<- p + ylab("Cover (%)")
p<- p + scale_x_continuous(breaks=seq(1996,2017,by=1),
                           labels = c(1996,rep("",3),2000,rep("",3),2004,rep("",3),2008,rep("",3),2012,rep("",3),2016,rep("",1)))
p<- p + scale_y_continuous(breaks = seq(0,100,by=5),
                           labels=c(0,"",10,"",20,"",30,"",40,"",50,"",60,"",70,"",80,"",90,"",100))
p<- p + theme(strip.text.x = element_text(size=11,face="bold"),
              strip.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())
p<- p + expand_limits(y=0)
p<- p + facet_wrap(~Country, scales="free", ncol=2)+theme(strip.text.x = element_blank(),
                                                          strip.background = element_blank(),
                                                          panel.grid.major = element_blank(),
                                                          panel.grid.minor = element_blank(),
                                                          panel.border = element_blank(),
                                                          panel.background = element_blank(),
                                                          plot.margin = unit(c(0.5,0.7,0,0.5), "lines"))

p<-p+scale_fill_manual("Cover type", 
                       breaks = c("HC", "FA"),
                       values = c('HC'="blue", "FA"="green"),
                       labels = c('HC'='Hard Coral', "FA"="Fleshy Algae"))

p<-p+ scale_colour_manual("Cover type", 
                          breaks = c("HC", "FA"),
                          values = c("HC"="blue", "FA"="dark green"),
                          labels = c('HC'='Hard Coral', "FA"="Fleshy Algae"))
p<-p+scale_shape_manual("Cover type", 
                        breaks = c("HC", "FA"),
                        values = c('HC'= 21, "FA"= 19),
                        labels = c('HC'='Hard Coral', "FA"="Fleshy Algae"))

p<-p+scale_linetype_manual("Period", 
                           breaks = c("Pre", "Post"),
                           values = c("Pre"='solid', "Post"='dotted'))
# p + guides(fill = guide_legend(override.aes = list(shape = 21)))


print(p)

library(plyr)
s<-dlply(alg2, .(Country), function(x) p %+% x)

for (i in 1:5){
  g<-unique(alg2$Country)
  jpeg(paste(g[i],"FA and HC together.jpeg"), width = 4.5, height = 3.5, units = 'in', res = 300)
  print(s[i])
  dev.off()
}


# Plot 3: HC & FA - pre vs post BAR PLOT ------------------------------------------

###plot
p<-NA 
p<- ggplot(algae_r2,aes(x=benthic_category,y=recent_cover,fill=Period))

p<- p + stat_summary(fun.y="mean", geom="bar",position=position_dodge(),alpha=0.8)
p<- p + stat_summary(fun.data = 'mean_se', geom = "errorbar",width=.2,size=0.8,position=position_dodge(.9))

p <- p + theme_bw()+theme(plot.title=element_blank(),
                          axis.line = element_line(size=1,colour = "black"),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.border = element_blank(),
                          panel.background = element_blank(),
                          text = element_text(size=9.5,face="bold"),
                          axis.ticks.length=unit(0.2,"cm"),
                          axis.text = element_text(colour="black"),
                          axis.title.x = element_blank())

p<- p + theme(legend.key = element_blank(),
              legend.position="bottom",
              legend.key.height=unit(0.2, "cm"),
              plot.margin = unit(c(0,0.1,0.1,0.1), "lines"),
              #               legend.margin=unit(0.5,"cm"),
              legend.text=element_text(size=8))

#p<- p + ggtitle(e[i])
p<- p + ylab("Cover (%)")

p<- p + theme(strip.text.x = element_text(size=11,face="bold"),
              strip.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())
p<- p + scale_y_continuous(breaks = seq(0,100,by=5),
                           labels=c(0,"",10,"",20,"",30,"",40,"",50,"",60,"",70,"",80,"",90,"",100))
p<-p + scale_x_discrete(breaks=c("HC","FA"),
                        labels=c("HC"="Coral","FA"="Algae"))
p<- p + expand_limits(y=0)
p<- p + facet_wrap(~Country)+theme(strip.text.x = element_blank(),
                                   strip.background = element_blank(),
                                   panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank(),
                                   panel.border = element_blank(),
                                   panel.background = element_blank(),
                                   plot.margin = unit(c(0.5,0.7,0,0.5), "lines"))
p<-p+ scale_fill_manual("Period", 
                        breaks = c("Pre", "Post"),
                        values = c("Pre"='dark blue', "Post"='dark red'),
                        labels = c("Pre"='pre-bleaching', "Post"='response'))

# p + guides(fill = guide_legend(override.aes = list(shape = 21)))


print(p)

library(plyr)
s<-dlply(algae_r2, .(Country), function(x) p %+% x)
g<-unique(algae_r2$Country)

for (i in 1:length(g)){
  jpeg(paste(g[i],"pre vs post FA and HC bar plot.jpeg"), width = 3, height = 3, units = 'in', res = 300)
  print(s[i])
  dev.off()
}
# Plot 4: Hard Coral only Pre vs post -REGIONAL bar plot --------------------------------------------------

library(Hmisc)
library(ggplot2)
library(dplyr)
library(plyr)
library(plotrix)


#PLOTTING PRE-BLEACHING FROM 2010 ONWARDS

p<-NA 

# p<- ggplot(data=ben_lev1[which(ben_lev1$level1_code=='HC' & ben_lev1$Year>=2010),],aes(x=level1_code,y=cover,fill=Period))
p<- ggplot(data=r2,aes(x=level1_code,y=recent_Coral,fill=Period))

# p<-p + geom_bar(stat= "summary", fun.y = "mean",position='fill')
p<- p + stat_summary(fun.y="mean", geom="bar",position=position_dodge(),alpha=0.8)
# p<- p + stat_summary(fun.data = mean_cl_boot, geom = "errorbar")
p<- p + stat_summary(fun.data = 'mean_se', geom = "errorbar",width=.2,size=0.8,position=position_dodge(.9))

p <- p + theme_bw()+theme(plot.title=element_blank(),
                          axis.line = element_line(size=1,colour = "black"),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.border = element_blank(),
                          panel.background = element_blank(),
                          axis.text = element_text(size=10,face="bold",colour='black'),
                          axis.ticks.length=unit(0.2,"cm"),
                          axis.ticks.x = element_blank(),
                          axis.title.x=element_blank(),
                          axis.text.x = element_blank(),
                          text = element_text(size=10,face="bold",colour='black'))

p<- p + theme(legend.key = element_blank(),
              legend.position="bottom",
              legend.key.height=unit(0.2, "cm"),
              plot.margin = unit(c(0,0.1,0.1,0.1), "lines"),
              #               legend.margin=unit(0.5,"cm"),
              legend.text=element_text(size=7,colour = 'black'))

#p<- p + ggtitle(e[i])
p<- p + ylab("Hard coral cover (%)")

# p<- p + theme(strip.text.x = element_text(size=11,face="bold"),
#               strip.background = element_blank(),
#               panel.grid.major = element_blank(),
#               panel.grid.minor = element_blank(),
#               panel.border = element_blank(),
#               panel.background = element_blank())
p<- p + scale_y_continuous(breaks = seq(0,100,by=5),
                           labels=c(0,"",10,"",20,"",30,"",40,"",50,"",60,"",70,"",80,"",90,"",100))
p<- p + expand_limits(y=0)
p<- p + facet_wrap(~Country, ncol=3)+theme(strip.text.x = element_blank(),
                                           strip.background = element_blank(),
                                           panel.grid.major = element_blank(),
                                           panel.grid.minor = element_blank(),
                                           panel.border = element_blank(),
                                           panel.background = element_blank(),
                                           plot.margin = unit(c(0.5,0.5,0,0.5), "lines"),
                                           text = element_text(colour='black'))

p<-p+ scale_fill_manual("Period", 
                        breaks = c("Pre", "Post"),
                        values = c("Pre"='dark blue', "Post"='dark red'),
                        labels = c("Pre"='pre-bleaching', "Post"='post-bleaching'))

# p + guides(fill = guide_legend(override.aes = list(shape = 21)))

p

#for regional plot
jpeg("regional_HC_column_chart_pre_vs_post_2010.jpeg", width = 3, height = 3, units = 'in', res = 300)
print(p)
dev.off()

#for individual countries
s<-NA
# s<-dlply(ben_lev1[which(ben_lev1$level1_code=='HC' & ben_lev1$Year>=2010),], .(Country), function(x) p %+% x)
s<-dlply(r2, .(Country), function(x) p %+% x)

g<-unique(r2$Country)

for (i in 1:length(unique(ben_lev1[which(ben_lev1$level1_code=='HC'),]$Country))){
  jpeg(paste(g[i],"HC_column_chart_pre_vs_post_2012.jpeg"), width = 3, height = 3, units = 'in', res = 300)
  print(s[i])
  dev.off()
}


# Plot 5: regional plot - hc trend all countries on single plot -------------------


#to get the linear regression equation - for region
ben_lev1$Year2<-ben_lev1$Year-1996  #create a new year category where 1992 is year 0
lin.mod <- lm(cover~Year2,ben_lev1)
summary(lin.mod)
coef(lin.mod)


q<-NA
q<- ggplot(ben_lev1[which(ben_lev1$level1_code=='HC'),], aes(x=Year, y=cover))
q<-q+  stat_summary(geom="ribbon", fun.data=mean_cl_boot, 
                    fun.args=list(conf.int=0.95), aes(fill="regional"),alpha=0.4,colour=NA)
q<-q+stat_summary(geom="smooth", fun.y=mean, size=1.2,aes(colour='regional',shape='regional',linetype="regional"))
q<-q+ stat_smooth(method = "lm",size=1,aes(colour='trendline',shape='trendline',fill='trendline',linetype='trendline'))

q<-q+  stat_summary(geom="smooth", fun.y=mean, size=1,aes(group=Country,shape=Country,colour=Country,fill=Country,linetype=Country))
q<-q+   stat_summary(geom="point", fun.y=mean,size=2,fill='white',aes(shape=Country,colour=Country))


q <- q + theme_bw()+theme(plot.title=element_text(size=10.5),
                          axis.line = element_line(size=1,colour = "black"),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.border = element_blank(),
                          panel.background = element_blank(),
                          text = element_text(size=10.5,face="bold"),
                          axis.ticks.length=unit(0.2,"cm"),
                          axis.text = element_text(colour='black'))

q<- q + theme(legend.key = element_blank(),
              legend.position="bottom",
              legend.key.height=unit(0.3, "cm"),
              legend.justification="left",
              plot.margin = unit(c(0,0.2,0,0.2), "lines"),
              legend.margin=unit(0, "cm"))

q <- q + ylab("Hard coral cover (%)")
# q<-q+ scale_colour_manual(values = c("Kenya" = "dark green", "La Reunion" = "red2",'Mozambique'='blue','Mauritius'='orange','Rodrigues'='orange','South Africa'='green',"Seychelles"='Purple',"Tanzania"="dark blue","Comoros"="grey40","Madagascar"="brown","regional"="black"))

q<-q+scale_colour_manual("Country", 
                         breaks = c("Kenya", "La Reunion", 'Mozambique','Mauritius','Rodrigues','South Africa',"Seychelles","Tanzania","Comoros","Madagascar",'regional',"trendline"),
                         values = c("Kenya" = "dark green", "La Reunion" = "red2",'Mozambique'='blue','Mauritius'='orange','Rodrigues'='orange','South Africa'='green',"Seychelles"='Purple',"Tanzania"="dark blue","Comoros"="grey40","Madagascar"="brown","regional"="black","trendline"="grey"))

q <- q + theme(plot.title = element_text(size = 12, face = "bold"))
q<- q + scale_x_continuous(breaks=seq(1996,2017,by=1),
                           labels = c(1996,rep("",3),2000,rep("",3),2004,rep("",3),2008,rep("",3),2012,rep("",3),2016,rep("",1)))
q <- q + scale_y_continuous(breaks = seq(0,100,by=5),
                            labels=c(0,"",10,"",20,"",30,"",40,"",50,"",60,"",70,"",80,"",90,"",100))

q<-q+ scale_shape_manual("Country", 
                         breaks = c("Kenya", "La Reunion", 'Mozambique','Mauritius','Rodrigues','South Africa',"Seychelles","Tanzania","Comoros","Madagascar",'regional','trendline'),
                         values=c("Kenya"= 21,"La Reunion"=22,'Mozambique'=19,'Mauritius'=23,'South Africa'= 24,"Seychelles"=25,"Tanzania"=17,"Madagascar"=15,"Comoros"=8,'regional'=NA,'trendline'=NA))

q<-q+ scale_fill_manual("Country", 
                        breaks = c("Kenya", "La Reunion", 'Mozambique','Mauritius','Rodrigues','South Africa',"Seychelles","Tanzania","Comoros","Madagascar",'regional','trendline'),
                        values=c("Kenya"= NA,"La Reunion"=NA,'Mozambique'=NA,'Mauritius'=NA,'South Africa'= NA,"Seychelles"=NA,"Tanzania"=NA,"Madagascar"=NA,"Comoros"=NA,'regional'='grey','trendline'=NA))
q<-q+ scale_linetype_manual("Country",
                            breaks = c("Kenya", "La Reunion", 'Mozambique','Mauritius','Rodrigues','South Africa',"Seychelles","Tanzania","Comoros","Madagascar",'regional','trendline'),
                            values=c("Kenya"= 'solid',"La Reunion"='solid','Mozambique'='solid','Mauritius'='solid','South Africa'= 'solid',"Seychelles"='solid',"Tanzania"='solid',"Madagascar"='solid',"Comoros"='solid','regional'='solid','trendline'='dashed'))
q<-q + expand_limits(y=0,x=1996)

q <- q + theme(plot.title = element_text(size = 12, face = "bold"))
q <- q + guides(fill = guide_legend(nrow=2))

q

jpeg("HC_single_plot_all_countries_lines.jpeg", width = 6.25, height = 4, units = 'in', res = 300)
q
dev.off()



# Seychelles sub-national analysis ----------------------------------------


seyc<-ben_lev1[which(ben_lev1$Country=='Seychelles'),]

seyc2<-r2[which(r2$Country=='Seychelles'),]    #sites with data for both periods from 2012

seyc2$island<-NA
seyc2$island[which(seyc2$Site=='Aldabra'|seyc2$Site=='Alphonse'|seyc2$Site=='Cerf Island'|seyc2$Site=='Desroches')]<-'Outer'
seyc2$island[which(seyc2$Site=='Mahe NW'|seyc2$Site=='North Reef')]<-'Inner'

seyc2$Period<-factor(seyc2$Period,levels=c("Pre","Post"))

sey_isl<-ddply(seyc2,c("Period","island"),summarise,
               mean_cover=mean(recent_Coral),
               se=sd(recent_Coral)/sqrt(length(Site)),
               n=length(Site))

df3 <- sey_isl%>%
  group_by(island) %>%
  arrange(Period) %>%
  mutate(pct.chg = (mean_cover - lag(mean_cover))/lag(mean_cover)*100)

write.csv(df3,"Seychelles_change_in_coral_cover_per_island_groups.csv",row.names =F)


#to get site level averages
ave_sey<-ddply(seyc2,c("Period","Site"),summarise,
               mean_cover=mean(recent_Coral),
               sd=sd(recent_Coral),
               Year=tail(recent_year,n=1))

ave_sey2<-ave_sey[order(ave_sey$Site,ave_sey$Period),]   #change in mean coral cover at each site

library(dplyr)

df2 <- ave_sey%>%
  group_by(Site) %>%
  arrange(Site,Period) %>%
  mutate(pct.chg = (mean_cover - lag(mean_cover))/lag(mean_cover)*100)

# df2$pct.chg[df2$Period=='Pre']<-NA

write.csv(df2,"Seychelles_change_in_coral_cover_per_site.csv",row.names =F)

#bar plot to show change in coral cover for inner and outer islands
p<-NA 
p<- ggplot(seyc2,aes(x=island,y=recent_Coral,fill=Period))
p<- p + stat_summary(fun.y="mean", geom="bar",position=position_dodge(),alpha=0.8)

p<- p + stat_summary(fun.data = 'mean_se', geom = "errorbar",width=.2,size=0.8,position=position_dodge(.9))
p

p <- p + theme_bw()+theme(plot.title=element_blank(),
                          axis.line = element_line(size=1,colour = "black"),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.border = element_blank(),
                          panel.background = element_blank(),
                          text = element_text(size=11,face="bold"),
                          axis.ticks.length=unit(0.2,"cm"),
                          axis.text = element_text(colour="black"),
                          axis.title.x = element_blank())

p<- p + theme(legend.key = element_blank(),
              legend.position="bottom",
              legend.key.height=unit(0.2, "cm"),
              plot.margin = unit(c(0,0.1,0.1,0.1), "lines"),
              #               legend.margin=unit(0.5,"cm"),
              legend.text=element_text(size=8))

#p<- p + ggtitle(e[i])
p<- p + ylab("Hard coral cover (%)")

p<- p + theme(strip.text.x = element_text(size=11,face="bold"),
              strip.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())
p<- p + scale_y_continuous(breaks = seq(0,100,by=5),
                           labels=c(0,"",10,"",20,"",30,"",40,"",50,"",60,"",70,"",80,"",90,"",100))
# p<-p + scale_x_discrete(breaks=c("HC","FA"),
#                         labels=c("HC"="Coral","FA"="Algae"))
p<- p + expand_limits(y=0)
p<-p+ scale_fill_manual("Period", 
                        breaks = c("Pre", "Post"),
                        values = c("Pre"='dark blue', "Post"='dark red'),
                        labels = c("Pre"='pre-bleaching', "Post"='response'))

# p + guides(fill = guide_legend(override.aes = list(shape = 21)))


print(p)

jpeg("Seychelles_inner_vs_outer_coral_cover.jpeg",res=300,height=4,width=4,units = 'in')
p
dev.off()


# Kenya - sub-national analysis -------------------------------------------

kenya<-ben_lev1[which(ben_lev1$Country=='Kenya'),]

ken2<-r2[which(r2$Country=='Kenya'),]    #sites with data for both periods from 2012

ken2$mgt<-NA

ken2$mgt[which(ken2$Site=='Watamu' | ken2$Site=="Malindi")]<-'MPA'
ken2$mgt[which(ken2$Site=='Malindi' & ken2$Station=="Mayunguni")]<-'Reserve'
ken2$mgt[which(ken2$Site=='Lamu')]<-'Open'
ken2$mgt[which(ken2$Site=='Lamu' & ken2$Station=="Iweni")]<-'CCA'
ken2$mgt[which(ken2$Site=='Mombasa')]<-'MPA'
ken2$mgt[which(ken2$Site=='Mombasa' & ken2$Station=="Nyali")]<-'CCA'
ken2$mgt[which(ken2$Site=='Mombasa' & ken2$Station=="Ras_Iwatine")]<-'CCA'
ken2$mgt[which(ken2$Site=='Shimoni')]<-'CCA'
ken2$mgt[which(ken2$Site=='Shimoni' & ken2$Station=="Mpunguti-Upper")]<-'Reserve'
ken2$mgt[which(ken2$Site=='Shimoni' & ken2$Station=="Mpunguti-Lower")]<-'Reserve'


ken2$mgt2<-"No-take"
ken2$mgt2[which(ken2$mgt=="Reserve" |ken2$mgt== "CCA" | ken2$mgt=='Open')]<-"other"
#to get percentage change - mangement regime

ken_mgt<-ddply(ken2,c("Period","mgt2"),summarise,
               mean_cover=mean(recent_Coral),
               se=sd(recent_Coral)/sqrt(length(Site)),
               n=length(Site))

ken3 <- ken_mgt%>%
  group_by(mgt2) %>%
  arrange(Period) %>%
  mutate(pct.chg = (mean_cover - lag(mean_cover))/lag(mean_cover)*100)

ken3<-ken3[order(ken3$mgt2),]
write.csv(ken3,"Kenya_change_in_coral_cover_per_management_groups.csv",row.names =F)

#bar plot to show change in coral cover for inner and outer islands
p<-NA 
p<- ggplot(ken2,aes(x=mgt2,y=recent_Coral,fill=Period))
p<- p + stat_summary(fun.y="mean", geom="bar",position=position_dodge(),alpha=0.8)

p<- p + stat_summary(fun.data = 'mean_se', geom = "errorbar",width=.2,size=0.8,position=position_dodge(.9))
p

p <- p + theme_bw()+theme(plot.title=element_blank(),
                          axis.line = element_line(size=1,colour = "black"),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.border = element_blank(),
                          panel.background = element_blank(),
                          text = element_text(size=11,face="bold"),
                          axis.ticks.length=unit(0.2,"cm"),
                          axis.text = element_text(colour="black"),
                          axis.title.x = element_blank())

p<- p + theme(legend.key = element_blank(),
              legend.position="bottom",
              legend.key.height=unit(0.2, "cm"),
              plot.margin = unit(c(0,0.1,0.1,0.1), "lines"),
              #               legend.margin=unit(0.5,"cm"),
              legend.text=element_text(size=8))

#p<- p + ggtitle(e[i])
p<- p + ylab("Hard coral cover (%)")

p<- p + theme(strip.text.x = element_text(size=11,face="bold"),
              strip.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())
p<- p + scale_y_continuous(breaks = seq(0,100,by=5),
                           labels=c(0,"",10,"",20,"",30,"",40,"",50,"",60,"",70,"",80,"",90,"",100))
# p<-p + scale_x_discrete(breaks=c("HC","FA"),
#                         labels=c("HC"="Coral","FA"="Algae"))
p<- p + expand_limits(y=0)
p<-p+ scale_fill_manual("Period", 
                        breaks = c("Pre", "Post"),
                        values = c("Pre"='dark blue', "Post"='dark red'),
                        labels = c("Pre"='pre-bleaching', "Post"='response'))

# p + guides(fill = guide_legend(override.aes = list(shape = 21)))


print(p)

jpeg("Kenya_no_take_vs_open_coral_cover.jpeg",res=300,height=3,width=3,units = 'in')
p
dev.off()


#to get site level averages
ken2$Site[which(ken2$Site=="Kisite")]<-"Shimoni"

ave_ken<-ddply(ken2,c("Period","Site"),summarise,
               mean_cover=mean(recent_Coral),
               sd=sd(recent_Coral),
               Year=tail(recent_year,n=1))

ave_ken2<-ave_ken[order(ave_ken$Site,ave_ken$Period),]   #change in mean coral cover at each site

library(dplyr)

df2 <- ave_ken%>%
  group_by(Site) %>%
  arrange(Site,Period) %>%
  mutate(pct.chg = (mean_cover - lag(mean_cover))/lag(mean_cover)*100)

# df2$pct.chg[df2$Period=='Pre']<-NA

write.csv(df2,"Kenya_change_in_coral_cover_per_site.csv",row.names =F)


# Comoros sub-national anaylsis --------------------------------------------

com2<-r2[which(r2$Country=='Comoros'),]    #sites with data for both periods from 2012

com3<-ddply(com2,c("Period"),summarise,
            mean_cover=mean(recent_Coral),
            se=sd(recent_Coral)/sqrt(length(Site)),
            n=length(Site))

com2$Site[which(com2$Station=='Mea'|com2$Station=='nkandzoni')]<-'Moheli'
com2$Site[which(com2$Station=='mitsamiuli'|com2$Station=='Moroni')]<-'Grande Comore'
com2$Site[which(com2$Station=='wani')]<-'Anjouan'

com2$Period<-factor(com2$Period,levels=c("Pre","Post"))

com_isl<-ddply(com2,c("Period","Site"),summarise,
               mean_cover=mean(recent_Coral),
               se=sd(recent_Coral)/sqrt(length(Site)),
               n=length(Site))

df3 <- com_isl%>%
  group_by(Site) %>%
  arrange(Period) %>%
  mutate(pct.chg = (mean_cover - lag(mean_cover))/lag(mean_cover)*100)

com<-df3[order(df3$Site),]   #change in mean coral cover at each site


write.csv(df3,"Comoros_change_in_coral_cover_per_island.csv",row.names =F)

com2$Site<-factor(com2$Site,levels=c("Grande Comore","Moheli","Anjouan"))

#bar plot to show change in coral cover for 3 islands
p<-NA 
p<- ggplot(com2,aes(x=Site,y=recent_Coral,fill=Period))
p<- p + stat_summary(fun.y="mean", geom="bar",position=position_dodge(),alpha=0.8)

p<- p + stat_summary(fun.data = 'mean_se', geom = "errorbar",width=.2,size=0.8,position=position_dodge(.9))
p

p <- p + theme_bw()+theme(plot.title=element_blank(),
                          axis.line = element_line(size=1,colour = "black"),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.border = element_blank(),
                          panel.background = element_blank(),
                          text = element_text(size=11,face="bold"),
                          axis.ticks.length=unit(0.2,"cm"),
                          axis.text = element_text(colour="black"),
                          axis.title.x = element_blank())

p<- p + theme(legend.key = element_blank(),
              legend.position="bottom",
              legend.key.height=unit(0.2, "cm"),
              plot.margin = unit(c(0,0.1,0.1,0.1), "lines"),
              #               legend.margin=unit(0.5,"cm"),
              legend.text=element_text(size=8))

#p<- p + ggtitle(e[i])
p<- p + ylab("Hard coral cover (%)")

p<- p + theme(strip.text.x = element_text(size=11,face="bold"),
              strip.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())
p<- p + scale_y_continuous(breaks = seq(0,100,by=5),
                           labels=c(0,"",10,"",20,"",30,"",40,"",50,"",60,"",70,"",80,"",90,"",100))
# p<-p + scale_x_discrete(breaks=c("HC","FA"),
#                         labels=c("HC"="Coral","FA"="Algae"))
p<- p + expand_limits(y=0)
p<-p+ scale_fill_manual("Period", 
                        breaks = c("Pre", "Post"),
                        values = c("Pre"='dark blue', "Post"='dark red'),
                        labels = c("Pre"='pre-bleaching', "Post"='response'))

# p + guides(fill = guide_legend(override.aes = list(shape = 21)))


print(p)

jpeg("Comoros_3_islands_coral_cover.jpeg",res=300,height=4,width=4,units = 'in')
p
dev.off()


# Tanzania sub-national analysis ------------------------------------------

tan2<-r2[which(r2$Country=='Tanzania'),]    #sites with data for both periods from 2012

tan2$Period<-factor(tan2$Period,levels=c("Pre","Post"))

tan_isl<-ddply(tan2,c("Period","Site"),summarise,
               mean_cover=mean(recent_Coral),
               se=sd(recent_Coral)/sqrt(length(Site)),
               n=length(Site))

df3 <- tan_isl%>%
  group_by(Site) %>%
  arrange(Period) %>%
  mutate(pct.chg = (mean_cover - lag(mean_cover))/lag(mean_cover)*100)

tan<-df3[order(df3$Site),]   #change in mean coral cover at each site


write.csv(tan,"Tanzania_change_in_coral_cover_per_site.csv",row.names =F)

tan2$Site<-factor(tan2$Site,levels=c("Tanga","Zanzibar","Mafia","Songosongo","Mtwara"))

#bar plot to show change in coral cover for 3 islands
p<-NA 
p<- ggplot(tan2,aes(x=Site,y=recent_Coral,fill=Period))
p<- p + stat_summary(fun.y="mean", geom="bar",position=position_dodge(),alpha=0.8)

p<- p + stat_summary(fun.data = 'mean_se', geom = "errorbar",width=.2,size=0.8,position=position_dodge(.9))
p

p <- p + theme_bw()+theme(plot.title=element_blank(),
                          axis.line = element_line(size=1,colour = "black"),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.border = element_blank(),
                          panel.background = element_blank(),
                          text = element_text(size=11,face="bold"),
                          axis.ticks.length=unit(0.2,"cm"),
                          axis.text = element_text(colour="black"),
                          axis.title.x = element_blank())

p<- p + theme(legend.key = element_blank(),
              legend.position="bottom",
              legend.key.height=unit(0.2, "cm"),
              plot.margin = unit(c(0,0.1,0.1,0.1), "lines"),
              #               legend.margin=unit(0.5,"cm"),
              legend.text=element_text(size=8))

#p<- p + ggtitle(e[i])
p<- p + ylab("Hard coral cover (%)")

p<- p + theme(strip.text.x = element_text(size=11,face="bold"),
              strip.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())
p<- p + scale_y_continuous(breaks = seq(0,100,by=5),
                           labels=c(0,"",10,"",20,"",30,"",40,"",50,"",60,"",70,"",80,"",90,"",100))
# p<-p + scale_x_discrete(breaks=c("HC","FA"),
#                         labels=c("HC"="Coral","FA"="Algae"))
p<- p + expand_limits(y=0)
p<-p+ scale_fill_manual("Period", 
                        breaks = c("Pre", "Post"),
                        values = c("Pre"='dark blue', "Post"='dark red'),
                        labels = c("Pre"='pre-bleaching', "Post"='response'))

# p + guides(fill = guide_legend(override.aes = list(shape = 21)))


print(p)

jpeg("Tanzania_coral_cover_change_per_site.jpeg",res=300,height=4,width=4,units = 'in')
p
dev.off()


# Madagascar sub-national analysis ----------------------------------------
mad2<-r2[which(r2$Country=='Madagascar'),]    #sites with data for both periods from 2012

mad2$Period<-factor(mad2$Period,levels=c("Pre","Post"))

mad_isl<-ddply(mad2,c("Period","Site"),summarise,
               mean_cover=mean(recent_Coral),
               se=sd(recent_Coral)/sqrt(length(Site)),
               n=length(Site))

df3 <- mad_isl%>%
  group_by(Site) %>%
  arrange(Period) %>%
  mutate(pct.chg = (mean_cover - lag(mean_cover))/lag(mean_cover)*100)

mad<-df3[order(df3$Site),]   #change in mean coral cover at each site


write.csv(mad,"Madagascar_change_in_coral_cover_per_site.csv",row.names =F)

# mad2$Site<-factor(mad2$Site,levels=c("madga","Zanzibar","Mafia","Songosongo","Mtwara"))

#bar plot to show change in coral cover for 3 islands
p<-NA 
p<- ggplot(mad2,aes(x=Site,y=recent_Coral,fill=Period))
p<- p + stat_summary(fun.y="mean", geom="bar",position=position_dodge(),alpha=0.8)

p<- p + stat_summary(fun.data = 'mean_se', geom = "errorbar",width=.2,size=0.8,position=position_dodge(.9))
p

p <- p + theme_bw()+theme(plot.title=element_blank(),
                          axis.line = element_line(size=1,colour = "black"),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.border = element_blank(),
                          panel.background = element_blank(),
                          text = element_text(size=11,face="bold"),
                          axis.ticks.length=unit(0.2,"cm"),
                          axis.text = element_text(colour="black"),
                          axis.title.x = element_blank())

p<- p + theme(legend.key = element_blank(),
              legend.position="bottom",
              legend.key.height=unit(0.2, "cm"),
              plot.margin = unit(c(0,0.1,0.1,0.1), "lines"),
              #               legend.margin=unit(0.5,"cm"),
              legend.text=element_text(size=8))

#p<- p + ggtitle(e[i])
p<- p + ylab("Hard coral cover (%)")

p<- p + theme(strip.text.x = element_text(size=11,face="bold"),
              strip.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())
p<- p + scale_y_continuous(breaks = seq(0,100,by=5),
                           labels=c(0,"",10,"",20,"",30,"",40,"",50,"",60,"",70,"",80,"",90,"",100))
# p<-p + scale_x_discrete(breaks=c("HC","FA"),
#                         labels=c("HC"="Coral","FA"="Algae"))
p<- p + expand_limits(y=0)
p<-p+ scale_fill_manual("Period", 
                        breaks = c("Pre", "Post"),
                        values = c("Pre"='dark blue', "Post"='dark red'),
                        labels = c("Pre"='pre-bleaching', "Post"='response'))

# p + guides(fill = guide_legend(override.aes = list(shape = 21)))


print(p)

jpeg("Madagascar_coral_cover_change_per_site.jpeg",res=300,height=4,width=4,units = 'in')
p
dev.off()

