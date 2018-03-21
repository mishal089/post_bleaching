wcs<-read.csv("WCS cover data_2016_2017.csv",header = T, stringsAsFactors = F)

#ensure that all NA values for cover are converted to zeros
wcs[c("HARD_CORAL", "SOFT_CORAL","MACROALGAE")][is.na(wcs[c("HARD_CORAL", "SOFT_CORAL","MACROALGAE")])] <- 0

#in month column - April should be changed to 4
wcs$MONTH[which(wcs$MONTH=='April')]<-4
wcs$MONTH[which(wcs$MONTH=='March')]<-3

#to find out how many transects 
library(plyr)
tran<-ddply(wcs,c("SITE","YEAR","MONTH","DAY"),summarise,
            no_tran=length(YEAR))

#to find out for repeated sites - 2017 and 2016 pre

rep<-ddply(wcs,c("Transect_ID","YEAR","MONTH","DAY"),summarise,
                    no_rep=length(YEAR))

list_sites<-length(unique(rep$Transect_ID)) #34

site<-ddply(rep,c("Transect_ID"),summarise,
            no_repeats=length(Transect_ID))

COORDS<-ddply(wcs,c("Transect_ID","LOCATION","SITE","YEAR","MONTH","DAY"),summarise,
              lat=unique(LAT),
              long=unique(LONG))

sites<-ddply(wcs,c("Transect_ID","LOCATION","SITE"),summarise,
              lat=unique(LAT),
              long=unique(LONG))

write.csv(sites,"WCS_2016_2017_sites.csv",row.names = F)

#summarise at site level

# wide to long

library(tidyr)
data_long <- gather(wcs, benthic_cover, cover, HARD_CORAL:MACROALGAE, factor_key=TRUE)

#remove 2016 nov and dec data here
data_long2<-data_long[-which(data_long$MONTH==11&data_long$YEAR==2016|data_long$MONTH==12& data_long$YEAR==2016),]


wcs1<-ddply(data_long2,c("COUNTRY","NAP","Transect_ID","YEAR","benthic_cover"),summarise,
              mean_cover=mean(cover),
            #   mean_soft=mean(SOFT_CORAL),
            # mean_algae=mean(MACROALGAE),
            # sd_hard=sd(HARD_CORAL),
            # sd_soft=sd(SOFT_CORAL),
            sd=sd(cover),
            samples=length(cover))

#steps to format clean data for post-bleaching
#add a period column
# keep only march,april,may data for 2016 and 2017
#add benthic code column
# organization - WCS Madagascar, source - new

# #at this point need to do a yearly average for 2017 data only
# 
# #split datasets into 2016 and 2017 and then re-merge
# 
# wcs_2016<-wcs1[wcs1$YEAR==2016,]
# wcs_2017<-wcs1[wcs1$YEAR==2017,]
# 
# #for 2016 data - exclude nov/dec data
# wcs__2016b<-wcs_2016[which(wcs_2016$MONTH==3|wcs_2016$MONTH==4|wcs_2016$MONTH==5),]
# 
# #for 2017 data - do average across months for each transect_id
# wcs_2017b<-ddply(wcs_2017,c("COUNTRY","NAP","Transect_ID","YEAR","benthic_cover"),summarise,
#             mean_cover=mean(mean_cover),
#             sd=sd(mean_cover),
#             samples=length(mean_cover))
# 
# #merge it now

wcs2<-wcs1
wcs2$benthic_cover<-as.character(wcs2$benthic_cover)

wcs2$benthic_cover[which(wcs2$benthic_cover=='HARD_CORAL')]<-'Coral'
wcs2$benthic_cover[which(wcs2$benthic_cover=='SOFT_CORAL')]<-'Soft coral'
wcs2$benthic_cover[which(wcs2$benthic_cover=='MACROALGAE')]<-'Algae-macro/fleshy'

wcs2$benthic_code<-'HC'
wcs2$benthic_code[which(wcs2$benthic_cover=='Soft coral')]<-'SC'
wcs2$benthic_code[which(wcs2$benthic_cover=='Algae-macro/fleshy')]<-'AMAC'

wcs2$Organization<-'WCS Madagascar'

wcs2$Period<-'Post'
wcs2$Period[which(wcs2$YEAR==2016)]<-'Pre'

wcs2$Source<-'New'

#NOW I NEED TO FILTER OUT THOSE SITES WHICH DON'T HAVE DATA PRE AND POST

rep2<-ddply(wcs2,c("Transect_ID"),summarise,
           no_rep=length(YEAR))

#andra_06 - no prebleaching data
#Andravona_unknown - no prebleaching data
#Belo_04 - no prebleaching data

wcs3<-merge.data.frame(wcs2,rep2,all.x=TRUE)
#exclude all those sites that dont have no_rep of 6

wcs4<-wcs3[-which(wcs3$no_rep==3),]

#FUNCTION TO CAPITALISE FIRST LETTER
capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}
wcs4$COUNTRY<- capFirst(wcs4$COUNTRY)

wcs4$NAP <- tolower(wcs4$NAP) 
wcs4$NAP <- capFirst(wcs4$NAP) ## capitalize first letter

write.csv(wcs4,"WCS Madagsacar benthic data formatted.csv",row.names = F)