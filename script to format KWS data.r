library(plyr)


setwd("~/CORDIO/IOC post-bleaching project/Post-bleaching assessment/Formatting/")

kws<-read.csv("kws.csv",header = T,stringsAsFactors = F)

kws<-kws[order(kws[1],kws[2],kws[4],kws[6],kws[8],kws[9]),]

#step 1 - get mean cover at a transect level
#step 2 - get mean cover and sd at a site level from transect means

#there are some errors in the data - need to correct
for (i in 1:nrow(kws)){
  kws$length2[i]<-kws$Distance[i+1]-kws$Distance[i]
}
kws$length2[which(kws$Cover=="")]<-""
kws$length2<-as.integer(kws$length2)

kws$Transect[which(kws$Site.name=="Coral Garden"& kws$Area=='Kisite'& kws$Distance==4000 & kws$Transect==5)]<-6  #error in the data

f<-ddply(kws, c("Date","Area","Site.name","Cover", "Transect"),summarise,
         cover=sum(length2,na.rm = T)/10)   #transect level mean cover, /1000*100 = /10

#test - to make sure each transect cover adds up to 100
test<-ddply(f,c("Date","Area","Site.name", "Transect"),summarise,
            total=sum(cover,na.rm = T))

unique(test$total)

#different categories will be present in different number of transects so cant do mean just yet - need to sum
v<-ddply(f, c("Date","Area","Site.name","Cover"),summarise,
         cover_site=sum(cover,na.rm = T),
         sd= sd(cover,na.rm=T))     # mean cover and sd at each site

w<-ddply(kws, c("Date","Area","Site.name"),summarise,
no.tran=length(unique(Transect)))    #number of transects per site

# df3<-merge(v,w,by="Site.name",all = F)
w$local<-paste(w$Area,w$Site.name,sep="_")
v$local<-paste(v$Area,v$Site.name,sep="_")
v$tran.no <- w$no.tran[match(v$local, w$local)]   #add the number of transects to each site

v$ave_cover<-v$cover_site/v$tran.no     #average cover at each site

v<-v[-which(v$Cover==""),]  #remove blank covers at end of each transect

#CAN DO THE SAME TEST - SUM OF COVER AT EACH SITE TO BE 100
test2<-ddply(v,c("Date","Area","Site.name"),summarise,
             total=sum(ave_cover,na.rm = T))

unique(test2$total)

#now need to make cover entries consistent - all names

v$Cover[which(v$Cover=="AT")]<-"Algae-turf"
v$Cover[which(v$Cover=="HC")]<-"Coral"
v$Cover[which(v$Cover=="BR")]<-"Bare substrate"
v$Cover[which(v$Cover=="CA")]<-"Algae-coralline"
v$Cover[which(v$Cover=="DC")]<-"Dead Standing coral"
v$Cover[which(v$Cover=="Faviates")]<-"Favites"
v$Cover[which(v$Cover=="HAL")]<-"Algae-Halimeda"
v$Cover[which(v$Cover=="MA")]<-"Algae-macro/fleshy"
v$Cover[which(v$Cover=="HAL")]<-"Algae-Halimeda"
v$Cover[which(v$Cover=="Hal")]<-"Algae-Halimeda"
v$Cover[which(v$Cover=="Porites(B)")]<-"Porites branching"
v$Cover[which(v$Cover=="Porites(m)")]<-"Porites massive"
v$Cover[which(v$Cover=="RB")]<-"Rubble"
v$Cover[which(v$Cover=="S")]<-"Sand"
v$Cover[which(v$Cover=="SC")]<-"Soft coral"
v$Cover[which(v$Cover=="SG")]<-"Seagrass"
v$Cover[which(v$Cover=="SPO")]<-"Spongy"
v$Cover[which(v$Cover=="Cousenaraea")]<-"Coscinaraea"
v$Cover[which(v$Cover=="FAVIA")]<-"Favia"
v$Cover[which(v$Cover=="FAVIATES")]<-"Favites"
v$Cover[which(v$Cover=="PLATYGYRA")]<-"Platygyra"
v$Cover[which(v$Cover=="porites(M)")]<-"Porites massive"
v$Cover[which(v$Cover=="Aestreapora")]<-"Astreopora"
v$Cover[which(v$Cover=="Aestreopora")]<-"Astreopora"
v$Cover[which(v$Cover=="Helliopora")]<-"Heliopora"
v$Cover[which(v$Cover=="Porites(M)")]<-"Porites massive"
v$Cover[which(v$Cover=="Consenaraea")]<-"Coscinaraea"
v$Cover[which(v$Cover=="Echnophillia")]<-"Echinophyllia"
v$Cover[which(v$Cover=="Hydrophora")]<-"Hydnopohora"
v$Cover[which(v$Cover=="pavona")]<-"Pavona"
v$Cover[which(v$Cover=="unknown")]<-"Unknown"
v$Cover[which(v$Cover=="Hydnophora")]<-"Hydnopohora"

codes<-read.csv("benthic_codes_all.csv",header=T,stringsAsFactors = F)

f<-match(v$Cover,codes$Name,nomatch = 0)
which(f==0)
v$Cover[which(f==0)]

v$benthic_code<-codes$Code[match(v$Cover,codes$Name)]

v1<-v[,c(1,2,3,4,11,9,6,8)]
v1<-rename(v1,c("Site.name"="Site","ave_cover"="mean_cover","tran.no"="number_replicates"))

v1[,'mean_cover']=round(v1[,'mean_cover'],2)
v1[,'sd']=round(v1[,'sd'],1)


write.csv(v1,"KWS_benthic_formatted.csv",row.names = F)


# Kenya fieldwork data  ---------------------------------------------------

# benthic -----------------------------------------------------------------
library(plyr)

setwd("~/CORDIO/IOC post-bleaching project/Post-bleaching assessment/Formatting/")

kenya<-read.csv("kenya_benthic.csv",header = T,stringsAsFactors = F)

test_ke<-ddply(kenya,c("Date","Site","Location","Transect"),summarise,
               cover=sum(Points,na.rm = T))

#all ok except Iweni and Mayunguni transect 2

h<-ddply(kenya,c("Date","Site","Location","Transect","Genera"),summarise,
               cover=sum(Points,na.rm = T))  #no repeat entries for same genus for each transect

#only need to do this because some sites dont have a total of 100 points otherwise points is the same as %
# kenya$total_points<-test_ke$cover[match(paste(kenya$Location,kenya$Transect,sep=""),paste(test_ke$Location,test_ke$Transect,sep=""))]

# kenya$mean_cover<-(kenya$Points/kenya$total_points)*100
kenya$mean_cover<-kenya$Points

x<-ddply(kenya,c("Date","Site","Location","Genera"),summarise,
         cover=sum(mean_cover,na.rm = T),
         sd=sd(mean_cover,na.rm = T)) 


w1<-ddply(kenya, c("Date","Site","Location"),summarise,
         no.tran=length(unique(Transect)))  

x$no.tran<-w1$no.tran[match(x$Location,w1$Location)]

x$per_cover<-x$cover/x$no.tran

test_x<-ddply(x,c("Date","Site","Location"),summarise,
              per=sum(per_cover,na.rm = T))

x1<-x[,c(1:4,8,6,7)]

x1<-rename(x1,c("Site"="Sector","Genera"="benthic_category","Location"="Site","no.tran"="number_replicates","per_cover"="mean_cover"))

x1[,'mean_cover']=round(x1[,'mean_cover'],2)
x1[,'sd']=round(x1[,'sd'],1)

codes<-read.csv("benthic_codes_all.csv",header=T,stringsAsFactors = F)

x1$benthic_category[which(x1$benthic_category=="Algae-Macro")]<-"Algae-macro/fleshy"      #can use this line to change names of benthic categories
x1$benthic_category[which(x1$benthic_category== "Algae-Turf" )]<-"Algae-turf" 
x1$benthic_category[which(x1$benthic_category=="Asteopora")]<-"Astreopora" 
x1$benthic_category[which(x1$benthic_category=="Bare Substrate")]<-"Bare substrate" 
x1$benthic_category[which(x1$benthic_category=="Branching Porites")]<-"Porites branching" 
x1$benthic_category[which(x1$benthic_category=="Hydronophora")]<-"Hydnopohora" 
x1$benthic_category[which(x1$benthic_category=="Lobophylia")]<-"Lobophyllia" 
x1$benthic_category[which(x1$benthic_category=="Massive Porites")]<-"Porites massive" 
x1$benthic_category[which(x1$benthic_category=="Recent dead coral")]<-"Recent Dead Coral" 
x1$benthic_category[which(x1$benthic_category=="Sponges")]<-"Sponge" 

f<-match(x1$benthic_category,codes$Name,nomatch = 0)
which(f==0)
unique(x1$benthic_category[which(f==0)])

for (i in 1:nrow(x1)){
  x1$benthic_code[i]<-codes$Code[f[i]]
}

x2<-x1[,c(1:4,8,5,6,7)]


write.csv(x2,"Kenya_fieldwork_benthic_formatted.csv",row.names = F)



# 2017 benthic dataset - test ---------------------------------------------
library(plyr)
setwd("~/CORDIO/IOC post-bleaching project/Post-bleaching assessment/Formatting/")

benthic<-read.csv("2017_WIO_benthic_updated.csv",header = T,stringsAsFactors = F)
codes<-read.csv("benthic_codes_all.csv",header=T,stringsAsFactors = F)


unique(benthic$Source)
unique(benthic$Period)

benthic$Period[which(benthic$Period=="pre")]<-"Pre"
benthic$Source[which(benthic$Source=="NEW")]<-"New"
benthic$benthic_code[which(benthic$benthic_code=="DSC")]<-"DC"
benthic$benthic_code[which(benthic$benthic_code=="OTHER")]<-"O"
benthic$benthic_code[which(benthic$benthic_code=="SILT")]<-"SIL"
benthic$benthic_code[which(benthic$benthic_code=="SG")]<-"SGR"
benthic$benthic_code[which(benthic$benthic_code=="S")]<-"SND"
benthic$benthic_code[which(benthic$benthic_code=="SPO")]<-"Spog"
benthic$benthic_code[which(benthic$benthic_code=="ZOA")]<-"Zooa"
benthic$benthic_code[which(benthic$benthic_code=="Zoa")]<-"Zooa"
benthic$benthic_code[which(benthic$benthic_code=="Acb")]<-"acb"
benthic$benthic_code[which(benthic$benthic_code=="Act")]<-"act"
benthic$benthic_code[which(benthic$benthic_code=="RB")]<-"RUB"
benthic$benthic_code[which(benthic$benthic_code=="Spo")]<-"Spog"
benthic$benthic_code[which(benthic$benthic_code=="FA")]<-"AMAC"

#to find if any benthic codes are non GCRMN standard
f<-match(benthic$benthic_code,codes$Code,nomatch = 0)
which(f==0)
unique(benthic$benthic_code[which(f==0)])



#[1] "RS"   "S"    ""     "NIA"  "Hyro"

#correct the codes
# benthic$benthic_code[which(benthic$benthic_code=="RS")]<-""

#new benthic category names based on matched codes
for (i in 1:nrow(benthic)){
  benthic$benthic_name2[i]<-codes$Name[f[i]]
}

benthic$Station<-gsub(" ","_",benthic$Station)
# benthic$Site<-gsub(" ","_",benthic$Site)
benthic$Site<- gsub(" $","",benthic$Site, perl=T)   #removes white space at the end of character
benthic$Station<-gsub("_$","",benthic$Station,perl=T) #removes _ at the end of character

#ALL spaces in station will be underscored but not for site

benthic$Station[which(benthic$Station=="North_reef"& benthic$Site=="Malindi")]<-"North_Reef"
benthic$Station[which(benthic$Station=="Raswatini"& benthic$Site=="Mombasa")]<-"Ras_Iwatine"
benthic$Station[which(benthic$Station=="StarFish"& benthic$Site=="Mombasa")]<-"Starfish"
benthic$Station[which(benthic$Station=="Richard_B"& benthic$Site=="Watamu")]<-"Richard_Bernets"
benthic$Station[which(benthic$Station=="Lobster"& benthic$Site=="Desroches")]<-"Lobster_rock"
benthic$Station[which(benthic$Station=="Oxy"& benthic$Site=="Desroches")]<-"Oxi_rock"
benthic$Station[which(benthic$Station=="CG_South"& benthic$Site=="North Reef")]<-"Coral_Gardens_South"
benthic$Station[which(benthic$Station=="CG_North"& benthic$Site=="North Reef")]<-"Coral_Gardens_North"
benthic$Station[which(benthic$Station=="NE_Point"& benthic$Site=="North Reef")]<-"North_East_Point"
benthic$Station[which(benthic$Station=="Sprat_city"& benthic$Site=="North Reef")]<-"Sprat_City"
benthic$Station[which(benthic$Station=="Mtsanga"& benthic$Site=="Bambao")]<-"bambao"
benthic$Station[which(benthic$Station=="Dragoni"& benthic$Site=="Bimbini")]<-"bimbini"
benthic$Station[which(benthic$Station=="Membwabwani"& benthic$Site=="Mitsamiouli")]<-"mitsamiuli"
benthic$Station[which(benthic$Station=="Hadawo"& benthic$Site=="Ouani")]<-"wani"
benthic$Station[which(benthic$Station=="Candzoni")]<-"nkandzoni"
benthic$Station[which(benthic$Station=="Amana_S"& benthic$Site=="Songosongo")]<-"Amana_South"
benthic$Station[which(benthic$Station=="Maziwe_south"& benthic$Site=="Tanga")]<-"Maziwe_South"


benthic$Site[which(benthic$Station=="Kanamai"& benthic$Site=="Mombasa")]<-"Kilifi"
benthic$Site[which(benthic$Site=="Chumbe Island")]<-"Zanzibar"


benthic$Site[which(benthic$Site=="Mahe")]<-"Mahe NW"
benthic$Site[which(benthic$Station=="Wasini_Tengefu"& benthic$Site=="Southcoast")]<-"Shimoni"
benthic$Site[which(benthic$Site=="Southcoast")]<-"Kisite"
benthic$Station[which(benthic$Station=="K-Light_House"& benthic$Site=="Kisite")]<-"Light_house"
benthic$Station[which(benthic$Station=="Wasini"& benthic$Site=="Shimoni")]<-"Wasini_Tengefu"
benthic$Site[which(benthic$Country=="Comoros")]<-NA

benthic$Station[which(benthic$Station=="itsandra")]<-"Itsandra"
benthic$Station[which(benthic$Station=="mea")]<-"Mea"
benthic$Station[which(benthic$Station=="moroni")]<-"Moroni"
benthic$Station[which(benthic$Station=="sambia")]<-"Sambia"


#to see if the sites are consitent - no double names or spelling issues
r<-unique(benthic[c("Country", "Site", "Station")])
r1<-r[order(r[1],r[2],r[3]),]

#summarise new aldabra data so that it matched GCRMN data which is an aggregation of deep and shallow sites

ald<-benthic[which(benthic$Organization=="SIF_Aldabra"),]

ald_s<-ddply(ald,c("Country","Year","Site","Reef.zone","benthic_name","benthic_code"),summarise,
             cover=mean(mean_cover,na.rm=T),
             sd=sd(mean_cover,na.rm=T))
ald_s$Station[which(ald_s$Reef.zone=="shallow")]<-"All_shallow_sites"
ald_s$Station[which(ald_s$Reef.zone=="deep")]<-"All_deep_sites"
ald_s$Organization<-"SIF_Aldabra"
ald_s$Source<-"NEW"
ald_s$Period<-"Post"
ald_s<-rename(ald_s,c("cover"="mean_cover"))

benthic<-benthic[-which(benthic$Organization=="SIF_Aldabra"),]

k<-rbind.fill(benthic,ald_s)

t<-k[,c(1,4:8,21,10:12,18:20)]

write.csv(t,"2017_WIO_benthic_pre_analysis.csv",row.names = F)
#remove data from those sites that do not have data for both periods (pre and post)

k1<-k[-which(k$Station=="Turtle_reef"& k$Site=="Watamu"),]
# k2<-k1[-which(k1$Station=="Kivulini"& k1$Site=="Malindi"),]
k2<-k1
k3<-k2[-which(k2$Station=="Dive_Point"& k2$Site=="Kisite"),]
k4<-k3[-which(k3$Station=="Coral_Garden"& k3$Site=="Kisite"),]
k5<-k4[-which(k4$Site=="Bel Ombre"),]
k6<-k5[-which(k5$Station=="Aquarium"& k5$Site=="North Reef"),]
# k7<-k6[-which(k6$Station=="Tausi"& k6$Site=="Lamu"),]
# k8<-k7[-which(k7$Station=="Kuruwitu"& k7$Site=="Kilifi"),]
k8<-k6
#until we get the post-bleaching data (fieldwork)
k9<-k8[-which(k8$Station=="memboimboini"|k8$Station=="itsamia"),]
# k9<-k8[-which(k8$Country=="Comoros"),]
# k10<-k9[-which(k9$Country=="Tanzania" & k9$Source=="GCRMN"),]  #excluding tz sites except chumbe
# k9<-k8[-which(k8$Country=="Seychelles" & k8$Site=="Mahe NW"),]  #excluding GVI data till it is sorted
# k9<-k8 #just for the purpose of the script
k10<-k9
#NEED TO REMOVE GCRMN COMOROS DATA WITH NO POST-BLEACHING DATA - BASED ON ZONES
k11<-k10[-which(k10$Station=="Mea"& k10$Reef.zone=="Back reef"),]
k12<-k11[-which(k11$Station=="Itsandra"& k11$Reef.zone=="Fore reef"),]
# y<-k10[-which(k10$Station=="nkandzoni"& k10$Reef.zone=="Back reef"),]
k13<-k12[-which(k12$Station=="bimbini"& k12$Reef.zone=="Fore reef"),]
k14<-k13[-which(k13$Station=="wani"& k13$Reef.zone=="Back reef"),]
# k15<-k14[-which(k14$Station=="moroni"& k14$Reef.zone=="Back reef"),]
# y<-k10[-which(k10$Station=="mitsamiuli"& k10$Reef.zone=="Fore reef"),]
k15<-k14[-which(k14$Station=="Sambia"& k14$Reef.zone=="Back reef"),]

#removing more sites with no data in both periods

k16<-k15[-which(k15$Station=="Kipwani"& k15$Site=="Tanga"),]  #no post-data
k17<-k16[-which(k16$Station=="Jambe"& k16$Site=="Tanga"),]    #no post-data
k18<-k17[-which(k17$Station=="Maziwe_North"& k17$Site=="Tanga"),]   #maziwe in GCRMN - waiting for clarification on which to use N/S
k19<-k18
# k19<-k18[-which(k18$Station=="Maziwe_South"& k18$Site=="Tanga"),]   #maziwe in GCRMN - waiting for clarification on which to use N/S
k20<-k19[-which(k19$Station=="Dambwe"& k19$Site=="Tanga"),]  #need confirmation on whether Dambwe and Dambwe W are the same
k21<-k20[-which(k20$Station=="Dambwe_W"& k20$Site=="Tanga"),] #need confirmation on whether Dambwe and Dambwe W are the same
k22<-k21[-which(k21$Station=="Ruvula"& k21$Site=="Songosongo"),]   #no pre-bleaching data
# k23<-k22[-which(k22$Station=="Machangi"& k22$Site=="Songosongo"),]  #need confirmation on whether Machangi and Machangi E are the same
# k24<-k23[-which(k23$Station=="Machangi_East"),]
k24<-k22
k25<-k24[-which(k24$Station=="Mwaromba"& k24$Site=="Diani-Chale"),]  #no post-data
k26<-k25
# k26<-k25[-which(k25$Site=="Mafia"),]  #post-bleaching data not in the dataset yet
k27<-k26[-which(k26$Station=="Maziwe"& k26$Site=="Tanga"),]   #maziwe in GCRMN - waiting for clarification on which to use N/S
# k28<-k27[-which(k27$Station=="Bawe"& k27$Site=="Zanzibar"),]   #waiting for fieldwork data
# k29<-k28[-which(k28$Station=="Chapwani"& k28$Site=="Zanzibar"),]   #waiting for fieldwork data

k29<-k27[-which(k27$Station=="Blue_Pillars"& k27$Site=="Nosy Be"),] 
k30<-k29[-which(k29$Station=="Kwale"& k29$Site=="Zanzibar"),]   #NO POST
k31<-k30[-which(k30$Station=="A2"& k30$Site=="Alphonse"),]   #NO POST
k32<-k31[-which(k31$Station=="A3"& k31$Site=="Alphonse"),]   #NO POST
k33<-k32[-which(k32$Station=="A9"& k32$Site=="Alphonse"),]   #NO POST
k34<-k33[-which(k33$Station=="B1"& k33$Site=="Bijoutier"),]   #NO POST
k35<-k34[-which(k34$Station=="Dindini"& k34$Site=="Mafia"),]   #NO POST
k36<-k35[-which(k35$Station=="Kifinge"& k35$Site=="Mafia"),]   #NO POST
k37<-k36[-which(k36$Station=="Chumbe"& k36$Site=="Zanzibar"),]   #ALREADY GOT CHUMBE DATA - DON'T WANT DUPLICATE FROM OTHER SOURCE (dont have POST for alli data)
k38<-k37[-which(k37$Site=="Pemba"),]   #NO POST FOR MISALI PEMBA
k39<-k38[-which(k38$Station=="Mnemba"& k38$Site=="Zanzibar"),]   #NO POST
# k40<-k39[-which(k39$Organization=='Ali Ussi'& k39$Site=="Zanzibar"),]   #received - NO POST YET. Waiting for Saleh data
k40<-k39[-which(k39$Station=="Area_51"& k39$Site=="Nosy Be"),] 
k41<-k40[-which(k40$Organization=='FIELDWORK'& k40$Station=="Uyombo_2"),]   


#only pre
# Tanzania - Tanga - Kipwani and Jambe
#only post
# Tanga: Maziwe North and Maziwe South - in GCRMN it is just Maziwe
# Tanga: Dambwe W - just Dambwe in GCRMN
# Ruvula (Mtwara)
# Machangi (Songosongo) - Machangi East in new data


# remove unwanted columns and reorder accordingly
# k9<-k9[,c(1,4:8,21,10:12,18:20)]

#k9<-rename(k9,c("benthic_name2"="benthic_name"))
k10<-k41
#test to make sure one benthic category per station per survey date -  if k_test has same number of rows as k9 it is fine
k10$date<-paste(k10$Day,k10$Month,k10$Year,sep="/")
#zone makes no difference

k_test<-ddply(k10,c(1,22,4,5:8,19,21,10,18,20),summarise,
              ave_cover=sum(mean_cover,na.rm=T),
              sd=sd(mean_cover,na.rm = T),
              N=length(mean_cover)) 

#this is to aggregate across so that it is one set of records per station per period per year
k11<-ddply(k_test,c(1,3:12),summarise,
           mean_cover=sum(ave_cover,na.rm=T),
           sd=sd(ave_cover,na.rm = T),
           N=length(ave_cover))

#NO DIFFERENCE IN NUMBER OF ROWS BETWEEN K10 and KTEST which implies that no sites were ever monitored
#more than once in a year and period

#the below text can be uncommented if necessary

# k10<-ddply(k9,c(1:5,7,8,12),summarise,
#               ave_cover=sum(mean_cover,na.rm=T),
#               sd=sd(mean_cover,na.rm = T),
#               N=length(mean_cover))

# k10_n<-ddply(k10,c(1:5,8),summarise,
#             n=max(N))
# 
# k10$stat<-paste(k10$Year,k10$Station,k10$Period,sep="_")
# k10_n$stat<-paste(k10_n$Year,k10_n$Station,k10_n$Period,sep="_")
# 
# k10$n<-k10_n$n[match(k10$stat,k10_n$stat)]
# 
# k10$mean_cover<-k10$ave_cover/k10$n

k11<-k11[,c(1:13)]

k11<-rename(k11,c("benthic_name2"="benthic_name"))


write.csv(k11,"post_bleaching_benthic_dataset_for_analysis.csv",row.names = F)

# Reef Conservation benthic data - summarise ------------------------------

rc<-read.csv("RC_Mauritius_combined.csv",header = T,stringsAsFactors = F)

#check data is correct for each survey
test<-ddply(rc,c(1:6),summarise,
            total=sum(percent_cover,na.rm=T))

#can use month column
#create a period column - to help with 2016 aggregation

for (i in 1:nrow(rc)){
  if (rc$Year[i]==2016){
    if (rc$Month[i]>=8){rc$Period[i]<-"Post"}
  } else if (rc$Year[i]==2017){rc$Period[i]<-"Post"}
  else {rc$Period[i]<-"Pre"}
}

#step 1 - for each year - sum for each benthic class

rc_sum<-ddply(rc,c(3:7,10),summarise,
              total=sum(percent_cover,na.rm=T),
              sd=sd(percent_cover,na.rm = T),
              N=length(percent_cover))

rc_n<-ddply(rc_sum,c(1:4,6),summarise,
              n=max(N))  #total number of surveys done at each site in the year

rc_sum$stat<-paste(rc_sum$Year,rc_sum$Station,rc_sum$Period,sep="_")
rc_n$stat<-paste(rc_n$Year,rc_n$Station,rc_n$Period,sep="_")

rc_sum$n<-rc_n$n[match(rc_sum$stat,rc_n$stat)]

rc_sum$ave_cover<-rc_sum$total/rc_sum$n

test1<-ddply(rc_sum,c(1:4,6),summarise,
             tot=sum(ave_cover,na.rm = T))
#ONE issues is for 2016 data - should not aggregate across the year - pre and post bl values

#it works fine - add source, organisation columns, rename and then save it
#will read this file in along with k and merge the two rbind.fill

rc_sum<-rc_sum[,c(1:6,12)]

rc_sum<-rename(rc_sum,c("ave_cover"="mean_cover"))
rc_sum[,'mean_cover']=round(rc_sum[,'mean_cover'],2)

codes<-read.csv("benthic_codes_all.csv",header = T,stringsAsFactors = F)

rc_sum$Benthic_code[which(rc_sum$Benthic_code=="NIA")]<-"ALG"
rc_sum$Benthic_code[which(rc_sum$Benthic_code=="RB")]<-"RUB"
rc_sum$Benthic_code[which(rc_sum$Benthic_code=="RC")]<-"RCK"
rc_sum$Benthic_code[which(rc_sum$Benthic_code=="RKC")]<-"RDC"
rc_sum$Benthic_code[which(rc_sum$Benthic_code=="SD")]<-"SND"
rc_sum$Benthic_code[which(rc_sum$Benthic_code=="SP")]<-"Spog"
rc_sum$Benthic_code[which(rc_sum$Benthic_code=="SI")]<-"SIL"

g<-match(rc_sum$Benthic_code,codes$Code,nomatch=0)
which(g==0)
unique(rc_sum$Benthic_code[which(g==0)])

rc_sum$Source<-"NEW"
rc_sum$Organisation<-"Reef Conservation"

rc_sum$Site<-rc_sum$Sector
rc_sum$Sector<-NA

rc_sum$Site[which(rc_sum$Site=="ALR")]<-"Anse La Raie Lagoon"
rc_sum$Station[which(rc_sum$Station=="ALLC")]<-"Live Lettuce Coral"
rc_sum$Station[which(rc_sum$Station=="ALBC")]<-"Live Branching Coral"
rc_sum$Station[which(rc_sum$Station=="ADC1")]<-"Degraded Coral 1"
rc_sum$Station[which(rc_sum$Station=="ADC2")]<-"Degraded Coral 2"
rc_sum$Station[which(rc_sum$Station=="ST")]<-"Snorkeling trail"

p<-unique(rc_sum[c("Station","Period")])   #to find out which stations have both pre and post-bl. 
#Bel Ombre sites are only post

write.csv(rc_sum,"Reef Conservation benthic formatted.csv",row.names = F)


# Comoros benthic fieldwork data - summarise ------------------------------

comor<-read.csv(file="comoros_fiedlwork_benthic.csv",header = T,stringsAsFactors = F)

test<-ddply(comor,c(1:4),summarise,
            t_tot=sum(Length,na.rm=T))   #all 100 so that is good

#first check there are no errors in the codes


comor$IDLevel.1[which(comor$IDLevel.1=="CM")]<-"cm"
comor$IDLevel.1[which(comor$IDLevel.1=="CE")]<-"ce"
comor$IDLevel.1[which(comor$IDLevel.1=="dc")]<-"DC"
comor$IDLevel.1[which(comor$IDLevel.1=="porb")]<-"porB"
comor$IDLevel.1[which(comor$IDLevel.1=="PorB")]<-"porB"
comor$IDLevel.1[which(comor$IDLevel.1=="snd")]<-"SND"
comor$IDLevel.1[which(comor$IDLevel.1=="CB")]<-"cb"
comor$IDLevel.1[which(comor$IDLevel.1=="CS")]<-"cs"
comor$IDLevel.1[which(comor$IDLevel.1=="ACD")]<-"acd"
comor$IDLevel.1[which(comor$IDLevel.1=="ACT")]<-"act"
comor$IDLevel.1[which(comor$IDLevel.1=="ACB")]<-"acb"
comor$IDLevel.1[which(comor$IDLevel.1=="bs")]<-"BS"
comor$IDLevel.1[which(comor$IDLevel.1=="CME")]<-"cme"
comor$IDLevel.1[which(comor$IDLevel.1=="sc")]<-"SC"
comor$IDLevel.1[which(comor$IDLevel.1=="sil")]<-"SIL"
comor$IDLevel.1[which(comor$IDLevel.1=="ANEM")]<-"Anem"
comor$IDLevel.1[which(comor$IDLevel.1=="CF")]<-"cf"
comor$IDLevel.1[which(comor$IDLevel.1=="Cm")]<-"cm"
comor$IDLevel.1[which(comor$IDLevel.1=="MUSH")]<-"mush"
comor$IDLevel.1[which(comor$IDLevel.1=="GON")]<-"gon"
comor$IDLevel.1[which(comor$IDLevel.1=="hc")]<-"HC"
comor$IDLevel.1[which(comor$IDLevel.1=="hydr")]<-"Hydr"
comor$IDLevel.1[which(comor$IDLevel.1=="UNID/OT")]<-"UNID"

#now need to sum by transect all benthic categories length

comor1<-ddply(comor,c(2:4,6),summarise,
              per_cover=sum(Length,na.rm = T)/10)

test1<-ddply(comor1,c(1:3),summarise,
            t_tot=sum(per_cover,na.rm=T))  #all = 100 so sawa
unique(test1$t_tot)

# order(unique(comor1$IDLevel.1))

codes<-read.csv("benthic_codes_all.csv",header = T,stringsAsFactors = F)

d<-match(comor1$IDLevel.1,codes$Code,nomatch = 0)
which(d==0)

#now need to do the station level average cover for each benthic code

comor2<-ddply(comor1,c(1,2,4),summarise,
              cover=sum(per_cover,na.rm = T))   #sum at each station

comor2n<-ddply(comor1,c(1,2),summarise,
              n=max(Transect..))

comor2$n<-comor2n$n[match(comor2$Station,comor2n$Station)]

comor2$mean_cover<-comor2$cover/comor2$n

test3<-ddply(comor2,c(1,2),summarise,
             tot=sum(mean_cover))
unique(test3$tot)  #all adds up to 100

comor2[,'mean_cover']=round(comor2[,'mean_cover'],2)

comor2<-rename(comor2,c("IDLevel.1"="benthic_code"))

comor2$zone<-NA
comor2$zone[which(comor2$Station=="Candzoni")]<-"Fore reef"
comor2$zone[which(comor2$Station=="Dragoni")]<-"Back reef"
comor2$zone[which(comor2$Station=="Hadawo")]<-"Fore reef"
comor2$zone[which(comor2$Station=="Itsandra")]<-"Back reef"
comor2$zone[which(comor2$Station=="Mea")]<-"Fore reef"
comor2$zone[which(comor2$Station=="Membwabwani")]<-"Back reef"
comor2$zone[which(comor2$Station=="Moroni")]<-"Fore reef"
comor2$zone[which(comor2$Station=="Mtsanga")]<-"Back reef"
comor2$zone[which(comor2$Station=="Sambia")]<-"Fore reef"

comor2$Period<-'Post'
comor2$Organization<-"Comoros fieldwork"
comor2$Country<-'Comoros'

comor3<-comor2[,c(10,2,1,7,3,6,8,9)]

write.csv(comor3,"Comoros_fieldwork_benthic_formatted.csv",row.names = F)


# January South Tanzania 2017 data ----------------------------------------

library(tidyr)

stz<-read.csv(file="2017 south tanzania data wide.csv",header = T,stringsAsFactors = F)

data_long <- gather(stz, benthic_category, mean_cover, 2:16, factor_key=TRUE)   #LONG

data_wide <- spread(data_long, MAJOR.CATEGORY....of.transect., mean_cover)


colnames(data_wide)[3] <- "mean_cover"
colnames(data_wide)[4] <- "sd"


data_wide2<-data_wide[,c(1:4)]

sub(".*? (.+)", "\\1", data_wide2$benthic_category)

data_wide2$code<-gsub("^.*?\\.","", data_wide2$benthic_category)
data_wide2$code<-gsub("\\.","", data_wide2$code)
data_wide2$code2<-gsub("^.*?_","", data_wide2$code)

f<-ddply(data_wide2,c(1),summarise,
         tot=sum(mean_cover))

d<-data_wide2[-which(data_wide2$code2=="WAND_SHADOW_TWS"),]

d1<-d[,c(1,7,3,4)]

write.csv(d1,"2017_S_TZ_fieldwork_data_formatted.csv",row.names = F)


# January Tanga 2017 data -------------------------------------------------
setwd("~/CORDIO/IOC post-bleaching project/Post-bleaching assessment/Formatting/")
library(plyr)

tanga<-read.csv(file="tz_tanga_2017_formatted.csv",header=T, stringsAsFactors = F)

test<-ddply(tanga,c(1),summarise,
            points=sum(length))

#to capitalise the first letter in a string
capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

tanga$Genus<-capFirst(tanga$Genus)

tanga$Genus[which(tanga$Genus=="Acopora")]<-"Acropora"
tanga$Genus[which(tanga$Genus=="Coralline algae")]<-"Algae-coralline"
tanga$Genus[which(tanga$Genus=="Dead standing coral")]<-"Dead Standing coral"
tanga$Genus[which(tanga$Genus=="Invertebrate")]<-"Inverts-other"
tanga$Genus[which(tanga$Genus=="Macroalgae")]<-"Algae-macro/fleshy"
tanga$Genus[which(tanga$Genus=="Recently dead coral")]<-"Recent Dead Coral"
tanga$Genus[which(tanga$Genus=="Turf algae")]<-"Algae-turf"
tanga$Genus[which(tanga$Genus=="Sea grass")]<-"Seagrass"
tanga$Genus[which(tanga$Genus=="Algae coralline")]<-"Algae-coralline"
tanga$Genus[which(tanga$Genus=="Pocillopora ")]<-"Pocillopora"
tanga$Genus[which(tanga$Genus=="Dead coral")]<-"Dead Standing coral"
tanga$Genus[which(tanga$Genus=="Hydnophora")]<-"Hydnopohora"
tanga$Genus[which(tanga$Genus=="Gardinoseris")]<-"Gardineroseris"
tanga$Genus[which(tanga$Genus=="Symphyllia")]<-"Symphillia"
tanga$Genus[which(tanga$Genus=="Synarea")]<-"Sinularia"

tanga2<-merge(ddply(tanga,c(1,2),summarise,
              points=sum(length,na.rm = T)),
              ddply(tanga,c(1),summarise,
              transect_total=sum(length)))

#this is using ddply at two levels in one line of code. the merge helps to connect the second output to the first ddply output

codes<-read.csv(file="benthic_codes_all.csv",header = T,stringsAsFactors = F)

unique(tanga2$Genus)

tanga2$code <- codes$Code[match(tanga2$Genus, codes$Name)]

tanga2$mean_cover<-tanga2$points/tanga2$transect_total*100

# for (i in 1:nrow(tanga2)){
#   tanga2$name2[i]<-grep(tanga2$Genus[i], codes$Name, value = T)
# }

test2<-ddply(tanga2,c(1),summarise,
             total=sum(mean_cover))

tanga3<-tanga2[,c(1,2,5,6)]

write.csv(tanga3,"tanga_2017_data_ready_to_add_to_regional_dataset.csv",row.names = F)
