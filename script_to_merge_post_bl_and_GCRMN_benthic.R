
# Assigning Coordinates to GCRMN dataset ---------------------------------------

# setwd("~/Desktop/work/Merge")
setwd("~/GitHub/post_bleaching/")
library(plyr)
# install.packages("tidyr")
library(tidyr)


GCRMN<-read.csv("GCRMN_benthic.csv",header = T,stringsAsFactors = F)  #change header=T to read in col names
# colnames(GCRMN)<-GCRMN[1,]
# GCRMN<-GCRMN[-1,]

coord<- read.csv("GCRMN_geo.csv",header = T, na.strings = "", stringsAsFactors = F)
# coord<- read.csv("GCRMN_geo.csv",header = T, stringsAsFactors = F)

colnames(coord)[1] <- "Country"
coord$Longitude<- as.numeric(coord$Longitude)

#Assign a Unique ID (station and site)
GCRMN$ID<-paste(GCRMN$Site,GCRMN$Station)

coord$ID<-paste(coord$Site,coord$Station)

f<-match(GCRMN$ID,coord$ID,nomatch = 0)
which(f==0)
unique(GCRMN$ID[which(f==0)])

#Assign correct names present in coordinate file- coord
GCRMN$ID[which(GCRMN$ID=="Kiunga_MNR Ch._Magogoni")]<-"Kiunga_MNR Ch._magogoni"
GCRMN$ID[which(GCRMN$ID=="Diani_Chale Chidzamba_cha_mekka")]<-"Diani-Chale Chidzamba_cha_mekka"
GCRMN$ID[which(GCRMN$ID=="Diani_Chale Chidzamba_cha_mekka")]<-"Diani-Chale Chidzamba_cha_mekka"
GCRMN$ID[which(GCRMN$ID=="Southcoast kisite-sheltered")]<-"Southcoast Kisite-Sheltered"
GCRMN$ID[which(GCRMN$ID=="Diani_Chale Limwinyumwinyu")]<-"Diani-Chale Limwinyumwinyu"
GCRMN$ID[which(GCRMN$ID=="Lamu Majoongoni")]<-"Lamu Majongooni"
GCRMN$ID[which(GCRMN$ID=="Diani_Chale Makonde")]<-"Diani-Chale Makonde"
GCRMN$ID[which(GCRMN$ID=="Southcoast Mpunguti-Upper_")]<-"Southcoast Mpunguti-Upper"
GCRMN$ID[which(GCRMN$ID=="Diani_Chale Mwamba_wa_chala")]<-"Diani-Chale Mwamba_wa_Chala"
GCRMN$ID[which(GCRMN$ID=="Diani_Chale Mwamba_wa_Rashidi")]<-"Diani-Chale Mwamba_wa_Rashidi"
GCRMN$ID[which(GCRMN$ID=="Diani_Chale Mwanyundo")]<-"Diani-Chale Mwanyundo"
GCRMN$ID[which(GCRMN$ID=="Diani_Chale Nguma")]<-"Diani-Chale Nguma"
GCRMN$ID[which(GCRMN$ID=="Diani_Chale Njau")]<-"Diani-Chale Njau"
GCRMN$ID[which(GCRMN$ID=="Inhaca_Island Ponta_Torres_2")]<-"Inhaca_Island Ponta_Torres"
GCRMN$ID[which(GCRMN$ID=="Diani_Chale Shimo_la_chitsanga")]<-"Diani-Chale Shimo_la_chitsanga"
GCRMN$ID[which(GCRMN$ID=="Kiunga_MNR Tenewi_north")]<-"Lamu Tenewi_North"
GCRMN$ID[which(GCRMN$ID=="Diani_Chale Tsipitali")]<-"Diani-Chale Tsipitali"
GCRMN$ID[which(GCRMN$ID=="Diani_Chale Chidzamba_cha_mavovo")]<-"Diani-Chale Chidzamba_cha_mavovo"
GCRMN$ID[which(GCRMN$ID=="Diani_Chale Chidzamba_cha_mwambogoya")]<-"Diani-Chale Chidzamba_cha_mwambogoya"
GCRMN$ID[which(GCRMN$ID=="Diani_Chale Doa_la_msusa")]<-"Diani-Chale Doa_la_msusa"
GCRMN$ID[which(GCRMN$ID=="Diani_Chale Ligagana")]<-"Diani-Chale Ligagana_"
GCRMN$ID[which(GCRMN$ID=="Southcoast MakoKokwe")]<-"Southcoast Makokokwe"
GCRMN$ID[which(GCRMN$ID=="Diani_Chale Mkingamo_gazi")]<-"Diani-Chale Mkingamo_gazi"
GCRMN$ID[which(GCRMN$ID=="Diani_Chale Mtengo")]<-"Diani-Chale Mtengo"
GCRMN$ID[which(GCRMN$ID=="Diani_Chale Mwamba_mkubwa")]<-"Diani-Chale Mwamba_mkubwa"
GCRMN$ID[which(GCRMN$ID=="Diani_Chale Mwamba_wa_Mwatajiri")]<-"Diani-Chale Mwamba_wa_Mwatajiri"
GCRMN$ID[which(GCRMN$ID=="Diani_Chale Mwanadzinani")]<-"Diani-Chale Mwanadzinani"
GCRMN$ID[which(GCRMN$ID=="Diani_Chale Mwaromba")]<-"Diani-Chale Mwaromba"
GCRMN$ID[which(GCRMN$ID=="Diani_Chale Ngunguluni")]<-"Diani-Chale Ngunguluni"
GCRMN$ID[which(GCRMN$ID=="Diani_Chale Shimo_la_mlango")]<-"Diani-Chale shimo_la_mlango"
GCRMN$ID[which(GCRMN$ID=="Lamu Tenewi_north")]<-"Lamu Tenewi_north"
GCRMN$ID[which(GCRMN$ID=="Kiunga_MNR Tenewi_South")]<-"Lamu Tenewi_South"
GCRMN$ID[which(GCRMN$ID=="Watamu Turtle_reef")]<-"Watamu Turtle_Reef"
GCRMN$ID[which(GCRMN$ID=="Diani_Chale Doa_la_maweni")]<-"Diani-Chale Doa_la_maweni"
GCRMN$ID[which(GCRMN$ID=="NA South Reef")]<-"NA South Reef"
GCRMN$ID[which(GCRMN$ID=="Lamu Tenewi_north")]<-"Lamu Tenewi_North"
GCRMN$ID[which(GCRMN$ID=="Diani_Chale Mchingamo_mwadudu")]<-"Diani-Chale Mchingamo_mwadudu"

#Assign coordinates
t<-match(GCRMN$ID,coord$ID,nomatch=0)
u<-which(t==0)

unique(GCRMN$ID[u])   #still 59 IDs that did not match. Is it because of spelling or no information on coordinates?
#Diani_Chale Doa_la_maweni should be Diani-Chale Doa_la_maweni (as in coords)
#Diani_Chale Mchingamo_mwadudu -> Diani-Chale Mchingamo_mwadudu
#NA South Reef -> NA South Reef
#Lamu Tenewi_north -> Lamu Tenewi_North
# the 4 listed above are related to spelling, but the rest are absent from coords dataset

GCRMN$Latitude<-NA
GCRMN$Longitude<-NA
for (i in 1:nrow(GCRMN)){
  if (t[i]!=0){
    GCRMN$Latitude[i]<-coord$Latitude[t[i]]
    GCRMN$Longitude[i]<-coord$Longitude[t[i]]
  }}

GCRMN<- GCRMN[,-22]

write.csv(GCRMN, "GCRMN_with_coords_for_merge.csv", row.names = F)



# Assigning coordinates to Post bleaching file ----------------------------
PostB<-read.csv("2017_WIO_benthic_pre_analysis.csv",header = T,stringsAsFactors = F)

geofile<- read.csv("2017_WIO_geo.csv",header = T,stringsAsFactors = F)

#Assign a Unique ID (station and site)
PostB$ID<-paste(PostB$Site,PostB$Station)
# PostB$ID<-sub(".*? (.+)", "\\1", PostB$ID)
PostB$ID<-sub("^\\s+", "", PostB$ID)  #removes white space before string

geofile$ID<-paste(geofile$Site,geofile$Station)
# geofile$ID<- gsub("$ ","",geofile$ID, perl=T)
# geofile$ID<-sub(".*? (.+)", "\\1", geofile$ID)
geofile$ID<-sub("^\\s+", "", geofile$ID)

f<-match(PostB$ID,geofile$ID,nomatch = 0)
which(f==0)
unique(PostB$ID[which(f==0)])

#correct ID names to match
# PostB$ID[which(PostB$ID=="Zanzibar Chumbe")]<-"Zanzibar Chumbe_MPA"
PostB$ID[which(PostB$ID=="Ankarea Antsoa_07")]<-"Antsoa_07"
PostB$ID[which(PostB$ID=="Ankarea NosyKarabo_06")]<-"NosyKarabo_06"
PostB$ID[which(PostB$ID=="Ankarea Tsara_08")]<-"Tsara_08"
PostB$ID[which(PostB$ID=="Ankivonjy Anki_14")]<-"Anki_14"
PostB$ID[which(PostB$ID=="Ankivonjy Plateau_13")]<-"Plateau_13"
PostB$ID[which(PostB$ID=="Soariake Beko_03")]<-"Beko_03"
PostB$ID[which(PostB$ID=="Soariake Sal_N2_05")]<-"Sal_N2_05"
PostB$ID[which(PostB$ID=="Ankarea Ambari_11")]<-"Ambari_11"
PostB$ID[which(PostB$ID=="Ankarea Beangovo_04")]<-"Beangovo_04"
PostB$ID[which(PostB$ID=="Ankarea NosyLava_01")]<-"NosyLava_01"
PostB$ID[which(PostB$ID=="Ankivonjy Ambatofisaka_19")]<-"Ambatofisaka_19"
PostB$ID[which(PostB$ID=="Ankivonjy Kisi_16")]<-"Kisi_16"
PostB$ID[which(PostB$ID=="Soariake Andre_07")]<-"Andre_07"
PostB$ID[which(PostB$ID=="Ankarea Ampa_10")]<-"Ampa_10"
PostB$ID[which(PostB$ID=="Ankarea NosyFisaka_02")]<-"NosyFisaka_02"
PostB$ID[which(PostB$ID=="Ankarea NosyVazoa_05")]<-"NosyVazoa_05"
PostB$ID[which(PostB$ID=="Ankivonjy Ambatomilay_18")]<-"Ambatomilay_18"
PostB$ID[which(PostB$ID=="Ankivonjy Madiro_17")]<-"Madiro_17"
PostB$ID[which(PostB$ID=="Soariake Andravona_08")]<-"Andravona_08"
PostB$ID[which(PostB$ID=="Soariake Anka_01")]<-"Anka_01"
PostB$ID[which(PostB$ID=="Soariake Sal_N1_11")]<-"Sal_N1_11"
PostB$ID[which(PostB$ID=="Soariake Tsand_10")]<-"Tsand_10"
PostB$ID[which(PostB$ID=="Watamu Uyombo_")]<-"Watamu Uyombo"
PostB$ID[which(PostB$ID=="Ankarea Ankarea_03")]<-"Ankarea_03"
PostB$ID[which(PostB$ID=="Ankarea NosyKaine_09")]<-"NosyKaine_09"
PostB$ID[which(PostB$ID=="Ankarea Rata_12")]<-"Rata_12"
PostB$ID[which(PostB$ID=="Ankivonjy Ankaso_15")]<-"Ankaso_15"
PostB$ID[which(PostB$ID=="Ankivonjy Marotogny_20")]<-"Marotogny_20"
PostB$ID[which(PostB$ID=="Soariake Andravona_12")]<-"Andravona_12"
PostB$ID[which(PostB$ID=="Soariake Sal_N2_02")]<-"Sal_N2_02" 
PostB$ID[which(PostB$ID=="Soariake Sal_N2_13")]<-"Sal_N2_13" 
PostB$ID[which(PostB$ID=="Soariake Antsa_09")]<-"Antsa_09" 

#Assign coordinates
t<-match(PostB$ID,geofile$ID,nomatch=0)
u<-which(t==0)
unique(PostB$ID[u])

PostB$Latitude<-NA
PostB$Longitude<-NA

for (i in 1:nrow(PostB)){
  if (t[i]!=0){
    PostB$Latitude[i]<-geofile$Lat[t[i]]
    PostB$Longitude[i]<-geofile$Lon[t[i]]
  }}

#create a subset dataframe which is only the postbleaching data for nonmatched - 36 sites
subpb<-PostB[u,]

#match this unmatched dataset with the coord dataset from gcrmn to get missing coords
t1<-match(subpb$ID,coord$ID,nomatch=0)
u1<-which(t1==0)
unique(subpb$ID[u1])    #24 sites which do not match with gcrmn coords

subpb$ID2<-subpb$ID
subpb$ID2[which(subpb$ID2=="Watamu Turtle_reef")]<-"Watamu Turtle_Reef" 
subpb$ID2[which(subpb$ID2=="Alphonse A9")]<-"Alphonse  A9" 
subpb$ID2[which(subpb$ID2=="Alphonse A2")]<-"Alphonse  A2" 
subpb$ID2[which(subpb$ID2=="Alphonse A3")]<-"Alphonse  A3" 

t2<-match(subpb$ID2,coord$ID,nomatch=0)
u2<-which(t2==0)
unique(subpb$ID[u2]) 
#now only 20 which do not match - do not have coordinates for these

#loop to assign coordinates

for (i in 1:nrow(subpb)){
  if (t2[i]!=0){
    subpb$Latitude[i]<-coord$Latitude[t2[i]]
    subpb$Longitude[i]<-coord$Longitude[t2[i]]
  }}

#match this newly matched dataset with the original postbleaching

# PostB$Latitude <- subpb$Latitude[match(PostB$ID, subpb$ID)]
# PostB$Longitude2 <- subpb$Longitude[match(PostB$ID, subpb$ID)]

t3<-match(PostB$ID,subpb$ID,nomatch=0)
u3<-which(t3==0)
unique(PostB$ID[u3]) 
#now only 20 which do not match - do not have coordinates for these

#loop to assign coordinates

for (i in 1:nrow(PostB)){
  if (t3[i]!=0){
    PostB$Latitude[i]<-subpb$Latitude[t3[i]]
    PostB$Longitude[i]<-subpb$Longitude[t3[i]]
  }}


PostB<- PostB[,-14]

write.csv(PostB, "Post_bleaching-with-Coordinates.csv", row.names = F)


# Creating source files from raw country files ---------------------------

# Tanzania only

Source_Tz<- read.csv("Source_Tz.csv",header = F,stringsAsFactors = F)
colnames(Source_Tz)<-Source_Tz[1,]
Source_Tz<-Source_Tz[-1,]
colnames(Source_Tz)[1] <- "Country"

GCRMN<-read.csv("GCRMN_benthic.csv",header = F,stringsAsFactors = F)
colnames(GCRMN)<-GCRMN[1,]
GCRMN<-GCRMN[-1,]
TZ_GCRMN<- GCRMN[which(GCRMN$Country=='Tanzania'),]

#checking for unique sites between Source_TZ and filtered Tz_GCRMN. create a 'GCRMN site column in the source file. 
Source_Tz$GCRMN_Site<- Source_Tz$Site

f<-match(Source_Tz$GCRMN_Site,TZ_GCRMN$Site,nomatch = 0)
which(f==0)
unique(Source_Tz$GCRMN_Site[which(f==0)])

#Edit sites names in source file to match GCRMN
Source_Tz$GCRMN_Site[which(Source_Tz$GCRMN_Site=="Kilwa")]<-"Songosongo"
Source_Tz$GCRMN_Site[which(Source_Tz$GCRMN_Site=="MBCA")]<-"Zanzibar"
Source_Tz$GCRMN_Site[which(Source_Tz$GCRMN_Site=="Changuu")]<-"Zanzibar"
Source_Tz$GCRMN_Site[which(Source_Tz$GCRMN_Site=="Chumbe")]<-"Zanzibar"
Source_Tz$GCRMN_Site[which(Source_Tz$GCRMN_Site=="Kipumbwi")]<-"Tanga"
Source_Tz$GCRMN_Site[which(Source_Tz$GCRMN_Site=="PECCA")]<-"Mtwara"
Source_Tz$GCRMN_Site[which(Source_Tz$GCRMN_Site=="MIMCA")]<-"Zanzibar"
Source_Tz$GCRMN_Site[which(Source_Tz$GCRMN_Site=="Mnemba")]<-"Zanzibar"
Source_Tz$GCRMN_Site[which(Source_Tz$GCRMN_Site=="Pange")]<-"Zanzibar"
Source_Tz$GCRMN_Site[which(Source_Tz$GCRMN_Site=="Bawe")]<-"Zanzibar"
Source_Tz$GCRMN_Site[which(Source_Tz$GCRMN_Site=="DMR")]<-"Dar_es_Salaam"
Source_Tz$GCRMN_Site[which(Source_Tz$GCRMN_Site=="Kigombe")]<-"Tanga"
Source_Tz$GCRMN_Site[which(Source_Tz$GCRMN_Site=="CHICOP")]<-"Zanzibar"
Source_Tz$GCRMN_Site[which(Source_Tz$GCRMN_Site=="Mafia island")]<-"Mafia"
Source_Tz$GCRMN_Site[which(Source_Tz$GCRMN_Site=="Ushongo")]<-"Tanga"
Source_Tz$GCRMN_Site[which(Source_Tz$GCRMN_Site=="PANGANI")]<-"Tanga"
Source_Tz$GCRMN_Site[which(Source_Tz$GCRMN_Site=="Tumbatu")]<-"Zanzibar"
Source_Tz$GCRMN_Site[which(Source_Tz$GCRMN_Site=="Mwarongo")]<-"Tanga"
Source_Tz$GCRMN_Site[which(Source_Tz$GCRMN_Site=="Zanzibar town")]<-"Zanzibar"
Source_Tz$GCRMN_Site[which(Source_Tz$GCRMN_Site=="Moa")]<-"Tanga"
Source_Tz$GCRMN_Site[which(Source_Tz$GCRMN_Site=="MIMP")]<-"Mafia"
Source_Tz$GCRMN_Site[which(Source_Tz$GCRMN_Site=="Boma")]<-"Tanga"
Source_Tz$GCRMN_Site[which(Source_Tz$GCRMN_Site=="Mikindani Bay")]<-"Mtwara"
Source_Tz$GCRMN_Site[which(Source_Tz$GCRMN_Site=="Mwambani")]<-"Tanga"
Source_Tz$GCRMN_Site[which(Source_Tz$GCRMN_Site=="Chumbe Island Coral Park (CHICOP)")]<-"Zanzibar"
Source_Tz$GCRMN_Site[which(Source_Tz$GCRMN_Site=="Nyange ")]<-"Zanzibar"


f<-match(Source_Tz$GCRMN_Site,TZ_GCRMN$Site,nomatch = 0)
which(f==0)
unique(Source_Tz$GCRMN_Site[which(f==0)])

#check station names: Add GCRMN_Station 1st.
Source_Tz$GCRMN_Station<- Source_Tz$Station

f<-match(Source_Tz$GCRMN_Station,TZ_GCRMN$Station,nomatch = 0)
which(f==0)
unique(Source_Tz$GCRMN_Station[which(f==0)])

Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Amana East")]<-"Amana_East"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Amana South")]<-"Amana_S"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Bawe 1")]<-"Bawe"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Bawe 2")]<-"Bawe"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Bawe 3")]<-"Bawe"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Bunju reef")]<-"Bunju"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Bwahari reef")]<-"Bwahari"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="C/Kati")]<-"Kati"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Changuu 1")]<-"Changuu"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Changuu 2")]<-"Changuu"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Changuu 3")]<-"Changuu"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Chumbe 1")]<-"Chumbe"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Chumbe 3")]<-"Chumbe"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Chumbe_North")]<-"Chumbe"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Chumbe_South")]<-"Chumbe"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Chundo/Kiroba")]<-"Chundo"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Dambwe Reef")]<-"Dambwe"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Fisi North")]<-"Fisi_N"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Fisi South")]<-"Fisi_S"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Fungu Boma")]<-"Fungu_Boma"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Fungu Nyama")]<-"Fungu_Nyama"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Fungu Zinga reef")]<-"Fungu_Zinga"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Jambe Reef")]<-"Jambe"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Kipwa Mtu (Kichwa Mtu)")]<-"Kichwa_Mtu"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Kipwani Reef")]<-"Kipwani"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Kisiwa Kikubwa")]<-"Kisiwa_Kikubwa"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Kitanga Reef")]<-"Kitanga"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Kitutia North")]<-"Kitutia"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Kwale 1")]<-"Kwale"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Kwale 2")]<-"Kwale"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Kwale 3")]<-"Kwale"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Machangi East")]<-"Machangi"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Majovu reef")]<-"Majovu"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Makome Reef")]<-"Makome"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Maziwe Reef")]<-"Maziwe"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Mijimile ndogo Reef")]<-"Mijimile_Ndogo"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Milimani North")]<-"Milimani"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Milimani South")]<-"Milimani"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Misali 1")]<-"Misali_Island"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Misali 2")]<-"Misali_Island"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Misali 3")]<-"Misali_Island"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Misali 4")]<-"Misali_Island"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Misali 5")]<-"Misali_Island"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Misali 6")]<-"Misali_Island"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Misali 7")]<-"Misali_Island"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Misali 8")]<-"Misali_Island"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Mlangoni")]<-"Mtwara"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Mmongo")]<-"Mtwara"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Msangamkuu-A")]<-"Mtwara"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Msangamkuu-B")]<-"Mtwara"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Mwamba Mkuu")]<-"Mwamba_Mkuu"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Mwamba mkuu")]<-"Mwamba_Mkuu"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Mwamba mkuu-Rufiji")]<-"Mwamba_Mkuu_Ruf"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Mwamba wa Kati")]<-"Mwamba_wa_Kati_"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Mwani Reef")]<-"Mwani"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Naumbu Reef")]<-"Naumbu"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Nyamalile North")]<-"Nyamalile_N"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Nyamalile South")]<-"Nyamalile_S"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Pemba reef")]<-"Pemba"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Shengue Reef")]<-"Shenguwe"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Songo North")]<-"Songo_N"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Taa Reef")]<-"Taa"
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Upangu Reef")]<-"Upangu" 
Source_Tz$GCRMN_Station[which(Source_Tz$GCRMN_Station=="Wamba Reef")]<-"Wamba"


f<-match(Source_Tz$GCRMN_Station,TZ_GCRMN$Station,nomatch = 0)
which(f==0)
unique(Source_Tz$GCRMN_Station[which(f==0)])


write.csv(Source_Tz, "Source_Tz.csv")



# Merging GCRMN with Source and Post bleaching with Source ----------------
library(dplyr)
gcs<- read.csv("GCRMN_with_coords_for_merge.csv",header = T,stringsAsFactors = F)


pbs<-read.csv("Post_bleaching-with-Coordinates.csv",header = T,stringsAsFactors = F)

#filter out only 'New' sites from the post bleaching source file - pbs
pbs1<-pbs[which(pbs$Source=='New'),]

#match function – using fields – country, site, station, year, benthic category, mean cover

#Check unique countries
unique(pbs1$Country)
unique(gcs$Country)
# f<-match(gcs$Country,pbs1$Country,nomatch = 0)
# which(f==0)
# unique(gcs$Country[which(f==0)])

#unique sites
# f<-match(gcs$Site,pbs1$Site,nomatch = 0)
# which(f==0)
# unique(gcs$Site[which(f==0)])
# 
# #correct all possible names to match, some sites are only in the pre-bleaching period, hence wont match.
# gcs$Site[which(gcs$Site=="Alphonse ")]<-"Alphonse"
# gcs$Site[which(gcs$Site=="Mahe")]<-"Mahe NW"
# # gcs$Site[which(gcs$Site=="Denis")]<-"Alphonse"
# gcs$Site[which(gcs$Site=="Southcoast")]<-"Kisite"
# gcs$Site[which(gcs$Site=="Diani_Chale")]<-"Diani-Chale"
# gcs$Site[which(gcs$Site=="Cerf")]<-"Cerf Island"
# gcs$Site[which(gcs$Site=="Chongoene_Reef")]<-"Alphonse"
# gcs$Site[which(gcs$Site=="Misali")]<-"Pemba"
# gcs$Site[which(gcs$Site=="Kwale")]<-"Zanzibar"
# # gcs$Site[which(gcs$Site=="Alphonse ")]<-"Alphonse"
# # gcs$Site[which(gcs$Site=="Alphonse ")]<-"Alphonse"
# # gcs$Site[which(gcs$Site=="Alphonse ")]<-"Alphonse"
# 

#correct the codes to match; the other 3 benthic categories are not present in the post bleaching file. 
gcs$benthic_category[which(gcs$benthic_category=="Lob")]<-"lob"

#YOU did not do the most important step - which is merge the two datasets based on a unique identifier

#gcrmn dataset station names do not have _ like the postbleaching dataset
gcs$Station<-gsub(" $","",gcs$Station,perl=T) #removes _ at the end of character
# gcs$station3<-gcs$Station
gcs$Station<-gsub(" ","_",gcs$Station)

gcs$Station[which(gcs$Station=="Willies_Bay_Reef")]<-"Willie's_Bay_Reef"
gcs$Station[which(gcs$Station=="White_Villa")]<-"White_Villa_Reef"
gcs$Station[which(gcs$Station=="Port_Launay_West_Rock")]<-"Port_Launay_West_Rocks"
gcs$Station[which(gcs$Station=="Baie_Ternay_North_East")]<-"Bay_Ternay_North_East"
gcs$Station[which(gcs$Station=="Baie_Ternay_Centre")]<-"Bay_Ternay_Centre"
gcs$Station[which(gcs$Station=="Baie_Ternay_North_West")]<-"Bay_Ternay_North_West"
gcs$Station[which(gcs$Station=="NS_Coco_Beach")]<-"Ns_Coco_Beach"

#start with country and station to see what stations are common in both
pbs1$ID<-paste(pbs1$Country,pbs1$Station)
  gcs$ID<-paste(gcs$Country,gcs$Station)

fst<-match(pbs1$ID,gcs$ID,nomatch = 0)
yst<-which(fst==0)
g1<-unique(pbs1$ID[which(fst!=0)])  #IDs that matched
g2<-unique(pbs1$ID[yst])    #IDs which are not in both
# g2a<-unique(pbs1$Period[yst])

g<-unique(pbs1[c("Country","Station")])  #there are 172 unique stations in post-bleaching data

#88 stations that are common between gcrmn and post-bleaching datasets
#there could be more but because of spelling they did not come up
#need to list the stations that did not match - 84 stations (g2)
#after this - check those against the gcrmn to make sure they are really not in gcrmn

g3<-unique(gcs[c("Country","Station")])

#could also look at in the post-bleaching data, for these 93 stations, how much of the data was 'pre'
#any 'post' data would definitely not be in the gcrmn dataset
#but still good practice to go over the two datasets to correct inconsistencies in site and station names

pbsub<-pbs1[yst,]   #subset of post-bl data with stations that may not be in gcrmn
unique(pbsub$Station[which(pbsub$Period=='Pre')])    #list of stations with 'pre' data to check if really not in gcrmn

#this is a list of 54 stations in pb dataset which have pre-bleaching data

#now look through these sites to see which could be in GCRMN dataset - none of them are except chumbe mpa and misali possibly

#the rest are new sites not in the gcrmn

#and then add year to see if for those stations, are you adding new data
#excluding site because there is so much discrepancy in how site was written between the two
pbs1$ID2<-paste(pbs1$Country,pbs1$Year,pbs1$Station)
gcs$ID2<-paste(gcs$Country,gcs$Year,gcs$Station)


f1<-match(pbs1$ID2,gcs$ID2,nomatch = 0)
y<-which(f1==0)
unique(pbs1$ID2[which(f1!=0)])   #list of stations that could have overlapping data/years

#this shows that all the pre-bleacing data from GVI in the post-ble data for example was from different years to the gcrmn dataset

#BLUE VENTURES 3 SITES HAVE DATA IN BOTH - LOST, NS COCO BEACH AND RECRUITMENT NORTH
#possible issues with Chumbe MPA and Chumbe (GCRMN)

#Add in Chumbe MPA data as is for now into GCRMN
#Delete the BV Mad data that is in the GCRMN and replace with post-bleaching data

fr<-match(gcs$ID2,pbs1$ID2,nomatch = 0)
yr<-which(fr==0)
unique(gcs$ID2[which(fr!=0)])

gcs1<-gcs[-which(fr!=0),]

#and then add all pbs1 data to gcrmn

#some cleaning before merging
#post-ble - rename 'benthic name 2' - 'benthic name'. delete columns 16,17
#gcrmn - rename zone-reef zone, sd cover - sd. delete columns 20,19,16,15. add organization, source, period columns

colnames(pbs1)[7] <- "benthic_name"
pbs2<-pbs1[,c(1:15)]
pbs2$Source<-'post_bl_2016'

colnames(gcs1)[7] <- "benthic_code"
colnames(gcs1)[9] <- "sd"
colnames(gcs1)[11] <- "Reef.zone"

gcs2<-gcs1[,c(1:14,17,18)]

gcs2$Organization<-NA
gcs2$Period<-'Pre'
gcs2$Source<-'GCRMN 2015'

WIO_benthic <- rbind.fill(gcs2, pbs2)

write.csv(WIO_benthic,"2017_WIO_GCRMN_benthic_dataset.csv",row.names = F)


#still to do
# 1. consitency between site and station names from post-bleaching and gcrmn dataset
# 2. chumbe mpa and chumbe gcrmn data - what is duplicate and what is not?
# 3. Organization field for GCRMN data to be filled


