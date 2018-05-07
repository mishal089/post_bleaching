
# Assigning Coordinates to GCRMN dataset ---------------------------------------

setwd("~/Desktop/work/Merge")
library(plyr)
install.packages("tidyr")
library(tidyr)


GCRMN<-read.csv("GCRMN_benthic.csv",header = F,stringsAsFactors = F)
colnames(GCRMN)<-GCRMN[1,]
GCRMN<-GCRMN[-1,]

coord<- read.csv("GCRMN_geo.csv",header = T,na.strings = "")
colnames(coord)[1] <- "Country"
coord$Longitude<- as.numeric(as.character.factor (coord$Longitude))

#Assign a Unique ID (station and site)
GCRMN[16]<-paste(GCRMN$Site,GCRMN$Station)
colnames(GCRMN)[16] <- "ID"

coord[7]<-paste(coord$Site,coord$Station)
colnames(coord)[7]<- "ID"

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

#Assign coordinates
t<-match(GCRMN$ID,coord$ID,nomatch=0)
u<-which(t==0)

unique(GCRMN$ID[u])
GCRMN$Latitude<-NA
for (i in 1:nrow(GCRMN)){
  if (t[i]!=0){
    GCRMN$Latitude[i]<-coord$Latitude[t[i]]
    GCRMN$Longitude[i]<-coord$Longitude[t[i]]
  }}

GCRMN<- GCRMN[,-16]

write.csv(GCRMN, "GCRMN-with-Coordinates.csv")



# Assigning coordinates to Post bleaching file ----------------------------
PostB<-read.csv("2017_WIO_benthic_pre_analysis.csv",header = F,stringsAsFactors = F)
colnames(PostB)<-PostB[1,]
PostB<-PostB[-1,]

geofile<- read.csv("2017_WIO_geo.csv",header = T,na.strings = "")

#Assign a Unique ID (station and site)
PostB[14]<-paste(PostB$Site,PostB$Station)
colnames(PostB)[14] <- "ID"

geofile[6]<-paste(geofile$Site,geofile$Station)
colnames(geofile)[6]<- "ID"

f<-match(PostB$ID,geofile$ID,nomatch = 0)
which(f==0)
unique(PostB$ID[which(f==0)])

#correct ID names to match
PostB$ID[which(PostB$ID=="Zanzibar Chumbe")]<-"Zanzibar Chumbe_MPA"
PostB$ID[which(PostB$ID=="Ankarea Antsoa_07")]<-"NA Antsoa_07"
PostB$ID[which(PostB$ID=="Ankarea NosyKarabo_06")]<-"NA NosyKarabo_06"
PostB$ID[which(PostB$ID=="Ankarea Tsara_08")]<-"NA Tsara_08"
PostB$ID[which(PostB$ID=="Ankivonjy Anki_14")]<-"NA Anki_14"
PostB$ID[which(PostB$ID=="Ankivonjy Plateau_13")]<-"NA Plateau_13"
PostB$ID[which(PostB$ID=="Soariake Beko_03")]<-"NA Beko_03"
PostB$ID[which(PostB$ID=="Soariake Sal_N2_05")]<-"NA Sal_N2_05"
PostB$ID[which(PostB$ID=="Ankarea Ambari_11")]<-"NA Ambari_11"
PostB$ID[which(PostB$ID=="Ankarea Beangovo_04")]<-"NA Beangovo_04"
PostB$ID[which(PostB$ID=="Ankarea NosyLava_01")]<-"NA NosyLava_01"
PostB$ID[which(PostB$ID=="Ankivonjy Ambatofisaka_19")]<-"NA Ambatofisaka_19"
PostB$ID[which(PostB$ID=="Ankivonjy Kisi_16")]<-"NA Kisi_16"
PostB$ID[which(PostB$ID=="Soariake Andre_07")]<-"NA Andre_07"
PostB$ID[which(PostB$ID=="Ankarea Ampa_10")]<-"NA Ampa_10"
PostB$ID[which(PostB$ID=="Ankarea NosyFisaka_02")]<-"NA NosyFisaka_02"
PostB$ID[which(PostB$ID=="Ankarea NosyVazoa_05")]<-"NA NosyVazoa_05"
PostB$ID[which(PostB$ID=="Ankivonjy Ambatomilay_18")]<-"NA Ambatomilay_18"
PostB$ID[which(PostB$ID=="Ankivonjy Madiro_17")]<-"NA Madiro_17"
PostB$ID[which(PostB$ID=="Soariake Andravona_08")]<-"NA Andravona_08"
PostB$ID[which(PostB$ID=="Soariake Anka_01")]<-"NA Anka_01"
PostB$ID[which(PostB$ID=="Soariake Sal_N1_11")]<-"NA Sal_N1_11"
PostB$ID[which(PostB$ID=="Soariake Tsand_10")]<-"NA Tsand_10"
PostB$ID[which(PostB$ID=="Watamu Uyombo_")]<-"Watamu Uyombo"
PostB$ID[which(PostB$ID=="Ankarea Ankarea_03")]<-"NA Ankarea_03"
PostB$ID[which(PostB$ID=="Ankarea NosyKaine_09")]<-"NA NosyKaine_09"
PostB$ID[which(PostB$ID=="Ankarea Rata_12")]<-"NA Rata_12"
PostB$ID[which(PostB$ID=="Ankivonjy Ankaso_15")]<-"NA Ankaso_15"
PostB$ID[which(PostB$ID=="Ankivonjy Marotogny_20")]<-"NA Marotogny_20"
PostB$ID[which(PostB$ID=="Soariake Andravona_12")]<-"NA Andravona_12"
PostB$ID[which(PostB$ID=="Soariake Sal_N2_02")]<-"NA Sal_N2_02" 

#Assign coordinates
t<-match(PostB$ID,geofile$ID,nomatch=0)
u<-which(t==0)

unique(PostB$ID[u])
PostB$Latitude<-NA
for (i in 1:nrow(PostB)){
  if (t[i]!=0){
    PostB$Latitude[i]<-geofile$Lat[t[i]]
    PostB$Longitude[i]<-geofile$Lon[t[i]]
  }}

PostB<- PostB[,-14]

write.csv(PostB, "Post_bleaching-with-Coordinates.csv")


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
gcs<- read.csv("GCRMN-with-Coordinates.csv",header = F,stringsAsFactors = F)
colnames(gcs)<-gcs[1,]
gcs<-gcs[-1,-1]


pbs<-read.csv("Post_bleaching-with-coordinates.csv",header = F,stringsAsFactors = F)
colnames(pbs)<-pbs[1,]
pbs<-pbs[-1,-1]

#filter out only 'New' sites from the post bleaching source file - pbs
pbs1<-pbs[which(pbs$Source=='New'),]

#match function – using fields – country, site, station, year, benthic category, mean cover

#Check unique countries
f<-match(gcs$Country,pbs1$Country,nomatch = 0)
which(f==0)
unique(gcs$Country[which(f==0)])

#unique sites
f<-match(gcs$Site,pbs1$Site,nomatch = 0)
which(f==0)
unique(gcs$Site[which(f==0)])

#correct all possible names to match, some sites are only in the pre-bleaching period, hence wont match.
gcs$Site[which(gcs$Site=="Alphonse ")]<-"Alphonse"
gcs$Site[which(gcs$Site=="Mahe")]<-"Mahe NW"
gcs$Site[which(gcs$Site=="Denis")]<-"Alphonse"
gcs$Site[which(gcs$Site=="Southcoast")]<-"Kisite"
gcs$Site[which(gcs$Site=="Diani_Chale")]<-"Diani-Chale"
gcs$Site[which(gcs$Site=="Cerf")]<-"Cerf Island"
gcs$Site[which(gcs$Site=="Chongoene_Reef")]<-"Alphonse"
gcs$Site[which(gcs$Site=="Misali")]<-"Pemba"
gcs$Site[which(gcs$Site=="Kwale")]<-"Zanzibar"
gcs$Site[which(gcs$Site=="Alphonse ")]<-"Alphonse"
gcs$Site[which(gcs$Site=="Alphonse ")]<-"Alphonse"
gcs$Site[which(gcs$Site=="Alphonse ")]<-"Alphonse"


#unique benthic categories
f<-match(gcs$benthic_category,pbs1$benthic_category,nomatch = 0)
which(f==0)
unique(gcs$benthic_category[which(f==0)])

#correct the codes to match; the other 3 benthic categories are not present in the post bleaching file. 
gcs$benthic_category[which(gcs$benthic_category=="Lob")]<-"lob"


#merge the 2 files into 1; gcs which is GCRMN and pbs1 which is post bleaching with 'New' filtered out.

WIO_benthic <- rbind.fill(gcs, pbs1)

write.csv(WIO_benthic, "WIO_benthic-combined.csv")



