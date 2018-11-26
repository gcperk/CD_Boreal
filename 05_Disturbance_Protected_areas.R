
## Script to read in ESRI data tables and combine outputs

##https://gis.stackexchange.com/questions/265863/how-to-read-a-feature-class-in-an-esri-personal-geodatabase-using-r
##http://rstudio-pubs-static.s3.amazonaws.com/255550_1c983c8a6a5a45a0aee5a923206f4818.html
#http://www.rspatial.org/spatial/rst/7-vectmanip.html#spatial-queries

library(dplyr)
library(rgdal)
library(sp)
library(raster)
library(rgeos)
library(maptools)
library(magrittr)
library(tibble)
library(tidyr)
library(sf)

out.dir = "X:/projects/Desktop_Analysis/data/output1/"
temp.dir = "X:/projects/Desktop_Analysis/data/temp/"

# The input file geodatabase
##Dissolved  = "X:/projects/Desktop_Analysis/data/Boreal.gdb"
#Intersect = "X:/projects/Desktop_Analysis/data/scratch.gdb"
Dissolved  = "X:/projects/Desktop_Analysis/data/scratch1.gdb"
Intersect = "X:/projects/Desktop_Analysis/data/scratch1.gdb"
Base  = "X:/projects/Desktop_Analysis/data/Base_data.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
dis_list <- ogrListLayers(Dissolved); print(dis_list)
int_list <- ogrListLayers(Intersect); print(int_list)
base_list <- ogrListLayers(Base); print(base_list)

#####################################################################
# Read in herd boundary layers 
b.core <- st_read(dsn=Base,layer="Boreal_core_BC") #; b.core$area.m = st_area(b.core)
b.range <- st_read(dsn=Base,layer="Boreal_range_BC") #; b.range$area.m = st_area(b.range)
b.core.r <- st_intersection(b.range,b.core) #;  b.core.r$area.m = st_area(b.core.r)
plot(st_geometry(b.core.r))#; plot(st_geometry(b.range),add = T)
st_crs(b.core)

# calculate the area of core/range/peripery to calculate % values 
Herd_key <- read.csv(paste(out.dir,"Herd_key.csv",sep = ""))

##############################################################################################
# Read in protected layers 

prot <- st_read(dsn=Intersect,layer="Protect_noOL") # ; st_crs(prot)# check CRS
prot <- st_transform(prot,crs = 3005) # ; plot(st_geometry(prot), add = T)
#unique(prot$desig)

## Range boundary 
    r.prot <-  st_intersection(b.range,prot)
    r.prot.0 = st_cast(r.prot,"POLYGON")
    r.prot.0$area.m = st_area(r.prot.0)
 
    ### Convert to tables and then calculate the areas 
    r.prot.df = data.frame(r.prot.0)        # calculate the length per range 
    r.prot.all.summary <- r.prot.df %>% 
      group_by(Range) %>% 
      summarise(R_allpro.area.m2 = sum(area.m))

    # calculate per type and range 
    r.prot.summary <-  r.prot.df %>% 
      group_by(Range,desig) %>% 
      summarise(R_Pro.Type.area.m2 = sum(area.m))%>% 
      mutate(desig2 = paste("R_",desig,sep = ""))
    # re- arrnage the tabel and add 0 to NA values 
    r.prot.summary <- r.prot.summary[,-2] # remove the "desig" column
    r.type.prot.sum = r.prot.summary %>% spread_(key = "desig2",value = 'R_Pro.Type.area.m2')
    r.type.prot.sum [is.na( r.type.prot.sum )]<- 0 
    
    # join the tables to the herd key 
    Pro_sum = left_join(Herd_key,r.prot.all.summary)
    Pro_sum = left_join(Pro_sum,r.type.prot.sum)

## Core boundary 
    c.prot <-  st_intersection(b.core.r,prot)
    c.prot.0 = st_cast(c.prot,"POLYGON")
    c.prot.0$area.m = st_area(c.prot.0)
    
    ### Convert to tables and then calculate the areas 
    c.prot.df = data.frame(c.prot.0)        # calculate the length per range 
    
    c.prot.all.summary <- c.prot.df %>% 
      group_by(Range) %>% 
      summarise(C_allpro.area.m2 = sum(area.m))
    
    # calculate per type and range 
    c.prot.summary <-  c.prot.df %>% 
      group_by(Range,desig) %>% 
      summarise(C_Pro.Type.area.m2 = sum(area.m))%>% 
      mutate(desig2 = paste("C_",desig,sep = ""))
    
    c.prot.summary <- c.prot.summary[,-2] # remove the "desig" column
    # re- arrnage the tabel and add 0 to NA values 
    c.type.prot.sum = c.prot.summary %>% spread_(key = "desig2",value = 'C_Pro.Type.area.m2')
    c.type.prot.sum [is.na(c.type.prot.sum )]<- 0 
    
    # join the tables to the herd key
    Pro_sum  = left_join(Pro_sum,c.prot.all.summary)
    Pro_sum = left_join(Pro_sum,c.type.prot.sum)
    
    # calculate the Peripery for all of the protection types 
    # note this will need to be ammended depending on the protection types within each of the boundaries. 
    
    Pro_sum <- Pro_sum %>% 
      mutate(P_allpro.area.m2 = R_allpro.area.m2-C_allpro.area.m2,
            P_02_park_er = R_02_park_er - C_02_park_er, 
            P_03_park_provincial = R_03_park_provincial -  C_03_park_provincial,
            P_05_park_protectedarea = R_05_park_protectedarea - C_05_park_protectedarea,
            P_16_mineral_reserve = R_16_mineral_reserve - C_16_mineral_reserve,
            P_17_uwr_no_harvest = R_17_uwr_no_harvest- C_17_uwr_no_harvest,
            P_18_wha_no_harvest = R_18_wha_no_harvest- C_18_wha_no_harvest,
            P_29_rec_site =    R_29_rec_site -  C_29_rec_site,
            P_30_vqo_retain = R_30_vqo_retain-C_30_vqo_retain,
            P_32_land_act_reserves = R_32_land_act_reserves-R_32_land_act_reserves,
            P_33_uwr_conditional_harvest = R_33_uwr_conditional_harvest-C_33_uwr_conditional_harvest,
           # P_34_wha_conditional_harvest = R_34_wha_conditional_harvest-0,
            P_35_vqo_partretain = R_35_vqo_partretain-C_35_vqo_partretain,
            P_36_vqo_modify = R_36_vqo_modify - C_36_vqo_modify,    
            P_38_vqo_maxmodify = R_38_vqo_maxmodify - C_38_vqo_maxmodify) 
    
    names(Pro_sum)
    

###########################################    
    
write.csv(Pro_sum,paste(out.dir,"Draft_disturbance_raw.csv",sep= "")) 
  
    
######################################

# Format into a single table     
tout = Pro_sum 
    
# Loop through each herd and create a full table # change to the number fo herds 
herds = c('Calendar','Chinchaga','Maxhamish','Snake-Sahtahneh','Westside Fort Nelson')

for (i in 1:length(herds)) { 
  #ii = herds[2]
  ii = herds[i]
  ################ All Disturbance ########################
  r.temp <- tout %>% filter(Range == paste(ii))
  r.temp = reshape2:::melt(r.temp)
  r.temp = r.temp %>% mutate(Boundary = substr(variable,1,1)) 
  r.temp = r.temp %>% mutate(Boundary = substr(variable,1,1)) 
  r.temp  <- r.temp  %>% mutate(Dist_type = gsub(".*R_|_m2.*","", variable)) 
  r.temp  <- r.temp  %>%  mutate(Dist_type = gsub("C_","",Dist_type))
  r.temp <- r.temp   %>%  mutate(Dist_type = gsub("P_","",Dist_type))
  #r.temp <- r.temp   %>%  mutate(Dist_type = gsub("_m","",Dist_type))
  #r.temp <- r.temp   %>%  mutate(Dist_type = gsub("_area","",Dist_type))
  
  # get the total areas (ha to calculate density)
  t.r =r.temp %>% filter(Boundary =='R' & Dist_type == "area_ha") %>%  dplyr::select(value)
  t.p =r.temp %>% filter(Boundary =='P' & Dist_type == "area_ha") %>%  dplyr::select(value)
  t.c = r.temp %>% filter(Boundary =='C' & Dist_type == "area_ha") %>%  dplyr::select(value)
  
  # add columns to the totals for RPC  
  r.temp <- r.temp %>% mutate(Total_area_ha = ifelse(Boundary =='R',t.r,
                                                     ifelse(Boundary =='P',t.p,
                                                            ifelse(Boundary == 'C',t.c,0))))
  
  # calculate ha and Percentage of area per disturbance type. 
  r.temp <-r.temp  %>% 
    mutate(Area_ha = value/10000) %>% 
    mutate(Percentage = (Area_ha/ as.numeric(Total_area_ha))*100) %>%
    dplyr::select(-c(variable, value,Total_area_ha)) 
  
  # get rid of the area_total rows 
  r.temp=r.temp[-(grep("area_ha",r.temp$Dist_type)),]
  r.temp = r.temp[-(grep("X",r.temp$Dist_type)),]
  r.sum.area = r.temp %>% dplyr::select(c(Range,Dist_type,Boundary,Area_ha)) %>% spread(Boundary,Area_ha)
  r.sum.pc = r.temp %>% dplyr::select(c(Range,Dist_type,Boundary,Percentage)) %>% 
    mutate(Percentage = round(Percentage,2)) %>% 
    spread(Boundary,Percentage)
  
  r.out <- left_join(r.sum.pc,r.sum.area, by = c("Dist_type",'Range')) 
  r.out <- r.out %>% 
    mutate(Range_pc = R.x, Periphery_pc = P.x,Core_pc = C.x, Range_ha = R.y, Periphery_ha = P.y,Core_ha = C.y )%>% 
    dplyr::select(-c(C.x,P.x,R.x, C.y,P.y,R.y ))
  write.csv(r.out,paste(temp.dir,"protect_draft_",ii,".csv",sep = "")) 
  } 
  

## To do 
## Once this has run through open the csv's generated and merge into a single table or three tables (HA or PC) 

## final xls sheet was named in Boreal_protection 








 
  
  
  
  
  
  ############################################################
  ### Add the detailed data of ##############
  
  p.temp = dis8 %>% filter(Range == paste(i))
  if(length(p.temp$Range)>0){ 
    p.temp = reshape2:::melt(p.temp)
    p.temp = p.temp %>% mutate(Boundary = substr(variable,1,1)) 
    p.temp = p.temp %>% mutate(Boundary = substr(variable,1,1)) 
    p.temp  <- p.temp  %>% mutate(Dist_type = gsub(".*R_|_m2.*","", variable)) 
    p.temp  <- p.temp  %>%  mutate(Dist_type = gsub("C_","",Dist_type))
    p.temp <- p.temp   %>%  mutate(Dist_type = gsub("P_","",Dist_type))
    
    # add columns to the totals for RPC  
    p.temp <- p.temp %>% mutate(Total_area_ha = ifelse(Boundary =='R',t.r,
                                                       ifelse(Boundary =='P',t.p,
                                                              ifelse(Boundary == 'C',t.c,0))))
    # calculate ha and Percentage of area per disturbance type. 
    p.temp <-p.temp  %>% 
      mutate(Area_ha = value/10000) %>% 
      mutate(Percentage = (Area_ha/ as.numeric(Total_area_ha))*100) %>%
      mutate(Dist_type = paste("Pest:",PEST_SPECIES_CODE,sep = "")) %>% 
      dplyr::select(-c(variable, value, PEST_SPECIES_CODE)) 
    # get rid of the area_total rows 
    p.temp = p.temp[-(grep("X",p.temp$Boundary)),]
    r.temp = rbind( r.temp,p.temp)
  } 
  
  
    
    
    




##### Old code for individual layers" 

## # Protected layers # by individual
#UWR <- st_read(dsn=Intersect,layer="UWR_r_int") # read in as sf type 
#OGMA <- st_read(dsn=Intersect,layer="OGMA_LEGAL_r_int") # read in as sf type 
#WHA <- st_read(dsn=Intersect,layer="WHA_BC") # read in as sf type 
#NP <- st_read(dsn=Intersect,layer="NP_r_int") # read in as sf type 
#Prot <- st_read(dsn=Intersect,layer="Prot_R_Int") # read in as sf type 
#Prot <- Prot[Prot$PROTECTED_LANDS_CODE %in% c("PP","OI"),]

######################################################################
# UWR
######################################################################
# RANGE
int <- st_intersection(b.range, UWR)
int$areaUWR <- st_area(int) 
# out put the values as table 
tb_UWR_by_range <- int %>% group_by(Range) %>% summarise(areaUWR = sum(areaUWR))
tb_UWR_by_range$areaUWR <- as.numeric(tb_UWR_by_range$areaUWR/10000)
r_out_uwr<- data.frame(tb_UWR_by_range)
r.uwr.total = sum(r_out_uwr$areaUWR)

# perifery 
int <- st_intersection(b.per,UWR)
int$areaUWR <- st_area(int) 
# out put the values as table 
tb_UWR_by_range <- int %>% group_by(Range) %>% summarise(areaUWR = sum(areaUWR))
tb_UWR_by_range$areaUWR <- as.numeric(tb_UWR_by_range$areaUWR/10000)
p_out_uwr<- data.frame(tb_UWR_by_range)
p.uwr.total = sum(p_out_uwr$areaUWR)

# CORE
# intersect Core and UWR 
int <- st_intersection(b.core, UWR)
int$areaUWR <- st_area(int) 
# out put the values as table 
tb_UWR_by_core <- int %>% group_by(CORE_NAME) %>% summarise(areaUWR = sum(areaUWR))
tb_UWR_by_core$areaUWR <- as.numeric(tb_UWR_by_core$areaUWR/10000)
out_uwr<- data.frame(tb_UWR_by_core)
c.uwr.total = sum(out_uwr$areaUWR)

#Total % 
total.pc.UWR.r = r.uwr.total /boreal_t_range * 100
total.pc.UWR.c = c.uwr.total / boreal_t_core * 100
total.pc.UWR.p = p.uwr.total / boreal_t_per * 100

######################################################################
# NP
#####################################################################
# RANGE 
int <- st_intersection(b.range,NP)
int$areaNP <- st_area(int) 
# out put the values as table 
tb_NP_by_range <- int %>% group_by(Range) %>% summarise(areaNP = sum(areaNP))
tb_NP_by_range$areaNP <- as.numeric(tb_NP_by_range$areaNP/10000)
r_out_NP<- data.frame(tb_NP_by_range)
r.NP.total = sum(r_out_NP$areaNP)

# Perifery 
int <- st_intersection(b.per,NP)
int$areaNP <- st_area(int) 
# out put the values as table 
tb_NP_by_range <- int %>% group_by(Range) %>% summarise(areaNP = sum(areaNP))
tb_NP_by_range$areaNP <- as.numeric(tb_NP_by_range$areaNP/10000)
p_out_NP<- data.frame(tb_NP_by_range)
p.NP.total = sum(p_out_NP$areaNP)

# intersect Core and NP
int <- st_intersection(b.core,NP)
int$areaNP <- st_area(int) 
## out put the values as table 
tb_NP_by_core <- int %>% group_by(CORE_NAME) %>% summarise(areaNP = sum(areaNP))
tb_NP_by_core$areaNP <- as.numeric(tb_NP_by_core$areaNP/10000)
out_NP<- data.frame(tb_NP_by_core)
c.NP.total = sum(out_NP$areaNP)

#Total % 
total.pc.NP.r = r.NP.total/boreal_t_range * 100
total.pc.NP = c.NP.total / boreal_t_core * 100
total.pc.NP.p = p.NP.total / boreal_t_per * 100


######################################################################
# Protect
#####################################################################
# RANGE 
int <- st_intersection(b.range,Prot)
int$areaProt <- st_area(int) 
# out put the values as table 
tb_Prot_by_range <- int %>% group_by(Range) %>% summarise(areaProt = sum(areaProt))
tb_Prot_by_range$areaProt <- as.numeric(tb_Prot_by_range$areaProt/10000)
r_out_Prot<- data.frame(tb_Prot_by_range)
r.Prot.total = sum(r_out_Prot$areaProt)

plot(int$Shape)

# Perif
int <- st_intersection(b.per,Prot)
int$areaProt <- st_area(int) 
# out put the values as table 
tb_Prot_by_range <- int %>% group_by(Range) %>% summarise(areaProt = sum(areaProt))
tb_Prot_by_range$areaProt <- as.numeric(tb_Prot_by_range$areaProt/10000)
p_out_Prot<- data.frame(tb_Prot_by_range)
p.Prot.total = sum(p_out_Prot$areaProt)

plot(int$Shape,col = "blue",add = T)

## CORE 
int <- st_intersection(b.core,Prot)
int$areaProt <- st_area(int) 
# out put the values as table 
tb_Prot_by_core <- int %>% group_by(CORE_NAME) %>% summarise(areaProt = sum(areaProt))
tb_Prot_by_core$areaProt <- as.numeric(tb_Prot_by_core$areaProt/10000)
out_Prot<- data.frame(tb_Prot_by_core)
c.Prot.total = sum(out_Prot$areaProt)

#Total %
total.pc.Prot.r = r.Prot.total/boreal_t_range * 100
total.pc.Prot.c = c.Prot.total / boreal_t_core * 100
total.pc.Prot.p = p.Prot.total / boreal_t_per * 100

######################################################################
# WHA
#####################################################################
## RANge 
int <- st_intersection(b.range,WHA)
int$areaWHA <- st_area(int) 
# out put the values as table 
tb_WHA_by_range <- int %>% group_by(Range) %>% summarise(areaWHA = sum(areaWHA))
tb_WHA_by_range$areaWHA <- as.numeric(tb_WHA_by_range$areaWHA/10000)
r_out_WHA<- data.frame(tb_WHA_by_range)
r.WHA.total = sum(r_out_WHA$areaWHA)

# perif 
int <- st_intersection(b.per,WHA)
int$areaWHA <- st_area(int) 
# out put the values as table 
tb_WHA_by_range <- int %>% group_by(Range) %>% summarise(areaWHA = sum(areaWHA))
tb_WHA_by_range$areaWHA <- as.numeric(tb_WHA_by_range$areaWHA/10000)
p_out_WHA<- data.frame(tb_WHA_by_range)
p.WHA.total = sum(p_out_WHA$areaWHA)

# Core 
int <- st_intersection(b.core,WHA)
int$areaWHA <- st_area(int) 
# out put the values as table 
tb_WHA_by_core <- int %>% group_by(CORE_NAME) %>% summarise(areaWHA = sum(areaWHA))
tb_WHA_by_core$areaWHA <- as.numeric(tb_WHA_by_core$areaWHA/10000)
out_WHA<- data.frame(tb_WHA_by_core)
c.WHA.total = sum(out_WHA$areaWHA)

#Total % 
total.pc.WHA.r = r.WHA.total  / boreal_t_core * 100
total.pc.WHA = c.WHA.total / boreal_t_core * 100
total.pc.WHA.p = p.WHA.total / boreal_t_per * 100

######################################################################
# OGMA
#####################################################################
## RANge 
int <- st_intersection(b.range,OGMA)
int$areaOGMA <- st_area(int) 
plot(int$Shape,add = TRUE,col = "red")
# out put the values as table 
tb_OGMA_by_range <- int %>% group_by(Range) %>% summarise(areaOGMA = sum(areaOGMA))
tb_OGMA_by_range$areaOGMA <- as.numeric(tb_OGMA_by_range$areaOGMA/10000)
r_out_OGMA<- data.frame(tb_OGMA_by_range)
r.OGMA.total = sum(r_out_OGMA$areaOGMA)

## perif 
int <- st_intersection(b.per,OGMA)
int$areaOGMA <- st_area(int) 
## out put the values as table 
tb_OGMA_by_range <- int %>% group_by(Range) %>% summarise(areaOGMA = sum(areaOGMA))
tb_OGMA_by_range$areaOGMA <- as.numeric(tb_OGMA_by_range$areaOGMA/10000)
p_out_OGMA<- data.frame(tb_OGMA_by_range)
p.OGMA.total = sum(p_out_OGMA$areaOGMA)

## Core 
int <- st_intersection(b.core,OGMA)
int$areaOGMA <- st_area(int) 
## out put the values as table 
tb_OGMA_by_core <- int %>% group_by(CORE_NAME) %>% summarise(areaOGMA = sum(areaOGMA))
tb_OGMA_by_core$areaOGMA <- as.numeric(tb_OGMA_by_core$areaOGMA/10000)
out_OGMA<- data.frame(tb_OGMA_by_core)
c.OGMA.total = sum(out_OGMA$areaOGMA)

#Total % 
total.pc.OGMA.r = r.OGMA.total  / boreal_t_core * 100
total.pc.OGMA = c.OGMA.total / boreal_t_core * 100
total.pc.OGMA.p = p.OGMA.total / boreal_t_per * 100

#####################################################################

#####################################################################
## compile data to export: RANGE
Range_key <- Herd_key %>% 
  group_by(Range) %>% 
  summarise(R_area_ha = first(R_area_ha))

out <- left_join(Range_key,r_out_WHA,by = "Range")
out <- left_join(out,r_out_NP,by = "Range")
out <- left_join(out,r_out_uwr, by = "Range") 
out <- left_join(out,r_out_Prot, by = "Range") 
out <- left_join(out,r_out_OGMA, by = "Range") 

out1 <- out%>%
  dplyr::select(-c(starts_with("Shape")))

out1[is.na(out1)] <- 0

# calculate the percentage value for each core boundary
range.out <-out1 %>% 
  mutate(WHA_pc = round((areaWHA/R_area_ha)*100,1)) %>% 
  mutate(NP_pc = round((areaNP/R_area_ha)*100,1)) %>% 
  mutate(UWR_pc = round((areaUWR/R_area_ha)*100,1)) %>% 
  mutate(Prot_pc = round((areaProt/R_area_ha)*100,1))%>%
  mutate(OGMA_pc = round((areaOGMA/R_area_ha)*100,1))

#write.csv(range.out,"4.1_Boreal_range_protect_caribou_herd.csv")

######################################
## compile data to export: PERIFERY
Range_key <- Herd_key %>% 
  group_by(Range) %>% 
  summarise(P_area_ha = first(P_area_ha))

out <- left_join(Range_key,p_out_WHA,by = "Range")
out <- left_join(out,p_out_NP,by = "Range")
out <- left_join(out,p_out_uwr, by = "Range") 
out <- left_join(out,p_out_Prot, by = "Range") 
out <- left_join(out,p_out_OGMA, by = "Range") 

out1 <- out%>%
  dplyr::select(-c(starts_with("Shape")))

out1[is.na(out1)] <- 0

# calculate the percentage value for each core boundary
per.out <-out1 %>% 
  mutate(WHA_pc = round((areaWHA/P_area_ha)*100,1)) %>% 
  mutate(NP_pc = round((areaNP/P_area_ha)*100,1)) %>% 
  mutate(UWR_pc = round((areaUWR/P_area_ha)*100,1)) %>% 
  mutate(Prot_pc = round((areaProt/P_area_ha)*100,1))%>%
  mutate(OGMA_pc = round((areaOGMA/P_area_ha)*100,1))

#write.csv(range.out,"4.1_Boreal_range_protect_caribou_herd.csv")

#############################################################
## compile data to export: CORE
out <- left_join(Herd_key,out_WHA,by = "CORE_NAME")
out <- left_join(out,out_NP,by = "CORE_NAME")
out <- left_join(out,out_uwr, by = "CORE_NAME") 
out <- left_join(out,out_Prot, by = "CORE_NAME") 
out <- left_join(out,out_OGMA, by = "CORE_NAME") 

out2 <- out%>%
  dplyr::select(-c(starts_with("Shape")))

out2[is.na(out2)] <- 0

# calculate the percentage value for each core boundary
core.out<-out2 %>% 
  mutate(WHA_pc = round((areaWHA/C_area_ha)*100,1)) %>% 
  mutate(NP_pc = round((areaNP/C_area_ha)*100,1)) %>% 
  mutate(UWR_pc = round((areaUWR/C_area_ha)*100,1)) %>% 
  mutate(Prot_pc = round((areaProt/C_area_ha)*100,1))%>%
  mutate(OGMA_pc = round((areaOGMA/C_area_ha)*100,1))

#write.csv(out2,"4.2_Boreal_core_protect_caribou_herd.csv")

range.out 
per.out
core.out

#########################################
## FORMAT TO DETAILED OUTPUTS FOR REPORT 

#1) calculate the total core area combined
core.total.out <- core.out %>%
  group_by((Range))%>% 
  summarise(C_total_area_H = sum(C_area_ha),areaWHA= sum(areaWHA), areaNP= sum(areaNP), areaUWR= sum(areaUWR),areaProt= sum(areaProt),areaOGMA=sum(areaOGMA))%>%
  mutate(WHA_pc = round((areaWHA/C_total_area_H)*100,1)) %>% 
  mutate(NP_pc = round((areaNP/C_total_area_H)*100,1)) %>% 
  mutate(UWR_pc = round((areaUWR/C_total_area_H)*100,1)) %>% 
  mutate(Prot_pc = round((areaProt/C_total_area_H)*100,1))%>%
  mutate(OGMA_pc = round((areaOGMA/C_total_area_H)*100,1))

# 2) create a table per herd. 
herds = as.factor(unique(Herd_key$Range))
for (i in herds) { 
  #i = herds[1]
  rang = range.out %>%  filter(grepl(i,Range))
  range.total = rang$R_area_ha
  rang = rang %>% dplyr::select(-(R_area_ha))
  rang = rang %>% gather(key = var_name, value = value, 2:ncol(rang)) %>% spread_(key = names(rang)[1],value = 'value')
  colnames(rang)= c("Protection.Type","Entire.Range")
  
  per = per.out %>%  filter(grepl(i,Range))
  per.total = per$P_area_ha
  per = per %>% dplyr::select(-(P_area_ha))
  per = per %>% gather(key = var_name, value = value, 2:ncol(per)) %>% spread_(key = names(per)[1],value = 'value')
  colnames(per)= c("Protection.Type","Periphery.Range")
  
  core = core.total.out %>% filter(grepl(i,`(Range)`))
  core.total = core$C_total_area_H
  core = core %>% dplyr::select(-(C_total_area_H))
  core = core %>% gather(key = var_name, value = value, 2:ncol(core)) %>% spread_(key = names(core)[1],value = 'value')
  colnames(core)= c("Protection.Type","Core.Range")
  
  file.out <- left_join(rang,per)
  file.out <- left_join(file.out,core)
  file.out = file.out%>% rbind(c("total",range.total,per.total,core.total))
  
  file.out$Type =   c("National Park (ha)","Old Growth Management Area (ha)", "Parks and Protected Areas (ha)","Ungulate Winter Range (ha)",
                      "Wildlife Habitat Area (ha)", "National Park (%)","Old Growth Management Area (%)", "Parks and Protected Areas (%)","Ungulate Winter Range (%)",
                      "Wildlife Habitat Area (%)","Total (ha)")
  file.out = file.out%>% dplyr::select(-Protection.Type)
  file.out = file.out[,c('Type', 'Entire.Range','Periphery.Range','Core.Range')]
  file.out = rbind(file.out[11,],file.out[1:10,])
  file.out = file.out %>% mutate(Entire.Range = as.numeric(Entire.Range), Periphery.Range = as.numeric(Periphery.Range),Core.Range = as.numeric(Core.Range))
  file.out = file.out %>% mutate_at(2:4, round, 1) 
  
  write.csv(file.out,paste(out.dir,"Protection_",i,".csv",sep = ''))
  print(i)} 
  
##########################################################################

# Output Core detailed Table. 
core.out

herds = as.factor(unique(Herd_key$Range))
for (i in herds) { 
  #i = herds[1]
  rang =core.out %>%  filter(grepl(i,Range))
  range.total = round(rang$C_area_ha,1)
  rang = rang %>% dplyr::select(-c(Range,P_area_ha,C_area_ha, R_area_ha))
  rang = rang %>% gather(key = var_name, value = value, 2:ncol(rang)) %>% spread_(key = names(rang)[1],value = 'value')
  file.out = rang
  file.out = file.out%>% rbind(c("total",range.total))
  
  file.out$Type =   c("National Park (ha)","Old Growth Management Area (ha)", "Parks and Protected Areas (ha)","Ungulate Winter Range (ha)",
                      "Wildlife Habitat Area (ha)", "National Park (%)","Old Growth Management Area (%)", "Parks and Protected Areas (%)","Ungulate Winter Range (%)",
                      "Wildlife Habitat Area (%)","Total (ha)")
  file.out = file.out%>% dplyr::select(-var_name)
  #file.out = file.out[,c('Type')]
  file.out = rbind(file.out[11,],file.out[1:10,])
  #file.out = file.out %>% mutate(Entire.Range = as.numeric(Entire.Range), Periphery.Range = as.numeric(Periphery.Range),Core.Range = as.numeric(Core.Range))
  
  write.csv(file.out,paste(out.dir,"Protection_by_core_",i,".csv",sep = ''))
  print(i)} 




#####################################################################














## OLD CODE ## DONT RUN





################################################################
# Intersect with  Core Area: 
c.UWR = intersect(b.core,UWR);     plot(c.UWR,add= T, col = 'Red')
# dissolve 
c.UWR.dis <- aggregate(c.UWR, by ="CORE_NAME",dissolve = FALSE) ; plot(c.UWR.dis,add = T,col = "blue")
out = data.frame(c.UWR.dis)
sf::st_area(c.UWR.dis)

######################################################################
## Calculate the protected areas for Range level 
######################################################################
r.protect <-  readOGR(dsn=Intersect,layer="R_protect_intersect_Dissolve")
pro.df = data.frame(r.protect)
unique(r.protect$desig)
# remove VQO layers 

## extract at at level of range areas
out.r.ha <- pro.df  %>%
  #dplyr::filter(!grepl(pattern="vqo", desig)) %>% 
  group_by(Range,desig)%>%
  summarise(sum = round(sum(Shape_Area/10000),0))%>%
  spread(Range,sum) 
totals <-out.r.ha
out.r.ha [is.na(out.r.ha )] <- 0

out.r.pc1 <- out.r.ha %>% dplyr::select(Calendar) %>% mutate(Calendar_PC = round((Calendar/541108*100),1))
out.r.pc2 <- out.r.ha %>% dplyr::select(Chinchaga) %>% mutate(Chinchaga_PC = round((Chinchaga/1390336*100),1)) 
out.r.pc3 <- out.r.ha %>% dplyr::select(Maxhamish) %>% mutate(Maxhamish_PC = round((Maxhamish/815326*100),1)) 
out.r.pc4 <- out.r.ha %>% dplyr::select(`Snake-Sahtahneh`)%>% mutate(Snake.Sahtahneh_PC = round((`Snake-Sahtahneh`/1230117*100),1)) 
out.r.pc5 <- out.r.ha %>% dplyr::select(`Westside Fort Nelson`)%>% mutate(Westside.Fort.Nelson_PC = round((`Westside Fort Nelson`/739961*100),1)) 

out.r.ha = cbind(out.r.ha,out.r.pc1,out.r.pc2,out.r.pc3,out.r.pc4,out.r.pc5)
out.r.ha [is.na(out.r.ha )] <- 0
write.csv(out.r.ha,"Boreal_range_Protect_detail.csv")

# calculate totals and percentages for combined Boreal herds 
totals [is.na(totals )] <- 0

t1 <- totals %>%
    mutate(sum = dplyr::select(., Calendar:`Westside Fort Nelson`) %>% 
    apply(1, sum, na.rm=TRUE)) %>% 
    summarise(sum_pro = sum(sum))
# get a total measure of protection for all range area: 
t1/boreal_t_range * 100

######################################################################
## Calculate the protected areas for CORE level 
######################################################################

r.protect <-  readOGR(dsn=Intersect,layer="C_protect_Dis")
pro.df = data.frame(r.protect)
#unique(r.protect$desig); plot(r.protect,add = T, col = "yellow")

pro.df1 = left_join(pro.df,Herd_key)

## extract at at level of range areas
out.r.ha <- pro.df1  %>%
  #dplyr::filter(!grepl(pattern="vqo", desig)) %>% 
  group_by(Range,desig)%>%
  summarise(sum = round(sum(Shape_Area/10000),0)) %>%
  spread(Range,sum) 

totals <-out.r.ha

out.r.pc1 <- out.r.ha %>% dplyr::select(Calendar) %>% mutate(Calendar_PC = round((Calendar/430933*100),1))
out.r.pc2 <- out.r.ha %>% dplyr::select(Chinchaga) %>% mutate(Chinchaga_PC = round((Chinchaga/859180*100),1)) 
out.r.pc3 <- out.r.ha %>% dplyr::select(Maxhamish) %>% mutate(Maxhamish_PC = round((Maxhamish/447949*100),1)) 
out.r.pc4 <- out.r.ha %>% dplyr::select(`Snake-Sahtahneh`)%>% mutate(Snake.Sahtahneh_PC = round((`Snake-Sahtahneh`/567403*100),1)) 
out.r.pc5 <- out.r.ha %>% dplyr::select(`Westside Fort Nelson`)%>% mutate(Westside.Fort.Nelson_PC = round((`Westside Fort Nelson`/269385*100),1)) 
out.r.ha = cbind(out.r.ha,out.r.pc1,out.r.pc2,out.r.pc3,out.r.pc4,out.r.pc5)
out.r.ha [is.na(out.r.ha )] <- 0

write.csv(out.r.ha,"Boreal_core_Protect_detail.csv")



