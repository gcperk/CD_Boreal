
## SCript to aggregate all the outputs of script 1 and 2. 
## This script will generate the "total disturbance % or ha values" 


#install.packages(c("rgdal","sp","dplyr","raster","rgeos","maptools","magrittr","tibble", 
#				"tidyr","sf","lwgeom","mapview"),dep = T )
library(ggplot2)
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
library(lwgeom)
library(mapview)


## set your output directory 
out.dir = "X:/projects/Desktop_Analysis/data/output1/"
temp.dir = "X:/projects/Desktop_Analysis/data/temp/"



##########################################################################################
# Read in data tables 
# Table 1 : all disturbance (except roads ands seismic)
# table 2 : natural disturbance ( burns and pests)
# table XX : roads 
# tabel xx : seismic
# Table 6 : Protection at the Range and the core levels

Herd_key <- read.csv(paste(out.dir,"Herd_key.csv",sep = ""))                # import Key 
dis1 <- read.csv(paste(temp.dir,"Disturb_calcs_EX_rd_seis.csv",sep = ""))   # import distubance per layer
dis1 = dis1 %>% dplyr::select(-c(X))
# read in road layers 
dis3 <- read.csv(paste(temp.dir,"Max_temp_cals.csv",sep = ''))              # import road distubance
dis4 <- read.csv(paste(temp.dir,"Snake_temp_cals.csv",sep = ''))
dis5 <- read.csv(paste(temp.dir,"Chin_temp_cals.csv",sep = ''))
dis6 <- read.csv(paste(temp.dir,"West_temp_cals.csv",sep = ''))
dis7 <- read.csv(paste(temp.dir,"Cale_temp_cals.csv",sep = ''))

dis3 <- dis3 %>% 
          mutate(R_Road_m2 = R_Road_Max_m2, C_Road_m2 = C_Road_Max_m2, P_Road_m2 = P_Road_Max_m2) %>%
          dplyr::select(-c(R_Road_Max_m2,C_Road_Max_m2,P_Road_Max_m2, R_Road_Max_length_m,C_Road_Max_length_m))
dis4 <- dis4 %>% 
  mutate(R_Road_m2 = R_Road_Snake_m2, C_Road_m2 = C_Road_Snake_m2, P_Road_m2 = P_Road_Snake_m2) %>%
  dplyr::select(-c(R_Road_Snake_m2,C_Road_Snake_m2,P_Road_Snake_m2))
dis5 <- dis5 %>% 
  mutate(R_Road_m2 = R_Road_Chin_m2, C_Road_m2 = C_Road_Chin_m2, P_Road_m2 = P_Road_Chin_m2) %>%
  dplyr::select(-c(R_Road_Chin_m2,C_Road_Chin_m2,P_Road_Chin_m2))

dis6 <- dis6 %>% 
  mutate(R_Road_m2 = R_Road_West_m2, C_Road_m2 = C_Road_West_m2, P_Road_m2 = P_Road_West_m2) %>%
  dplyr::select(-c(R_Road_West_m2,C_Road_West_m2,P_Road_West_m2))
dis7 <- dis7 %>% 
  mutate(R_Road_m2 = R_Road_Cale_m2, C_Road_m2 = C_Road_Cale_m2, P_Road_m2 = P_Road_Cale_m2) %>%
  dplyr::select(-c(R_Road_Cale_m2,C_Road_Cale_m2,P_Road_Cale_m2))

roads = rbind(dis3,dis4,dis5,dis6,dis7)
roads = roads %>% dplyr::select(-c(X))


dis8 <- read.csv(paste(temp.dir,"Seismic_dist_all_herds.csv",sep = ''))
dis8 = dis8 %>% dplyr::select(-c(X))

# join all the disturbances together 
tout = left_join(Herd_key,roads, by = "Range")
tout = left_join(tout,dis1, by = "Range")
tout = left_join(tout,dis8, by = "Range")


# import natural disturbance
nat1 <- read.csv(paste(out.dir,"Nat_dist_type_RPC.csv",sep =""))  ; nat1 = nat1 %>% dplyr::select(-c(X))
nat2 <- read.csv(paste(out.dir,"Combines_Nat_dist_RPC.csv",sep ="")) 
nat2 = nat2 %>% dplyr::select(-c(X))

tout = left_join(tout,nat1 , by = "Range")
tout = left_join(tout,nat2 , by = "Range")
head(tout)


#######################################

# Detailed tables 
# import the more detailed tabels: 
dis10 <- read.csv(paste(out.dir,"Pest_by_type_RPC.csv",sep ="")) # pest types break down 


########################################

# Loop through each herd and create a full table 
herds = c('Calendar','Chinchaga','Maxhamish','Snake-Sahtahneh','Westside Fort Nelson')

for (i in herds) { 
  #i = herds[2]
  
  ################ All Disturbance ########################
  r.temp <- tout %>% filter(Range == paste(i))
  r.temp = reshape2:::melt(r.temp)
  r.temp = r.temp %>% mutate(Boundary = substr(variable,1,1)) 
  r.temp = r.temp %>% mutate(Boundary = substr(variable,1,1)) 
  r.temp  <- r.temp  %>% mutate(Dist_type = gsub(".*R_|_m2.*","", variable)) 
  r.temp  <- r.temp  %>%  mutate(Dist_type = gsub("C_","",Dist_type))
  r.temp <- r.temp   %>%  mutate(Dist_type = gsub("P_","",Dist_type))
  r.temp <- r.temp   %>%  mutate(Dist_type = gsub("_m","",Dist_type))
  r.temp <- r.temp   %>%  mutate(Dist_type = gsub("_area","",Dist_type))
  
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
    dplyr::select(-c(variable, value)) 
  # get rid of the area_total rows 
  r.temp=r.temp[-(grep("area_ha",r.temp$Dist_type)),]
  r.temp = r.temp[-(grep("X",r.temp$Dist_type)),]

  ############################################################
  ### Add the detailed data of ##############
  
  p.temp = dis10 %>% filter(Range == paste(i))
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
            r.temp = rbind(r.temp,p.temp)
                            } 

  ########################################
  # import temporal data sets: 
  dis9 <- read.csv(paste(out.dir,"Temporal_decade_dist_RPC.csv",sep ="")) 
            c.temp = dis9 %>% filter(Range == paste(i))
            c.temp = c.temp %>% dplyr::select(-c(X))
            c.temp = c.temp %>% filter(dec.period > 0) 
          
            
            # if(length(c.temp$Range)>0){ 
            c.temp = reshape2:::melt(c.temp)
            c.temp = c.temp %>% mutate(Boundary = substr(variable,1,1)) 
            c.temp = c.temp %>% mutate(Boundary = substr(variable,1,1)) 
            c.temp  <- c.temp  %>% mutate(Dist_type = gsub(".*R_|_m2.*","", variable)) 
            c.temp  <- c.temp  %>%  mutate(Dist_type = gsub("C_","",Dist_type))
            c.temp <- c.temp   %>%  mutate(Dist_type = gsub("P_","",Dist_type))
            
            # add columns to the totals for RPC  
            c.temp <- c.temp %>% mutate(Total_area_ha = ifelse(Boundary =='R',t.r,
                                                               ifelse(Boundary =='P',t.p,
                                                                      ifelse(Boundary == 'C',t.c,0))))
            
            # calculate ha and Percentage of area per disturbance type. {Herd 2]}
            c.temp <-c.temp  %>% 
              mutate(Area_ha = value/10000) %>% 
              mutate(Percentage = (Area_ha/ as.numeric(Total_area_ha))*100) %>%
              mutate(dec.period = ifelse(Range == "Maxhamish",rep(c(1958,1968,1988,1998,2008),9),rep(c(1958,1968,1978,1988,1998,2008),9))) %>%
              mutate(Dist_type = paste(Dist_type,":",dec.period)) %>%
              dplyr::select(-c(variable, value,dec.period)) 
            
            # add to the other table
            r.temp = rbind(r.temp,c.temp)
  
  # } 

  #################
  ## write out tables of area and Percentage 
  #################
  
  # output table of area
  r.area <- r.temp %>% dplyr::select(Boundary,Dist_type,Area_ha) %>% 
    spread(key = Boundary,value = Area_ha)
  r.area = r.area[c("Dist_type","R","P","C")]
  r.area = r.area %>%
    mutate_if(is.numeric,round,digits = 1) 
  r.area [is.na(r.area )]<- 0 
  
  
  r.pc <- r.temp %>% dplyr::select(Boundary,Dist_type,Percentage) %>% 
    spread(key = Boundary,value = Percentage)
  r.pc = r.pc[c("Dist_type","R","P","C")]
  r.pc = r.pc%>%
    mutate_if(is.numeric,round,digits = 1)
  r.pc [is.na(r.pc)]<- 0 
  
  # write out results
  write.csv(r.area,paste(out.dir,i,"_Dist_Summary_ha.csv",sep = ''))
  write.csv(r.pc,paste(out.dir,i,"_Dist_Summary_pc.csv",sep = ''))
  
} 
  
