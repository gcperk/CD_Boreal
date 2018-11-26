## 
## Caribou disturbance analysis 2018 
## 
## August 14th 2018 - updated November 5th 2018 
##
## written by genevieve perkins (genevieve.perkins@gov.bc.ca)
## 
## This requires initial preparation of layers within arcmap. 
## Step 1) 
## Assemble layers as per datasheet in arcmap mxd 
## Clip the layers to the range with the largest extent (for example range boundary for Boreal (not core))
## Create a filegeodatabdase and output these to the given data base. 
## Edit this script for whichever 

## Step 2)
## Run through the script below. You may need to adjust 
## - the names of files to match your arcmap exports 
## - the directory/folder sructure. 
##
## General notes: 
## For each disturbance layers the script will read in, intersect with range and core areas and calculate the area and or length. The peripery area will be calculated for each herd as well. 
## With each layer the compiled disturbance will also be calculated. 

## Associated help files for reference: 
##https://gis.stackexchange.com/questions/265863/how-to-read-a-feature-class-in-an-esri-personal-geodatabase-using-r
##http://rstudio-pubs-static.s3.amazonaws.com/255550_1c983c8a6a5a45a0aee5a923206f4818.html
#http://www.rspatial.org/spatial/rst/7-vectmanip.html#spatial-queries
#https://r-spatial.github.io/sf/articles/sf1.html
#https://github.com/r-spatial/sf/issues/290

## Read in packages and libraries required: 

#install.packages(c("rgdal","sp","dplyr","raster","rgeos","maptools","magrittr","tibble", 
#			"tidyr","sf","lwgeom","mapview"),dep = T )
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
#out.dir = "X:/projects/Desktop_Analysis/data/output1/"

out.dir = "X:/projects/Desktop_Analysis/documents/NxNW/"
temp.dir = "X:/projects/Desktop_Analysis/data/temp/"

## Set your input geodatabases (this will be where you saved your arcmap exports)
## edit these to your filepath and name of gdb

#Dissolved  = "X:/projects/Desktop_Analysis/data/Boreal.gdb" # contains 
#Intersect = "X:/projects/Desktop_Analysis/data/scratch1.gdb"
Base  = "X:/projects/Desktop_Analysis/data/Base_data.gdb" # contains boundaries of interest 

## List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
#dis_list <- ogrListLayers(Dissolved); print(dis_list)
int_list <- ogrListLayers(Intersect); print(int_list)
base_list <- ogrListLayers(Base); print(base_list)

##############################################################################################
# Read in herd boundary layers 

b.core <- st_read(dsn=Base,layer="Boreal_core_BC")
b.range <- st_read(dsn=Base,layer="Boreal_range_BC")
b.core.r <- st_intersection(b.range,b.core)

# calculate the area of core/range/peripery to calculate % values 

Herd_key<- data.frame(b.core.r) %>% 
  dplyr::select(Range,CORE_NAME,Shape_Area_m,Shape_Area.1) %>% 
  mutate(R_area_ha = Shape_Area_m/10000) %>% mutate(C_area_ha = Shape_Area.1/10000) %>%
  dplyr::select(-c(Shape_Area_m,Shape_Area.1)) %>%
  group_by(Range)%>%
  summarise(R_area_ha = first(R_area_ha),C_area_ha = sum(C_area_ha))
    
  Herd_key$P_area_ha <- Herd_key$R_area_ha - Herd_key$C_area_ha # add the values for the periperhy 
  write.csv(Herd_key, paste(out.dir,"Herd_key.csv",sep = ""))
  
##############################################################################################
# Read in individual Disturbance layers:

# 1) pipeline (length)
r.pipe <- st_read(dsn=Intersect,layer="R_PIPELINE_SEGMENT_PERMIT_S") # multistring 
          r.pipe.int = st_intersection(b.range,r.pipe)   # intersect with ranges
          # RANGE: intersect with range and calculate length per range
          r.pipe.int <- st_buffer(r.pipe.int,1) # ; all.pipe = sum(st_area(r.pipe)) ; plot(st_geometry(r.pipe))
          #st_is_valid(r.pipe.int)                       # check valid geometry
          r.pipe.int = st_cast(r.pipe.int,"POLYGON")     # fix overlaps
          r.pipe.int$Area.m <- st_area(r.pipe.int)
          ##plot(st_geometry(r.pipe.int))
          #st_write(r.pipe.int,"Dist_R_pipe.shp")       #write out individual dist_layer for Range
          r.pipe.int.df = data.frame(r.pipe.int)        # calcaulte area
          r.pipe.int.df.out  = r.pipe.int.df %>% 
                  mutate(Pipe_area_m = Area.m) %>% 
                  group_by(Range) %>% 
                  summarise(R_Pipe_area_m = sum(Pipe_area_m))
      
          # CORE: intersect with core and calculate length per range
          c.pipe.int = st_intersection(b.core,r.pipe) # intersect with ranges
          c.pipe.int <- st_buffer(c.pipe.int,1)
          c.pipe.int = st_cast(c.pipe.int,"POLYGON")
          c.pipe.int$area_m = st_area(c.pipe.int)
          #st_is_valid(c.pipe.int)
          ##plot(st_geometry(c.pipe.int))
          #st_write(r.pipe.int,"Dist_C_pipe.shp") 
          c.pipe.int.df = data.frame(c.pipe.int)        # calculate the length per core/range 
          c.pipe.int.df.out  = c.pipe.int.df %>% 
            mutate(Pipe_area_m = area_m) %>% 
            group_by(Range) %>% 
            summarise(C_Pipe_area_m = sum(Pipe_area_m))
          
          out = left_join(r.pipe.int.df.out,c.pipe.int.df.out, all = both)
          out [is.na(out )] <- 0
          out$P_Pipe_area_m = out$R_Pipe_area_m - out$C_Pipe_area_m
          out.pipe = out
               
# 2) transmission (length and area) 
r.tran.sf <- st_read(dsn=Intersect,layer="BC_trans") # multistring  
          
          # 1) RANGE: calculate the range extent to use range extent 
          r.tran <- st_intersection(b.range,r.tran.sf)# intersect with ranges
          r.tran <- st_buffer(r.tran,1) #;all.tran = sum(st_area(r.tran)) 
          r.tran <- st_cast(r.tran,"POLYGON")
          r.tran$area_m = st_area(r.tran)
          #st_is_valid(r.tran)                   # check valid geometr
          r.tran.df = data.frame(r.tran)        # calculate the length per range 
          r.tran.df.out  = r.tran.df%>% 
            mutate(Trans_area_m = area_m) %>% 
            group_by(Range) %>% 
            summarise(R_Trans_area_m = sum(Trans_area_m))
        
          ##plot(st_geometry(r.tran))
          #st_write(r.tran,"Dist_R_tran.shp")       #write out individual dist_layer for Range

          # 2) CORE; intersect with core and calculate length per range 
          c.tran.int = st_intersection(b.core.r,r.tran.sf) # intersect with ranges
          c.tran.int$area_m = st_area(c.tran.int)
          c.tran.int <- st_buffer(c.tran.int,1) #;all.tran = sum(st_area(r.tran)) 
          #st_is_valid(c.pipe.int)  
          c.tran.int = st_cast(c.tran.int,"POLYGON")
          ##plot(st_geometry(c.pipe.int))
          #st_write(c.trans.int,"Dist_C_trans.shp") 
          c.tran.int.df = data.frame(c.tran.int)        # calculate the length per core/range 
          c.tran.int.df.out  = c.tran.int.df %>% 
            mutate(Trans_area_m = area_m) %>% 
            group_by(Range) %>% 
            summarise(C_Trans_area_m = sum(Trans_area_m))
        
          out.trans = left_join(r.tran.df.out,c.tran.int.df.out, all = both)
          out.trans[is.na(out.trans)] <- 0
          out.trans$P_Trans_area_m = out.trans$R_Trans_area_m - out.trans$C_Trans_area_m
    
          range.layer.cals <- left_join(out.pipe,out.trans) ; range.layer.cals [is.na(range.layer.cals )] <- 0
          
          ## 3) ALL DISTURBANCE UNION 1
          out = st_union(r.pipe.int,r.tran) ; plot(st_geometry(out))
          out = st_union(out); plot(st_geometry(out),add = T)
          out = st_cast(out,"POLYGON")
          ##x.area = sum(st_area(out)) 
          
# 3) mine
r.mine.sf <- st_read(dsn=Intersect,layer="R_Mining_Intersect") # multipoly
r.mine.sf <- st_intersection(b.range,r.mine.sf)
#all.mine = sum(st_area(r.mine.sf)); plot(st_geometry(r.mine.sf))

          # 1) RANGE: calculate the range extent to use range extent 
          r.mine <- st_intersection(b.range,r.mine.sf)# intersect with ranges
          r.mine <- st_cast(r.mine,"POLYGON")
          #st_is_valid(r.mine)                   # check valid geometr
          r.mine$Area.m <- st_area(r.mine)
          r.mine.df = data.frame(r.mine)        # calculate the length per range 
          r.mine.df.out  = r.mine.df %>% 
            group_by(Range) %>% 
            summarise(R_mine_m2 = sum(Area.m))

          # 2) CORE: calculate the range extent to use range extent 
          c.mine <- st_intersection(b.core.r,r.mine.sf)# intersect with ranges
          c.mine <- st_cast(c.mine,"POLYGON")
          #st_is_valid(c.mine)                   # check valid geometr
          c.mine$Area.m <- st_area(c.mine)
          c.mine.df = data.frame(c.mine)        # calculate the length per range 
          c.mine.df.out  = c.mine.df %>% 
            group_by(Range) %>% 
            summarise(C_mine_m2 = sum(Area.m))
          # combine into mine table: 
          out.mine = left_join(r.mine.df.out,c.mine.df.out, all = both)
          out.mine$P_mine_m2 = out.mine$R_mine_m2 - out.mine$C_mine_m2
          # combine into disturbance by layer 
          range.layer.cals <- left_join(out.mine,range.layer.cals) ; range.layer.cals [is.na(range.layer.cals )] <- 0

        ## 3) ALL DISTURBANCE UNION 2
          out1 = st_union(out,r.mine.sf) ; plot(st_geometry(out1))
          out1 = st_union(out1); plot(st_geometry(out1))
          #x.area = sum(st_area(out1)) 
          out1 = st_cast(out1,"POLYGON")

# 4) agriculture 
r.agr.sf <- st_read(dsn=Intersect,layer="R_agr_Intersect") # multipoly
#all.agg = sum(st_area(r.agr.sf)); plot(st_geometry(r.agr.sf))

        # 1) RANGE: calculate the range extent to use range extent 
        r.agr <- st_intersection(b.range,r.agr.sf)# intersect with ranges
        r.agr <- st_cast( r.agr,"POLYGON")
        #st_is_valid(r.agr)                   # check valid geometr
        r.agr$Area.m <- st_area(r.agr)
        r.agr.df = data.frame(r.agr)        # calculate the length per range 
        r.agr.df.out  = r.agr.df %>% 
          group_by(Range) %>% 
          summarise(R_agr_m2 = sum(Area.m))
        #plot(st_geometry(r.agr))
        
        # 2) CORE: calculate the range extent to use range extent 
        c.agr <- st_intersection(b.core.r,r.agr.sf)# intersect with core #plot(st_geometry(c.agr),add = T, col = "red") 
        c.agr <- st_cast(c.agr,"POLYGON")
        #st_is_valid(c.agr)                   # check valid geometr
        c.agr$Area.m <- st_area(c.agr)
        c.agr.df = data.frame(c.agr)        # calculate the length per range 
        c.agr.df.out  = c.agr.df %>% 
          group_by(Range) %>% 
          summarise(C_agr_m2 = sum(Area.m))
        
        ## combine into mine table: 
        out.agr = left_join(r.agr.df.out,c.agr.df.out, all = both)
        out.agr$P_agr_m2 = out.agr$R_agr_m2 - out.agr$C_agr_m2
        # combine into disturbance by layer 
        range.layer.cals <- left_join(range.layer.cals,out.agr) ; range.layer.cals [is.na(range.layer.cals )] <- 0
  
        ## ALL DISTURBANCE COMBINED: UNION 3
        out2 = st_union(out1,r.agr.sf) ; plot(st_geometry(out2))
        out2 = st_union(out2); plot(st_geometry(out2))
        #x.area = sum(st_area(out2)) #out1 = st_cast(out1,"POLYGON")
        
# 5) air strips
r.air.sf <- st_read(dsn=Intersect,layer="R_air_int") # multipoly
r.air.sf <- st_cast(r.air.sf,"POLYGON")
#air.agg = sum(st_area(r.air.sf)); plot(st_geometry(r.air.sf))        

          # 1) RANGE: calculate the range extent to use range extent 
          r.air <- st_intersection(b.range,r.air.sf)# intersect with ranges
          r.air <- st_cast( r.air,"POLYGON")
          #st_is_valid(r.air)                   # check valid geometr
          r.air$Area.m <- st_area(r.air)
          r.air.df = data.frame(r.air)        # calculate the length per range 
          r.air.df.out  = r.air.df %>% 
            group_by(Range) %>% 
            summarise(R_air_m2 = sum(Area.m))
          #plot(st_geometry(r.air))
          
          # 2) CORE: calculate the range extent to use range extent 
          c.air<- st_intersection(b.core.r,r.air.sf)# intersect with core #plot(st_geometry(c.agr),add = T, col = "red") 
          c.air <- st_cast(c.air,"POLYGON")
          c.air$Area.m <- st_area(c.air)
          c.air.df = data.frame(c.air)        # calculate the length per range 
          c.air.df.out  = c.air.df %>% 
            group_by(Range) %>% 
            summarise(C_air_m2 = sum(Area.m))
          
          ## combine into mine table: 
          out.air = left_join(r.air.df.out,c.air.df.out, all = both)
          out.air$P_air_m2 = out.air$R_air_m2 - out.air$C_air_m2
          # combine into disturbance by layer 
          range.layer.cals <- left_join(range.layer.cals,out.air) ; range.layer.cals [is.na(range.layer.cals )] <- 0
  
        ## UNION 4
        #out3 = st_union(out2,r.air.sf) #; plot(st_geometry(out3))
        #out3 = st_union(out3); plot(st_geometry(out3))
        #x.area = sum(st_area(out3)) 

# 6) dams 
r.dam.sf <- st_read(dsn=Intersect,layer="R_dams_inte" ) # multipoly
        ## RANGE: intersect with range and calculate length per range
        r.dams = st_intersection(b.range,r.dam.sf)   # intersect with ranges
        r.dams <- st_buffer(r.dams,1)
        r.dams <- st_cast(r.dams,"POLYGON")
        r.dams$area <- st_area(r.dams)
        ##plot(st_geometry(r.dams))
        #st_write(r.dams,"Dist_R_dams.shp")       #write out individual dist_layer for Range
        r.dams.df = data.frame(r.dams)        # calculate the length per range 
        r.dams.df.out  = r.dams.df %>% 
          mutate(Dam_area_m = area) %>% 
          group_by(Range) %>% 
          summarise(R_Dams_m2 = sum(area))
        
      # CORE: intersect with core and calculate length per range
        c.dams = st_intersection(b.core.r,r.dam.sf)   # intersect with ranges
        c.dams <- st_buffer(c.dams,1)
        c.dams <- st_cast(c.dams,"POLYGON")
        c.dams$area <- st_area(c.dams)
        ##plot(st_geometry(r.dams))
        #st_write(r.dams,"Dist_R_dams.shp")       #write out individual dist_layer for Range
        c.dams.df = data.frame(c.dams)        # calculate the length per range 
        c.dams.df.out  = c.dams.df %>%
          group_by(Range) %>% 
          summarise(C_Dams_m2 = sum(area))
        
      out.dams = left_join(r.dams.df.out,c.dams.df.out, all = both)
      out.dams[is.na(out.dams)] <- 0
      out.dams$P_Dams_m2 = out.dams$R_Dams_m2 - out.dams$C_Dams_m2

      range.layer.cals <- left_join(range.layer.cals,out.dams) ; range.layer.cals [is.na(range.layer.cals )] <- 0

        # # UNION 5
        r.dam.sf<- st_buffer(r.dam.sf,1) ;all.dam = sum(st_area(r.dam.sf )) ; plot(st_geometry(r.dam.sf))  
        out4 = st_union(out3,r.dam.sf) ; plot(st_geometry(out4))
        out4 = st_union(out4); plot(st_geometry(out4))
        #x.area = sum(st_area(out4))     
        
# 7) reservoirs # no reservoirs: 
#r.res.sf <- st_read(dsn=Dissolved,layer="R_reserv_dis") ;  plot(st_geometry(r.res.sf))         

        # This is Empty for Boreal         
        
## 8) Cutblock ## this is all years of consolidated cutblock layer 
# Split out age classes
b.r.c = st_read(dsn = Intersect , layer = "R_cutblock")
b.r.c$TimeSinceCut = 2018-b.r.c$HARVEST_YEAR; #plot(b.r.c$Shape); plot(b.core.sf$Shape,add = TRUE)
sort(unique(b.r.c$TimeSinceCut)) # note in the boreal the oldest age cut is 56 years 

# cutblocks 0-80 years
b.r.c0.80 = b.r.c[b.r.c$TimeSinceCut < 81,] ; unique(b.r.c0.80$TimeSinceCut); #plot(b.r.c0.80$Shape)
b.r.c0.80.0 = b.r.c0.80
b.r.c0.80 = st_cast(b.r.c0.80,"POLYGON"); #st_is_valid(b.r.c0.80)
#all.cut = sum(st_area(b.r.c0.80))
b.r.c0.80.u = st_union(b.r.c0.80)
       
        ## ALL DISTURBANCE UNION 5 # may need to run this in stand alone R rather than R -studio
        #out5 = st_union(b.r.c0.80.u,out4) ; plot(st_geometry(out5))
        #out5 = st_union(out5); plot(st_geometry(out5))
        ##x.area = sum(st_area(out5)) 

        ## RANGE: intersect with range and calculate length per range
        r.cut = st_intersection(b.range,b.r.c0.80.0)     # intersect with ranges
        r.cut <- st_cast(r.cut,"POLYGON")
        r.cut$area <- st_area(r.cut)
        ##plot(st_geometry(r.cut))
        #st_write(r.cut,"Dist_R_cutblock0.80.shp")       #write out individual dist_layer for Range
        r.cut.df = data.frame(r.cut)        # calculate the length per range 
        
        # add a column to differentiate the age brackets of cutblocks 
        r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR >= 1958 & HARVEST_YEAR <= 1967,1958,0))
        r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR >= 1968 & HARVEST_YEAR <= 1977,1968,dec.period))
        r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR >= 1978 & HARVEST_YEAR <= 1987,1978,dec.period))
        r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR >= 1988 & HARVEST_YEAR <= 1997,1988,dec.period))   
        r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR >= 1998 & HARVEST_YEAR <= 2007,1998,dec.period))   
        r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR >= 2008 & HARVEST_YEAR <= 2017,2008,dec.period)) 
        #r.cut.df[r.cut.df$dec.period == 0,]
        #unique(r.cut.df$dec.period)
           
        # output the amount of cutblock by range (all years (0-80))  
        r.cut.df.out  = r.cut.df %>% 
          group_by(Range) %>% 
          summarise(R_cut0_80_m2 = sum(area))
        
        # output the amount of cutblock by range (all years (0-40))  
        r.cut.df.out.0.40 <- r.cut.df %>%
          filter(dec.period >= 1978) %>% 
          group_by(Range) %>% 
          summarise(R_cut0_40_m2 = sum(area))
        
        #output the amount of cutblock per decade (all years) 
        r.cut.df.out.temp  = r.cut.df %>% 
          group_by(Range,dec.period) %>% 
          summarise(R_cut_dec_m2 = sum(area))
        
        # CORE: intersect with core and calculate length per range
        c.cut = st_intersection(b.core.r,b.r.c0.80.0)   # intersect with core
        c.cut <- st_cast(c.cut,"POLYGON")
        c.cut$area <- st_area(c.cut)
        
        ##plot(st_geometry(c.cut))
        #st_write(r.dams,"Dist_C_cutblock0.80.shp.shp")       #write out individual dist_layer for core
        c.cut.df = data.frame(c.cut)        # calculate the area per range 
        
        # set up the age class
        # add a column to differentiate the age brackets of cutblocks 
        c.cut.df <- mutate(c.cut.df,dec.period = ifelse(HARVEST_YEAR >= 1958 & HARVEST_YEAR <= 1967,1958,0))
        c.cut.df <- mutate(c.cut.df,dec.period = ifelse(HARVEST_YEAR >= 1968 & HARVEST_YEAR <= 1977,1968,dec.period))
        c.cut.df <- mutate(c.cut.df,dec.period = ifelse(HARVEST_YEAR >= 1978 & HARVEST_YEAR <= 1987,1978,dec.period))
        c.cut.df <- mutate(c.cut.df,dec.period = ifelse(HARVEST_YEAR >= 1988 & HARVEST_YEAR <= 1997,1988,dec.period))   
        c.cut.df <- mutate(c.cut.df,dec.period = ifelse(HARVEST_YEAR >= 1998 & HARVEST_YEAR <= 2007,1998,dec.period))   
        c.cut.df <- mutate(c.cut.df,dec.period = ifelse(HARVEST_YEAR >= 2008 & HARVEST_YEAR <= 2017,2008,dec.period)) 
        #c.cut.df[c.cut.df$dec.period == 0,]
        #unique(c.cut.df$dec.period)
        
        # all cutblocks (0-80 years)
        c.cut.df.out  = c.cut.df %>% 
          group_by(Range) %>% 
          summarise(C_cut0_80_m2 = sum(area))
        
        # output the amount of cutblock by range (all years (0-40))  
        c.cut.df.out.0.40 <- c.cut.df %>%
          filter(dec.period >= 1978) %>% 
          group_by(Range) %>% 
          summarise(C_cut0_40_m2 = sum(area))
        
        #output the amount of cutblock per decade
        c.cut.df.out.temp  = c.cut.df %>% 
          group_by(Range,dec.period) %>% 
          summarise(C_cut_dec_m2 = sum(area))
        
        #output the temporary (age group curblocks) 
        Temp.cut = left_join(r.cut.df.out.temp , c.cut.df.out.temp) 
        Temp.cut[is.na(Temp.cut)] <- 0
        Temp.cut$P_cut_dec_m2 = Temp.cut$R_cut_dec_m2 - Temp.cut$C_cut_dec_m2
        ## we will use this later with burn data 
        
        # add 0-80 years data 
        out.cut = left_join(r.cut.df.out,c.cut.df.out , all = both)
        out.cut$P_cut0_80_m2 = out.cut$R_cut0_80_m2 - out.cut$C_cut0_80_m2
        range.layer.cals <- left_join(range.layer.cals,out.cut) ; range.layer.cals [is.na(range.layer.cals )] <- 0
        
        # add 0-40 years data 
        out.cut0.40 = left_join(r.cut.df.out.0.40,c.cut.df.out.0.40 , all = both)
        out.cut0.40$P_cut0_40_m2 =  out.cut0.40$R_cut0_40_m2 -  out.cut0.40$C_cut0_40_m2
        range.layer.cals <- left_join(range.layer.cals,out.cut0.40) ; range.layer.cals [is.na(range.layer.cals )] <- 0
        
              
# 9) wells 
r.wells.sf <- st_read(dsn=Intersect,layer="R_WELL_FACILITY_PERMIT_SP_I" ) # multipoly
r.wells.sf <- st_union(r.wells.sf)
#all.wells = sum(st_area(r.wells.sf)) ; plot(st_geometry(r.wells.sf))  
  
        ## All Disturbance : UNION 5 # may need to run this in stand alone R rather than R -studio
        out6 = st_union(out5,r.wells.sf) ; plot(st_geometry(out6))
        out6 = st_union(out6); plot(st_geometry(out6))
        #x.area = sum(st_area(out6)) 

        ## Intersect with RANGE: intersect with range and calculate length per range
              r.wells = st_intersection(b.range,r.wells.sf)   # intersect with range
              r.wells <- st_cast(r.wells,"POLYGON")
              r.wells$area <- st_area(r.wells)
              
              ##plot(st_geometry(r.wells))
              #st_write(r.dams,"Dist_R_wells.shp")       #write out individual dist_layer for Range
              r.wells.df = data.frame(r.wells)        # calculate the length per range 
              r.wells.df.out  = r.wells.df %>% 
                group_by(Range) %>% 
                summarise(R_Wells_m2 = sum(area))
              
              # CORE: intersect with core and calculate length per range
              c.wells = st_intersection(b.core.r,r.wells.sf)   # intersect with core
              c.wells$area <- st_area(c.wells)
              ##plot(st_geometry(c.wells))
              #st_write(r.dams,"Dist_C_wells.shp")       #write out individual dist_layer for Range
              c.wells.df = data.frame(c.wells)        # calculate the length per range 
              c.wells.df.out  = c.wells.df %>% 
                group_by(Range) %>% 
                summarise(C_Wells_m2 = sum(area))
              
              out.wells = left_join(r.wells.df.out,c.wells.df.out, all = both)
              out.wells[is.na(out.wells)] <- 0
              out.wells$P_Wells_m2 = out.wells$R_Wells_m2 - out.wells$C_Wells_m2
              
              range.layer.cals <- left_join(range.layer.cals,out.wells) ; range.layer.cals [is.na(range.layer.cals )] <- 0

# 10) urban
r.urban.sf <- st_read(dsn=Dissolved,layer="R_Urban_dis" ) # multipoly
r.urban.sf <- st_union(r.urban.sf);
r.urban.sf <- st_cast(r.urban.sf,"POLYGON")
#all.urban = sum(st_area(r.urban.sf)) ; plot(st_geometry(r.urban.sf))  
      
      ## All disturbance: UNION 6 # may need to run this in stand alone R rather than R -studio
      out7 = st_union(out6,r.urban.sf) 
      out7 = st_union(out7);
      #x.area = sum(st_area(out6)) 
 
            ## Intersect with RANGE: intersect with range and calculate length per range
            r.urban = st_intersection(b.range,r.urban.sf)   # intersect with range
            r.urban = st_cast(r.urban,"POLYGON")
            r.urban$area <- st_area(r.urban)
            ##plot(st_geometry(r.urban))
            #st_write(r.dams,"Dist_R_urban.shp")       #write out individual dist_layer for Range
            r.urban.df = data.frame(r.urban)        # calculate the length per range 
            r.urban.df.out  = r.urban.df %>% 
              group_by(Range) %>% 
              summarise(R_Urban_m2 = sum(area))
            
            # CORE: intersect with core and calculate length per range
            c.urban = st_intersection(b.core.r,r.urban.sf)   # intersect with core
            c.urban = st_cast(c.urban,"POLYGON")
            c.urban$area <- st_area(c.urban)
            ##plot(st_geometry(c.urban))
            #st_write(r.dams,"Dist_C_urban.shp")       #write out individual dist_layer for Range
            c.urban.df = data.frame(c.urban)        # calculate the length per range 
            c.urban.df.out  = c.urban.df %>% 
              group_by(Range) %>% 
              summarise(C_urban_m2 = sum(area))
            
            out.urban = left_join(r.urban.df.out,c.urban.df.out, all = both)
            out.urban[is.na(out.urban)] <- 0
            out.urban$P_urban_m2 = out.urban$R_Urban_m2 - out.urban$C_urban_m2
            
            range.layer.cals <- left_join(range.layer.cals,out.urban) ; range.layer.cals [is.na(range.layer.cals )] <- 0
            
# 11) Rail  
r.rail.sf <- st_read(dsn=Intersect,layer="B_rail_clip") # multipoly
r.rail.sf <- st_intersection(b.range,r.rail.sf)
#r.rail.sf <- st_cast(r.rail.sf,"POLYGON") 
#st_is_valid(r.rail.sf)
#r.rail.sf <- st_make_valid((r.rail.sf))
r.rail <- st_buffer(r.rail.sf,1)
r.rail <- st_cast(r.rail,"POLYGON") ; all.rail= sum(st_area(r.rail)) ; plot(st_geometry(r.rail))  
     
        ## UNION 7 # may need to run this in stand alone R rather than R -studio
          out8 = st_union(out7,r.rail) ;# plot(st_geometry(out7))
          out8 = st_union(out8);# plot(st_geometry(out8))
          #x.area = sum(st_area(out7)) 
    
        # 1) RANGE: calculate the range extent to use range extent 
        r.rail <- st_intersection(b.range,r.tran.sf)# intersect with ranges
        r.rail <- st_buffer(r.rail,1) #;all.tran = sum(st_area(r.tran)) 
        r.rail <- st_cast(r.rail,"POLYGON")
        r.rail$area.m <- st_area(r.rail)
        r.rail.df = data.frame(r.rail)        # calculate the length per range 
        r.rail.df.out  = r.rail.df%>% 
          group_by(Range) %>% 
          summarise(R_Rail_m2 = sum(area.m))
        ##plot(st_geometry(r.rail))
        #st_write(r.rail,"Dist_R_rail.shp")       #write out individual dist_layer for Range
        
          # 2) CORE; intersect with core and calculate length per range 
          c.rail.int = st_intersection(b.core.r,r.tran.sf) # intersect with core areas
          c.rail.int <- st_buffer(c.rail.int,1) #;all.tran = sum(st_area(r.tran))
          c.rail.int = st_cast(c.rail.int,"POLYGON")
          c.rail.int$area.m <- st_area(c.rail.int)
          ##plot(st_geometry(c.rail.int))
          #st_write(c.trans.int,"Dist_C_Rail.shp") 
          c.rail.int.df = data.frame(c.rail.int)        # calculate the length per core/range 
          c.rail.int.df.out  = c.rail.int.df %>% 
            group_by(Range) %>% 
            summarise(C_Rail_m2 = sum(area.m))
          
          out.rail= left_join(r.rail.df.out,c.rail.int.df.out, all = both)
          out.rail[is.na(out.rail)] <- 0
          out.rail$P_Rail_m2 = out.rail$R_Rail_m2 - out.rail$C_Rail_m2
          
          range.layer.cals <- left_join(range.layer.cals,out.rail) ; range.layer.cals [is.na(range.layer.cals )] <- 0
          
    
# 12)  Recreation sites 
r.rec.sf <- st_read(dsn=Intersect,layer="R_rec_int") # multipoly
r.rec.sf <- st_intersection(b.range,r.rec.sf)
r.rec.sf <- st_union(r.rec.sf)
#all.rec = sum(st_area(r.rec.sf)) ; plot(st_geometry(r.rec.sf))  
    
        ## ALL DISTURBANCE: UNION 8 # may need to run this in stand alone R rather than R -studio
        out9 = st_union(out8,r.rec.sf) ; plot(st_geometry(out9))
        out9 = st_union(out9); plot(st_geometry(out9))
        #x.area = sum(st_area(out8))     
       
        # 1) RANGE: calculate the range extent to use range extent 
        r.rec <- st_intersection(b.range,r.rec.sf)# intersect with range
        r.rec <- st_cast(r.rec,"POLYGON")
        r.rec$area = st_area(r.rec)
        #all.rec = sum(st_area(r.rec))
        #st_is_valid(r.tran)                   # check valid geometr
        r.rec.df = data.frame(r.rec)        # calculate the length per range 
        r.rec.df.out  = r.rec.df%>% 
          group_by(Range) %>% 
          summarise(R_Rec_m2 = sum(area))
        
        ##plot(st_geometry(r.rec))
        #st_write(r.rec,"Dist_R_Rec.shp")       #write out individual dist_layer for Range
        
        # CORE: intersect with core and calculate length per range
        c.rec = st_intersection(b.core.r,r.rec.sf)   # intersect with core
        c.rec = st_cast(c.rec,"POLYGON")
        c.rec$area <- st_area(c.rec)
        ##plot(st_geometry(c.urban))
        #st_write(r.dams,"Dist_C_urban.shp")       #write out individual dist_layer for Range
        c.rec.df = data.frame(c.rec)        # calculate the length per range 
        c.rec.df.out  = c.rec.df %>% 
          group_by(Range) %>% 
          summarise(C_Rec_m2 = sum(area))
        
        out.rec = left_join(r.rec.df.out,c.rec.df.out, all = both)
        out.rec[is.na(out.rec)] <- 0
        out.rec$P_Rec_m2 = out.rec$R_Rec_m2 - out.rec$C_Rec_m2
        
        range.layer.cals <- left_join(range.layer.cals,out.rec) ; range.layer.cals [is.na(range.layer.cals )] <- 0
        
        
####################################################################    

# write out all the layers combined into a single disturbance layer ( note this excludes Roads and Seismic )
st_write(out9,paste(temp.dir,"Temp_disturb_combo_EX_rd_seis.shp",sep = "")) # this writes out as single layer   
        
# Write out the datasheet onto a temp file 
write.csv(range.layer.cals,paste(temp.dir,"Disturb_calcs_EX_rd_seis.csv",sep = "") )        
        
# make memory room!
rm('out1','out2','out3','out4','out5','out6','out7','r.agr.sf','r.air.sf','r.dam.sf','r.mine.sf')
rm('r.pipe','r.rail.sf','r.rec.sf','r.tran','r.tran.sf','r.urban.sf','r.wells.sf')
rm('b.r.c0.80')

################################################################################    
################################################################################

## PART 2; working with large datasets.(Roads and Seismic lines) 

### 13) Roads  
## Note : due to the size of the road files these may need to be broke downinto herd boundaries and run individually.
## For the boreal herds, each herd was first clipped to each boundary and exported as individual feature class. 
## For roads and seismic lines this is very slow and may need to be run on stand along R (rather than R-studio) 
## Due to the long processing time the area and length are both calculted within this script. The results are exported as csv file and shapefiles. 


## Herd 1) 
## maxhamish     ##working 
b.r1 = sf::st_read(dsn = Intersect , layer ="R_Road_Maxh" ) # multi surface
# cant get length from this layer ... perhaps export and then 
b.r1 <- st_cast(b.r1, "MULTIPOLYGON") 
b.r1 <- st_make_valid((b.r1))
b.r1 <- st_cast(b.r1, "POLYGON") 
plot(st_geometry(b.r1))
            
            # all disturbance layer: 
            b.r1.dis <- st_union(b.r1) #; plot(b.r1)
            st_write(b.r1.dis, paste(temp.dir,"Temp_roads_max.shp",sep = ""))
            plot(st_geometry(b.r1.dis))
            rm('b.r1.dis') 
            
            # intesect with range
            b.range1 <- st_intersection(b.range,b.r1)
            #head(b.range1)
            b.range1$area.m <- st_area(b.range1)
            b.range1$length.m <- st_length(b.range1)
            b.range1.df = data.frame(b.range1)        # calculate the length per range 
            b.range1.df.out  = b.range1.df%>% 
              group_by(Range) %>% 
              summarise(R_Road_Max_m2 = sum(area.m),R_Road_Max_length_m = sum(length.m))
            
            # intersect with core areas
            c.road1 = st_intersection(b.core.r,b.r1 )   # intersect with core
            #plot(st_geometry(c.road1))
            c.road1$area.m <- st_area(c.road1)
            c.road1$length.m <- st_length(c.road1)
            c.road1.df = data.frame(c.road1)        # calculate the length per range 
            c.road1.df.out  = c.road1.df%>% 
              group_by(Range) %>% 
              summarise(C_Road_Max_m2 = sum(area.m),C_Road_Max_length_m = sum(length.m))
            # add together and write out csv
            out.Max.road = left_join(b.range1.df.out,c.road1.df.out )
            out.Max.road = out.Max.road %>% filter(Range == "Maxhamish")
            out.Max.road [is.na(out.Max.road )] <- 0
            out.Max.road$P_Road_Max_m2 =  out.Max.road$R_Road_Max_m2 - out.Max.road$C_Road_Max_m2
            write.csv(out.Max.road,paste(temp.dir,"Max_temp_cals.csv",sep = ''))

            rm('b.r1') 
            
## Herd 2) 
# snake # working 
b.r5 = sf::st_read(dsn = Intersect , layer ="R_Roads_Snake") # geometry
b.r5 <- st_cast(b.r5, "MULTIPOLYGON") 
b.r5 <- st_make_valid((b.r5))  #; plot(st_geometry(b.r5))
b.r5 <- st_cast(b.r5, "POLYGON")

            # all disturbance layer: 
            b.r5.dis <- st_union(b.r5)  #; plot(b.r1)
            st_write(b.r5.dis, paste(temp.dir,"Temp_roads_snake.shp",sep = ""))
            plot(st_geometry(b.r5.dis))
            rm('b.r5.dis')
            # intesect with range
            b.range2 <- st_intersection(b.range,b.r5)
            #head(b.range1)
            b.range2$area.m <- st_area(b.range2)
            b.range2.df = data.frame(b.range2)        # calculate the length per range 
            b.range2.df.out  = b.range2.df%>% 
              group_by(Range) %>% 
              summarise(R_Road_Snake_m2 = sum(area.m))
            
            # intersect with core areas
            c.road2 = st_intersection(b.core.r,b.r5)   # intersect with core
            plot(st_geometry(c.road2))
            c.road2$area.m <- st_area(c.road2)
            c.road2.df = data.frame(c.road2)        # calculate the length per range 
            c.road2.df.out  = c.road2.df%>% 
              group_by(Range) %>% 
              summarise(C_Road_Snake_m2 = sum(area.m))#,C_Road_Snake_length_m = sum(length.m))
            
            
            # add together and write out csv
            out.Snake.road = left_join(b.range2.df.out,c.road2.df.out )
            out.Snake.road = out.Snake.road %>% filter(Range == "Snake-Sahtahneh")
            out.Snake.road [is.na( out.Snake.road)] <- 0
            out.Snake.road$P_Road_Snake_m2 =  out.Snake.road$R_Road_Snake_m2 - out.Snake.road$C_Road_Snake_m2
            write.csv(out.Snake.road,paste(temp.dir,"Snake_temp_cals.csv",sep = ''))
            
rm('b.r5') 

## Herd 3) 
## Chincaga # WORKING 
b.r2 = sf::st_read(dsn = Intersect , layer ="R_Roads_Chin")
b.r2 <- st_cast(b.r2, "MULTIPOLYGON")
st_make_valid(b.r2) #; plot(st_geometry(b.r2))
b.r2 <- st_cast(b.r2, "POLYGON")

            # all disturbance layer: 
            b.r2.dis <- st_union(b.r2)  #; plot(b.r1)
            st_write(b.r2.dis, paste(temp.dir,"Temp_roads_Chin.shp",sep = ""))
            plot(st_geometry(b.r2.dis)) ; rm('b.r2.dis')
            
            # intesect with range
            b.range3 <- st_intersection(b.range,b.r2)
            b.range3$area.m <- st_area(b.range3)
            b.range3.df = data.frame(b.range3)        # calculate the length per range 
            b.range3.df.out  = b.range3.df%>% 
              group_by(Range) %>% 
              summarise(R_Road_Chin_m2 = sum(area.m))
            
            # intersect with core areas
            c.road2 = st_intersection(b.core.r,b.r2)   # intersect with core
            #plot(st_geometry(c.road2))
            
            c.road2$area.m <- st_area(c.road2)
            c.road2.df = data.frame(c.road2)        # calculate the length per range 
            c.road2.df.out  = c.road2.df%>% 
              group_by(Range) %>% 
              summarise(C_Road_Chin_m2 = sum(area.m))#,C_Road_Chin_length_m = sum(length.m))
            
            # add together and write out csv
            out.Chin.road = left_join(b.range3.df.out,c.road2.df.out)
            out.Chin.road = out.Chin.road %>% filter(Range == "Chinchaga")
            out.Chin.road [is.na(out.Chin.road)] <- 0
            out.Chin.road$P_Road_Chin_m2 =  out.Chin.road$R_Road_Chin_m2 - out.Chin.road$C_Road_Chin_m2
            write.csv(out.Chin.road,paste(temp.dir,"Chin_temp_cals.csv",sep = ''))

rm('b.r2') 

#####################
# Herd 4) 
# westside
b.r4 = sf::st_read(dsn = Dissolved, layer ="R_roads_west")  
b.r4 <- st_cast(b.r4, "MULTIPOLYGON")
st_make_valid(b.r4) #; plot(st_geometry(b.r4))
b.r4 <- st_cast(b.r4, "POLYGON")

            # all disturbance layer: 
            b.r4.dis <- st_union(b.r4)  
            st_write(b.r4.dis, paste(temp.dir,"Temp_roads_West.shp",sep = ""))
            #plot(st_geometry(b.r2.dis)) ; 
            rm('b.r4.dis')

          # intesect with range
          b.range4 <- st_intersection(b.range,b.r4)
          b.range4$area.m <- st_area(b.range4)
          b.range4.df = data.frame(b.range4)        # calculate the length per range 
          b.range4.df.out  = b.range4.df%>% 
            group_by(Range) %>% 
            summarise(R_Road_West_m2 = sum(area.m))

          # intersect with core areas
          c.road4 = st_intersection(b.core.r,b.r4)   # intersect with core
          c.road4$area.m <- st_area(c.road4)
          c.road4.df = data.frame(c.road4)        # calculate the length per range 
          c.road4.df.out  = c.road4.df%>% 
            group_by(Range) %>% 
            summarise(C_Road_West_m2 = sum(area.m))#,C_Road_Chin_length_m = sum(length.m))
          # add together and write out csv
          out.West.road = left_join(b.range4.df.out,c.road4.df.out)
          out.West.road = out.West.road %>% filter(Range == "Westside Fort Nelson")
          out.West.road[is.na(out.West.road)] <- 0
          out.West.road$P_Road_West_m2 =  out.West.road$R_Road_West_m2 - out.West.road$C_Road_West_m2
          write.csv(out.West.road,paste(temp.dir,"West_temp_cals.csv",sep = ''))

rm('b.r4') 

### Herd 5) 

## Calendar
b.r3 = sf::st_read(dsn = Dissolved, layer ="R_roads_Cal")  
b.r3 <- st_cast(b.r3, "MULTIPOLYGON")
st_make_valid(b.r3) #; plot(st_geometry(b.r3))
b.r3 <- st_cast(b.r3, "POLYGON")
          
          # all disturbance layer: 
          b.r3.dis <- st_union(b.r3)  
          st_write(b.r3.dis, paste(temp.dir,"Temp_roads_Cale.shp",sep = ""))
          #plot(st_geometry(b.r3.dis)) ; 
          rm('b.r3.dis')

          # intesect with range
          b.range5 <- st_intersection(b.range,b.r3)
          b.range5$area.m <- st_area(b.range5)
          b.range5.df = data.frame(b.range5)        # calculate the length per range 
          b.range5.df.out  = b.range5.df%>% 
            group_by(Range) %>% 
            summarise(R_Road_Cale_m2 = sum(area.m))
          
          # intersect with core areas
          c.road5 = st_intersection(b.core.r,b.r3)   # intersect with core
          c.road5$area.m <- st_area(c.road5)
          c.road5.df = data.frame(c.road5)        # calculate the length per range 
          c.road5.df.out  = c.road5.df%>% 
            group_by(Range) %>% 
            summarise(C_Road_Cale_m2 = sum(area.m))#,C_Road_Chin_length_m = sum(length.m))
          
          # add together and write out csv
          out.Cale.road = left_join(b.range5.df.out,c.road5.df.out)
          out.Cale.road = out.Cale.road %>% filter(Range == "Calendar")
          out.Cale.road[is.na(out.Cale.road)] <- 0
          out.Cale.road$P_Road_Cale_m2 =  out.Cale.road$R_Road_Cale_m2 - out.Cale.road$C_Road_Cale_m2
          write.csv(out.Cale.road,paste(temp.dir,"Cale_temp_cals.csv",sep = ''))

rm('b.r3')
          
# check the files 
#out1 = st_read("X:/projects/Desktop_Analysis/data/temp/Temp_out1.shp")
#r1 = st_read("X:/projects/Desktop_Analysis/data/temp/Temp_out2.1.shp")
#r2 = st_read("X:/projects/Desktop_Analysis/data/temp/Temp_out2.2.shp")
#r3 = st_read("X:/projects/Desktop_Analysis/data/temp/Temp_out2.3.shp")
#r4 = st_read("X:/projects/Desktop_Analysis/data/temp/Temp_out2.4.shp")

########################################################################
## Combine the generated layers with the road layers 

## Open ArcMap project
## set projections to 3005 (albers layers) or use the base layers 
## Read in the all disturbacne layer (out9) and the 5 road layers
## Merge the 5x roads layer with the merged simple output layers generated above:  "Temp_disturb_combo_EX_rd_seis.shp"
## Project to 3005 (albers projection and export )
## Output stored in Scratch1 geodatabase  
## save in out.dir as "Disturb_poly_draft1.shap
## note this is still missing the seismic line data 


###########################################################
# if you want to check the outputs 

out9 = st_read(dsn = Intersect, layer = "Disturb_poly_draft1" )
#out9 = st_read(paste(out.dir,"Disturb_poly_draft1.shp",sep = ""), package="sf")
st_transform(out9,3005)
out9 <- st_cast(out9, "POLYGON")
st_is_valid(out9)
st_make_valid(out9) ; plot(st_geometry(out9))
#out9 <- st_union(out9)


###################################################################
# Repeat the same for seismic 

# within arc map you will need to combine these four datasets 

## Seismic layers need to be prepped individually per layer (x4) 
#WHSE_MINERAL_TENURE_OG_GEOPHYSICAL_1996_2004_SP
#WHSE_MINERAL_TENURE_OG_GEOPHYSICAL_2002_2006_SP
#WHSE_MINERAL_TENURE_OG_GEOPHYSICAL_PUB_SP
#WHSE_BASEMAPPING.TRIM_MISCELLANEOUS_LINES

# For each layer 
# export a copy from the WHSE -
# clip for each of the herd boundaries- Using the most encompasing extent of the caribou boundary
# For the 4 layers merge together 
# these merged files are then read into R 
# the naming convention in ArcMap is "#####_Seis_misc_merge" stored in the 


## Herd 1) 
## maxhamish  
b.s1 = sf::st_read(dsn = Intersect , layer ="R_Seis_Max_4") 
b.s1  = st_transform(b.s1,3005)
b.s1 = st_buffer(b.s1,5) # buffer to the average width of the seismic line 

b.road1 <- st_intersection(b.range,b.s1)# intersect with range
b.road1 <- st_cast(b.road1, "POLYGON")
#st_is_valid(b.road1)
# st_make_valid(b.road1) #; plot(st_geometry(b.r3))

# write out a copy to dissolve in ArcMap
st_write(b.road1, paste(temp.dir,"Temp_Seis_Max.shp",sep = ""))

    # dissolved layer for all disturbance layer: # do this instead in ArcMap 
    #b.r3.dis <- st_union(b.road1)  
    #rm('b.r3.dis')
  
    # range area
    b.road1$area.m <- st_area(b.road1)
    b.road1.df = data.frame(b.road1)        # calculate the length per range 
    b.road1.df.out  = b.road1.df%>% 
      group_by(Range) %>% 
      summarise(R_Seis_Max_m2 = sum(area.m))
    
    # intersect with core areas
    c.road1 = st_intersection(b.core.r,b.road1)   # intersect with core
    c.road1$area.m <- st_area(c.road1)
    c.road1.df = data.frame(c.road1)        # calculate the length per range 
    c.road1.df.out  = c.road1.df%>% 
      group_by(Range) %>% 
      summarise(C_Seis_Max_m2 = sum(area.m))#,C_Road_Chin_length_m = sum(length.m))
    
    
    # add together and write out csv
    out.Cale.road = left_join(b.road1.df.out, c.road1.df.out)
    out.Cale.road = out.Cale.road %>% filter(Range == "Maxhamish")
    out.Cale.road[is.na(out.Cale.road)] <- 0
    out.Cale.road$P_Seis_Max_m2 =  out.Cale.road$R_Seis_Max_m2 - out.Cale.road$C_Seis_Max_m2
    
    out.seis = out.Cale.road
  
    all.out<- out.seis %>% 
      mutate(R_Seis_m2 = R_Seis_Max_m2,  C_Seis_m2 = C_Seis_Max_m2, P_Seis_m2 = P_Seis_Max_m2 ) %>%
      dplyr::select(c( Range, R_Seis_m2, C_Seis_m2, P_Seis_m2))

    #write.csv(out.Cale.road,paste(temp.dir,"Cale_temp_cals.csv",sep = '')
    rm('c.road1','b.road1')
    
    
## Herd 2) 
## Snake 
    b.s1 = sf::st_read(dsn = Intersect , layer ="Snake_Seis_misc_Merge") 
    b.s1  = st_transform(b.s1,3005)
    b.s1 = st_buffer(b.s1,5) # buffer to the average width of the seismic line 
    
    b.road1 <- st_intersection(b.range,b.s1)# intersect with range
    b.road1 <- st_cast(b.road1, "POLYGON")
    #st_is_valid(b.road1)
    # st_make_valid(b.road1) #; plot(st_geometry(b.r3))
    
    # write out a copy to dissolve in ArcMap
    st_write(b.road1, paste(temp.dir,"R_Seis_Snake.shp",sep = ""))
    
    # range area
    b.road1$area.m <- st_area(b.road1)
    b.road1.df = data.frame(b.road1)        # calculate the length per range 
    b.road1.df.out  = b.road1.df%>% 
      group_by(Range) %>% 
      summarise(R_Seis_m2 = sum(area.m))
    
    # intersect with core areas
    c.road1 = st_intersection(b.core.r,b.road1)   # intersect with core
    c.road1$area.m <- st_area(c.road1)
    c.road1.df = data.frame(c.road1)        # calculate the length per range 
    c.road1.df.out  = c.road1.df%>% 
      group_by(Range) %>% 
      summarise(C_Seis_m2 = sum(area.m))#,C_Road_Chin_length_m = sum(length.m))
    
    
    # add together and write out csv
    out.Cale.road = left_join(b.road1.df.out, c.road1.df.out)
    out.Cale.road = out.Cale.road %>% filter(Range == "Snake-Sahtahneh")
    out.Cale.road[is.na(out.Cale.road)] <- 0
    out.Cale.road$P_Seis_m2 =  out.Cale.road$R_Seis_m2 - out.Cale.road$C_Seis_m2
    
    # add to other road data: 
    all.out= rbind(all.out,out.Cale.road)
    all.out
  
    #write.csv(out.Cale.road,paste(temp.dir,"Cale_temp_cals.csv",sep = '')
    rm('c.road1','b.road1')
    
  
## herd 3)  ##Cal 
b.s1 = sf::st_read(dsn = Intersect , layer ="Cal_Seis_misc_Merge") 
b.s1  = st_transform(b.s1 ,3005)
b.s1 = st_buffer(b.s1,5) # buffer to the average width of the seismic line 

    b.road1 <- st_intersection(b.range,b.s1)# intersect with range
    b.road1 <- st_cast(b.road1, "POLYGON")
    #st_is_valid(b.road1)
    # st_make_valid(b.road1) #; plot(st_geometry(b.r3))
    
    # write out a copy to dissolve in ArcMap
    st_write(b.road1, paste(temp.dir,"R_Seis_Cal.shp",sep = ""))
   
    # range area
    b.road1$area.m <- st_area(b.road1)
    b.road1.df = data.frame(b.road1)        # calculate the length per range 
    b.road1.df.out  = b.road1.df%>% 
      group_by(Range) %>% 
      summarise(R_Seis_m2 = sum(area.m))
    
    # intersect with core areas
    c.road1 = st_intersection(b.core.r,b.road1)   # intersect with core
    c.road1$area.m <- st_area(c.road1)
    c.road1.df = data.frame(c.road1)        # calculate the length per range 
    c.road1.df.out  = c.road1.df%>% 
      group_by(Range) %>% 
      summarise(C_Seis_m2 = sum(area.m))#,C_Road_Chin_length_m = sum(length.m))
    
    # add together and write out csv
    out.Cale.road = left_join(b.road1.df.out, c.road1.df.out)
    out.Cale.road = out.Cale.road %>% filter(Range == "Calendar")
    out.Cale.road[is.na(out.Cale.road)] <- 0
    out.Cale.road$P_Seis_m2 =  out.Cale.road$R_Seis_m2 - out.Cale.road$C_Seis_m2
    
    # add to other road data: 
    all.out = rbind(all.out,out.Cale.road)
    all.out
  
## herd 4) # west 
b.s1 = sf::st_read(dsn = Intersect , layer ="West_Seis_misc_Merge")  
b.s1  = st_transform(b.s1 ,3005)
b.s1 = st_buffer(b.s1,5) # buffer to the average width of the seismic line 

    b.road1 <- st_intersection(b.range,b.s1)# intersect with range
    b.road1 <- st_cast(b.road1, "POLYGON")
    #st_is_valid(b.road1)
    # st_make_valid(b.road1) #; plot(st_geometry(b.r3))

    # write out a copy to dissolve in ArcMap
    st_write(b.road1, paste(temp.dir,"R_Seis_West.shp",sep = ""))

        # range area
    b.road1$area.m <- st_area(b.road1)
    b.road1.df = data.frame(b.road1)        # calculate the length per range 
    b.road1.df.out  = b.road1.df%>% 
      group_by(Range) %>% 
      summarise(R_Seis_m2 = sum(area.m))

    # intersect with core areas
    c.road1 = st_intersection(b.core.r,b.road1)   # intersect with core
    c.road1$area.m <- st_area(c.road1)
    c.road1.df = data.frame(c.road1)        # calculate the length per range 
    c.road1.df.out  = c.road1.df%>% 
      group_by(Range) %>% 
      summarise(C_Seis_m2 = sum(area.m))#,C_Road_Chin_length_m = sum(length.m))

  # add together and write out csv
  out.Cale.road = left_join(b.road1.df.out, c.road1.df.out)
  out.Cale.road[is.na(out.Cale.road)] <- 0
  out.Cale.road$P_Seis_m2 =  out.Cale.road$R_Seis_m2 - out.Cale.road$C_Seis_m2
  
  # add to other road data: 
  all.out = rbind(all.out,out.Cale.road)
  all.out
  #x = all.out

## herd 5) # Chin 
b.s1 = sf::st_read(dsn = Intersect , layer ="Chin_Seis_misc_Merge") 
b.s1  = st_transform(b.s1 ,3005)
b.s1 = st_buffer(b.s1,5) # buffer to the average width of the seismic line 

    b.road1 <- st_intersection(b.range,b.s1)# intersect with range
    b.road1 <- st_cast(b.road1, "POLYGON")
    #st_is_valid(b.road1)
    # st_make_valid(b.road1) #; plot(st_geometry(b.r3))

    # write out a copy to dissolve in ArcMap
    st_write(b.road1, paste(temp.dir,"R_Seis_Chin.shp",sep = ""))
  
        # range area
    b.road1$area.m <- st_area(b.road1)
    b.road1.df = data.frame(b.road1)        # calculate the length per range 
    b.road1.df.out  = b.road1.df%>% 
      group_by(Range) %>% 
      summarise(R_Seis_m2 = sum(area.m))

    # intersect with core areas
    c.road1 = st_intersection(b.core.r,b.road1)   # intersect with core
    c.road1$area.m <- st_area(c.road1)
    c.road1.df = data.frame(c.road1)        # calculate the length per range 
    c.road1.df.out  = c.road1.df%>% 
      group_by(Range) %>% 
      summarise(C_Seis_m2 = sum(area.m))#,C_Road_Chin_length_m = sum(length.m))

    # add together and write out csv
    out.Cale.road = left_join(b.road1.df.out, c.road1.df.out)
    #out.Cale.road = out.Cale.road %>% filter(Range == "Maxhamish")
    out.Cale.road[is.na(out.Cale.road)] <- 0
    out.Cale.road$P_Seis_m2 =  out.Cale.road$R_Seis_m2 - out.Cale.road$C_Seis_m2
    
    # add to other road data: 
    all.out = rbind(all.out,out.Cale.road)
    all.out


#Write out the all.out data files     
 
write.csv(all.out,paste(temp.dir,"Seismic_dist_all_herds.csv",sep = ''))

#######################


##################################################################################
##################################################################################

#### NATURAL DISTURBANCE   

## BURN:
# Add to Burn information Break down of burns into 0-40 and 40-80 years (and total) 
# Break down burns into fire years (same as cutblocks) 

b.r.0 = st_read(dsn = Intersect, layer ="Burn_combo_int")
b.r.0$TimeSinceBurn = 2018-b.r.0$FIRE_YEAR #; plot(b.r.0$Shape)
b.r.0 <- st_intersection(b.range,b.r.0)

b.r.00 = b.r.0
b.r.00 <- st_cast(b.r.00,"POLYGON")
b.r.00$area = st_area(b.r.00)
head(b.r.00)

#sort(unique(b.r.0$TimeSinceBurn))  # 0 - 40 years range 
# burns 0-40 years 
b.r.0.40 = b.r.0[b.r.0$TimeSinceBurn <41,]; #sort(unique(b.r.0$TimeSinceBurn)) 
b.r.0.40 = st_intersection(b.range,b.r.0.40)
b.r.0.40 <- st_cast(b.r.0.40 ,"POLYGON")
b.r.0.40$area = st_area(b.r.0.40)
#st_write(b.r.0.40,"Dist_R_burn.0.40.shp")       #write out individual dist_layer for Range

all.dis.burn = st_union(b.r.0.40)

# burns 41-80 years 
b.r.40.80 = b.r.0[b.r.0$TimeSinceBurn>40,]; #sort(unique(b.r.40.80$TimeSinceBurn)) 
b.r.40.80 = st_intersection(b.range,b.r.40.80)
b.r.40.80 <- st_cast(b.r.40.80 ,"POLYGON")
b.r.40.80$area = st_area(b.r.40.80)
#st_write(b.r.40.80,"Dist_R_burn.40.80.shp")       #write out individual dist_layer for Range

## All years of burns 
b.r.0 = st_cast(b.r.0,"POLYGON");  
b.r.0 = st_union(b.r.0) ; #plot(b.r.c0.40)
b.r.0 <- st_intersection(b.range,b.r.0)
b.r.0$area = st_area(b.r.0)
#all.burn = sum(st_area(b.r.0)) 
#plot(st_geometry(b.r.0))
#st_write(b.r.0 ,"Dist_R_burn.0.80.shp")       #write out individual dist_layer for Range
      
# work with the data frames for 0-40 years ## RANGE 
          b.r.0.40.df = data.frame(b.r.0.40)        # calculate the length per range 
          b.r.00.df = data.frame(b.r.00)
          
          # add a column to differentiate the age brackets of cutblocks 
          b.r.00.df <- mutate(b.r.00.df,dec.period = ifelse(FIRE_YEAR >= 1958 & FIRE_YEAR <= 1967,1958,0))
          b.r.00.df <- mutate(b.r.00.df,dec.period = ifelse(FIRE_YEAR >= 1968 & FIRE_YEAR <= 1977,1968,dec.period))
          b.r.00.df<- mutate(b.r.00.df,dec.period = ifelse(FIRE_YEAR >= 1978 & FIRE_YEAR <= 1987,1978,dec.period))
          b.r.00.df<- mutate(b.r.00.df,dec.period = ifelse(FIRE_YEAR >= 1988 & FIRE_YEAR <= 1997,1988,dec.period))   
          b.r.00.df<- mutate(b.r.00.df,dec.period = ifelse(FIRE_YEAR >= 1998 & FIRE_YEAR <= 2007,1998,dec.period))   
          b.r.00.df<- mutate(b.r.00.df,dec.period = ifelse(FIRE_YEAR >= 2008 & FIRE_YEAR <= 2018,2008,dec.period)) 
          #b.r.0.40.df[b.r.0.40.df$dec.period == 0,]
          #head(b.r.0.40.df)
          #unique(b.r.0.40.df$dec.period)
          
          # output the amount of burns by range (all years (0-80))  
          b.r.0.df = data.frame(b.r.0)
          r.burn.df.out  = b.r.0.df  %>% group_by(Range) %>% summarise(R_burn0_80_m2 = sum(area))
          
          # output the amount of burns by range (all years (0 - 40)  
          r.burn.df.out.0.40  = b.r.0.40.df %>% group_by(Range) %>% summarise(R_burn0_40_m2 = sum(area))
          
          # output the amount of burns by range (all years (40-80))  
          b.r.40.80.df = data.frame(b.r.40.80)
          r.burn.df.out.40.80  = b.r.40.80.df %>% group_by(Range) %>% summarise(R_burn40_80_m2 = sum(area))
          
          #output the amount of cutblock per decade (all years) 
          b.r.00.df.temp  =  b.r.00.df %>% group_by(Range,dec.period) %>% summarise(R_burn_dec_m2 = sum(area))
        
          # aggregate to single datset 
          burn.range = left_join(r.burn.df.out,r.burn.df.out.0.40)
          burn.range = left_join(burn.range,r.burn.df.out.40.80 )
         
    #############################
    #### CORE: intersect with core and calculate length per range
    # all years: 
          c.burn = st_intersection(b.core.r,b.r.00)   # intersect with core
          c.burn <- st_cast(c.burn,"POLYGON")
          c.burn$area <- st_area(c.burn)
          
    # 0 - 40 years 
          c.burn0.40 = st_intersection(b.core.r,b.r.0.40)   # intersect with core
          c.burn0.40 <- st_cast(c.burn0.40,"POLYGON")
          c.burn0.40$area <- st_area(c.burn0.40)
          
    # 41 - 80 years
          c.burn40.80 = st_intersection(b.core.r,b.r.40.80)   # intersect with core
          c.burn40.80 <- st_cast(c.burn40.80,"POLYGON")
          c.burn40.80$area <- st_area(c.burn40.80)
      
     ## Working with data frames
          
          c.burn.00.df = data.frame(c.burn)        # calculate the length per range 
          
          # add a column to differentiate the age brackets of cutblocks 
          c.burn.00.df <- mutate(c.burn.00.df ,dec.period = ifelse(FIRE_YEAR >= 1958 & FIRE_YEAR <= 1967,1958,0))
          c.burn.00.df  <- mutate(c.burn.00.df ,dec.period = ifelse(FIRE_YEAR >= 1968 & FIRE_YEAR <= 1977,1968,dec.period))
          c.burn.00.df <- mutate(c.burn.00.df ,dec.period = ifelse(FIRE_YEAR >= 1978 & FIRE_YEAR <= 1987,1978,dec.period))
          c.burn.00.df <- mutate(c.burn.00.df ,dec.period = ifelse(FIRE_YEAR >= 1988 & FIRE_YEAR <= 1997,1988,dec.period))   
          c.burn.00.df <- mutate(c.burn.00.df ,dec.period = ifelse(FIRE_YEAR >= 1998 & FIRE_YEAR <= 2007,1998,dec.period))   
          c.burn.00.df <- mutate(c.burn.00.df ,dec.period = ifelse(FIRE_YEAR >= 2008 & FIRE_YEAR <= 2018,2008,dec.period)) 
          #c.burn0.40.df[c.burn0.40.df$dec.period == 0,]
          #head(c.burn0.40.df)
          #unique(c.burn0.40.df$dec.period)

          # output the amount of burns by range (all years (0-80))  
          c.burn.df = data.frame(c.burn)
          c.burn.df.out  = c.burn.df %>% group_by(Range) %>% summarise(C_burn0_80_m2 = sum(area))
          
          # output the amount of burns by range (all years (0 - 40)  
          c.burn.df.out.0.40  = data.frame(c.burn0.40) %>% group_by(Range) %>% summarise(C_burn0_40_m2 = sum(area))
          
          # output the amount of burns by range (all years (40-80))  
          c.burn40.80.df = data.frame(c.burn40.80)
          c.burn.df.out.40.80  = c.burn40.80.df  %>% group_by(Range) %>% summarise(C_burn40_80_m2 = sum(area))
          
          #output the amount of cutblock per decade (all years) 
          c.burn.00.df.temp  =  c.burn.00.df %>% group_by(Range,dec.period) %>% summarise(C_burn_dec_m2 = sum(area))
          
          # aggregate to single datset 
          burn.core = left_join(c.burn.df.out,c.burn.df.out.0.40)
          burn.core = left_join(burn.core,c.burn.df.out.40.80)
          
          ## Combine to burn summary and write out       
# burn by range 
burn.all = left_join(burn.range,burn.core)
burn.all$P_burn0_80_m2 = burn.all$R_burn0_80_m2 - burn.all$C_burn0_80_m2  
burn.all$P_burn0_40_m2 = burn.all$R_burn0_40_m2 - burn.all$C_burn0_40_m2
burn.all$P_burn40_80_m2 = burn.all$R_burn40_80_m2 - burn.all$C_burn40_80_m2

# burn by range and decade
burn.by.temp = left_join(b.r.00.df.temp,c.burn.00.df.temp) 
burn.by.temp$P_burn_dec_m2 = burn.by.temp$R_burn_dec_m2 - burn.by.temp$C_burn_dec_m2        

###########################################

# join the cutlock with burn decade data. 
all.temp.data = left_join(burn.by.temp,Temp.cut)
all.temp.data[is.na(all.temp.data)]<- 0 

############################################
### PEST 
r.pest <-  st_read(dsn = Intersect, layer ="Pest_r_Select_Intersect")
r.pest <-  st_intersection(b.range,r.pest)
        
        # All disturbance 
        r.pest.0 = st_cast(r.pest,"POLYGON")
        r.pest.0 = st_union(r.pest.0)
        #all.pest = sum(st_area(r.pest.0)) ; plot(st_geometry(r.pest.0))  

        ## RANGE SCALE:   
        # work with the data frames for 0-40 years ## RANGE 
        r.pest = st_cast(r.pest,"POLYGON") 
        r.pest$area.m = st_area(r.pest)
        r.pest.df = data.frame(r.pest)        # calculate the length per range 
        
        # add a column to differentiate the age brackets of pest capture
        r.pest.df<- r.pest.df %>% mutate(dec.period = ifelse(CAPTURE_YEAR >= 1988 & CAPTURE_YEAR <= 1997,1988,0))   
        r.pest.df<- mutate(r.pest.df,dec.period = ifelse(CAPTURE_YEAR >= 1998 & CAPTURE_YEAR <= 2007,1998,dec.period))   
        r.pest.df<- mutate(r.pest.df,dec.period = ifelse(CAPTURE_YEAR >= 2008 & CAPTURE_YEAR <= 2018,2008,dec.period)) 
        #r.pest.df[r.pest.df$dec.period == 0,]
        #head(r.pest.df)
        #unique(r.pest.df$dec.period)
        
        # output the amount of burns by range (all years (0-80))  
        r.pest.df.out  = r.pest.df %>% group_by(Range) %>% summarise(R_pest_m2 = sum(area.m))
        #output the amount of cutblock per decade (all years) 
        r.pest.df.out.temp  =   r.pest.df %>% group_by(Range,dec.period) %>% summarise(R_pest_dec_m2 = sum(area.m))
        #output the amount of cutblock per decade (all years) 
        r.pest.df.out.type  = r.pest.df %>% group_by(Range,PEST_SPECIES_CODE) %>% summarise(R_pest_type_m2 = sum(area.m))
        
        ########################################
        ## CORE SCALE
        c.pest <-  st_intersection(b.core.r,r.pest)
        # All disturbance 
        c.pest.0 = st_cast(c.pest,"POLYGON")
        c.pest.0= st_union(c.pest.0)
        #all.pest = sum(st_area(r.pest.0)) ; plot(st_geometry(r.pest.0))  
        
        # work with the data frames for 0-40 years ## RANGE 
        c.pest = st_cast(c.pest,"POLYGON") 
        c.pest$area.m = st_area(c.pest)
        c.pest.df = data.frame(c.pest)        # calculate the length per range 
        
        # add a column to differentiate the age brackets of pest capture
        c.pest.df<- c.pest.df %>% mutate(dec.period = ifelse(CAPTURE_YEAR >= 1988 & CAPTURE_YEAR <= 1997,1988,0))   
        c.pest.df<- mutate(c.pest.df,dec.period = ifelse(CAPTURE_YEAR >= 1998 & CAPTURE_YEAR <= 2007,1998,dec.period))   
        c.pest.df<- mutate(c.pest.df,dec.period = ifelse(CAPTURE_YEAR >= 2008 & CAPTURE_YEAR <= 2018,2008,dec.period)) 
        #c.pest.df[c.pest.df$dec.period == 0,]
        #head(r.pest.df)
        #unique(r.pest.df$dec.period)
        
        # output the amount of pests by range (all years (0-80))  
        c.pest.df.out  = c.pest.df %>% group_by(Range) %>% summarise(C_pest_m2 = sum(area.m))
        #output the amount of cutblock per decade (all years) 
        c.pest.df.out.temp  = c.pest.df %>% group_by(Range,dec.period) %>% summarise(C_pest_dec_m2 = sum(area.m))
        #output the amount of cutblock per decade (all years) 
        c.pest.df.out.type  = c.pest.df %>% group_by(Range,PEST_SPECIES_CODE) %>% summarise(C_pest_type_m2 = sum(area.m))
        
        # aggregate range and core
        pest.range = left_join(r.pest.df.out,c.pest.df.out)
        pest.range$P_pest_m2 =  pest.range$R_pest_m2 -  pest.range$C_pest_m2 
        # by temp
        pest.temp = left_join( r.pest.df.out.temp, c.pest.df.out.temp)
        pest.temp[is.na( pest.temp)]<- 0 
        # by type
        pest_type <-  left_join( r.pest.df.out.type, c.pest.df.out.type)
        pest_type$P_pest_type_m2 = pest_type$R_pest_type_m2 - pest_type$C_pest_type_m2
        write.csv(pest_type, paste(out.dir,"Pest_by_type_RPC.csv",sep =""))
        
        
   
############################################################################
# Aggregate natural disturbance    
        
# Pests + Burns (0-40 years combined)
        
############################################################################       
    
# Step 1 aggregate table and output 
        
out.temp = left_join(burn.all, pest.range)   
write.csv(out.temp,paste(out.dir,"Nat_dist_type_RPC.csv",sep =""))    

# write out temporal datasets: 
all.temp.data <-left_join(all.temp.data,pest.temp)           
write.csv(all.temp.data,paste(out.dir,"Temporal_decade_dist_RPC.csv",sep ="")) 

       
# Add disturbance combined: UNION Natural Dist 
r.pest.0 = st_union(r.pest.0) 
out.nat = st_union(all.dis.burn, r.pest.0) ; plot(st_geometry(out.nat))

# write out shape file 
out.nat2 = st_cast(out.nat,"POLYGON")
xnat.area2 = sum(st_area(out.nat2)) 
x = st_simplify(out.nat2)    ;  plot(st_geometry(x)) 
st_write(x, "All_natural_dissolve.shp") # this writes out as single layer   

# calculate the range and core overlaps for "all disturbance" 
out.nat1 = st_union(out.nat);
st_make_valid(out.nat1)

out.nat1 <- st_cast(out.nat1,"POLYGON")

# calcualte the totals for all nat. disturbacne 
out.nat.r = st_intersection(b.range,out.nat1)
out.nat.c = st_intersection(b.core.r,out.nat1)
# add areas
out.nat.r$area.m = st_area(out.nat.r)
out.nat.c$area.m = st_area(out.nat.c)
# conver to DF
out.nat.r.df <-data.frame(out.nat.r)
out.nat.c.df <-data.frame(out.nat.c)
# summarise data 
range.out  <- out.nat.r.df%>%  group_by(Range) %>% summarise(R_allnatdis_m = sum(area.m))
core.out <- out.nat.c.df %>%  group_by(Range) %>% summarise(C_allnatdis_m = sum(area.m))
# 

all.dis<- left_join(range.out,core.out)
all.dis$P_allnatdis_m  = all.dis$R_allnatdis_m  - all.dis$C_allnatdis_m 
write.csv(all.dis,paste(out.dir,"Combines_Nat_dist_RPC.csv",sep ="")) 


###################################################################################

# once this script is run - then use the 04_Disturbance_Data_summary.R script 
# to collate all the csv's generated as part of this script and oputput the main summary table for reporting


