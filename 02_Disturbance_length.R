## 
## Caribou disturbance analysis 2018 
## 
## August 14th 2018 
##
## written by genevieve perkins (genevieve.perkins@gov.bc.ca)
## 
## This requires initial preparation of layers within arcmap. 
## Step 1) 
##
## Assemble layers as per datasheet in arcmap mxd 
## Clip the layers to the range with the largest extent (for example range boundary for Boreal (not core))
## Create a filegeodatabdase and output these to the given data base. 
##
## Step 2)
## Run through the script 01_disturbance layers first! 
## 
## Step 3) 
## Run this script
## You may need to adjust 
## - the names of files to match your arcmap exports 
## - the directory/folder sructure. 
##
## Step 4)
## For road and seismic lines these need to be run in Arcmap. 
## To assemble the "all disturbance layers" this need to be done in ArcMap
## The final assemble of the data tables is done at the end of the script.


## General notes: 
## For each disturbance layers the script will read in, intersect with range and core areas and calculate the length. The peripery area will be calculated for each herd as well. 
## With each layer the compiled disturbance will also be calculated. 



## Associated help files for reference: 
##https://gis.stackexchange.com/questions/265863/how-to-read-a-feature-class-in-an-esri-personal-geodatabase-using-r
##http://rstudio-pubs-static.s3.amazonaws.com/255550_1c983c8a6a5a45a0aee5a923206f4818.html
#http://www.rspatial.org/spatial/rst/7-vectmanip.html#spatial-queries
#https://r-spatial.github.io/sf/articles/sf1.html
#https://github.com/r-spatial/sf/issues/290


## Read in packages and libraries required: 

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

## Set your input geodatabases (this will be where you saved your arcmap exports)
## edit these to your filepath and name of gdb

Dissolved  = "X:/projects/Desktop_Analysis/data/Boreal.gdb" # contains 
Intersect = "X:/projects/Desktop_Analysis/data/scratch1.gdb"
Base  = "X:/projects/Desktop_Analysis/data/Base_data.gdb" # contains boundaries of interest 

## List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
dis_list <- ogrListLayers(Dissolved); print(dis_list)
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

##############################################################################################
# Read in individual Disturbance layers: LINEAR FEATURES ONLY 

# 1) pipeline (length)
          r.pipe <- st_read(dsn=Intersect,layer="R_PIPELINE_SEGMENT_PERMIT_S") # multistring 
          r.pipe = st_transform(r.pipe,3005)
          r.pipe<- st_cast(r.pipe, "LINESTRING")
          r.pipe.int = st_intersection(b.range,r.pipe)   # intersect with ranges
          r.pipe.int$Length_m = st_length(r.pipe.int)    # calculate the length
          
          # RANGE: intersect with range and calculate length per range
          #r.pipe.int <- st_buffer(r.pipe.int,1)  ; all.pipe = sum(st_area(r.pipe)) ; plot(st_geometry(r.pipe))
          #st_is_valid(r.pipe.int)                       # check valid geometry
          #r.pipe.int = st_cast(r.pipe.int,"POLYGON")     # fix overlaps
          ##plot(st_geometry(r.pipe.int))
          #st_write(r.pipe.int,"Dist_R_pipe.shp")       #write out individual dist_layer for Range
          
          r.pipe.int.df = data.frame(r.pipe.int)        # calculate the length per range 
          r.pipe.int.df.out  = r.pipe.int.df %>% 
            mutate(Pipe_length_m = Length_m) %>% 
            group_by(Range) %>% 
            summarise(R_Pipe_length_m = sum(Pipe_length_m))
          
          # CORE: intersect with core and calculate length per range
          c.pipe.int = st_intersection(b.core,r.pipe) # intersect with ranges
          c.pipe.int<- st_cast(c.pipe.int, "LINESTRING")
          c.pipe.int$Length_m = st_length(c.pipe.int)
          
          #c.pipe.int <- st_buffer(c.pipe.int,1)
          #st_is_valid(c.pipe.int)  
          #c.pipe.int = st_cast(c.pipe.int,"POLYGON")
          ##plot(st_geometry(c.pipe.int))
          #st_write(r.pipe.int,"Dist_C_pipe.shp") 
          c.pipe.int.df = data.frame(c.pipe.int)        # calculate the length per core/range 
          c.pipe.int.df.out  = c.pipe.int.df %>% 
            mutate(Pipe_length_m = Length_m) %>% 
            group_by(Range) %>% 
            summarise(C_Pipe_length_m = sum(Pipe_length_m))
          
          out = left_join(r.pipe.int.df.out,c.pipe.int.df.out, all = both)
          out [is.na(out )] <- 0
          out$P_Pipe_length_m = out$R_Pipe_length_m - out$C_Pipe_length_m
          out.pipe = out

# 2) transmission (length  
r.tran.sf <- st_read(dsn=Intersect,layer="BC_trans") # multistring  

          # 1) RANGE: calculate the range extent to use range extent 
          r.tran <- st_intersection(b.range,r.tran.sf)# intersect with ranges
          r.tran = st_transform(r.tran,3005)
          r.tran<- st_cast(r.tran, "LINESTRING")
          
          r.tran$Length_m = st_length(r.tran)
          r.tran.df = data.frame(r.tran)        # calculate the length per range 
          r.tran.df.out  = r.tran.df%>% 
            mutate(Trans_length_m = Length_m) %>% 
            group_by(Range) %>% 
            summarise(R_Trans_length_m = sum(Trans_length_m))
          
          # 2) CORE; intersect with core and calculate length per range 
          c.tran.int = st_intersection(b.core.r,r.tran.sf) # intersect with ranges
          c.tran.int$Length_m = st_length(c.tran.int)
          c.tran.int.df = data.frame(c.tran.int)        # calculate the length per core/range 
          c.tran.int.df.out  = c.tran.int.df %>% 
            mutate(Trans_length_m = Length_m) %>% 
            group_by(Range) %>% 
            summarise(C_Trans_length_m = sum(Trans_length_m))
          
          out.trans = left_join(r.tran.df.out,c.tran.int.df.out, all = both)
          out.trans[is.na(out.trans)] <- 0
          out.trans$P_Trans_length_m = out.trans$R_Trans_length_m - out.trans$C_Trans_length_m
          
          range.layer.cals <- left_join(out.pipe,out.trans) ; range.layer.cals [is.na(range.layer.cals )] <- 0

          ## 3) ALL DISTURBANCE UNION 1
          out = st_union(r.pipe.int,r.tran) ; plot(st_geometry(out))
          out = st_union(out); plot(st_geometry(out),add = T)
          
          #out = st_cast(out,"LINESTRING")
          st_length(out)
          #x.area = sum(st_area(out)) 

# 6) dams 
r.dam.sf <- st_read(dsn=Intersect,layer="R_dams_inte" ) # multipoly
r.dam.sf = st_transform(r.dam.sf ,3005)
          ## RANGE: intersect with range and calculate length per range
          r.dams = st_intersection(b.range,r.dam.sf)   # intersect with ranges
          r.dams <- st_cast( r.dams , "LINESTRING")
          r.dams$Length_m = st_length( r.dams)    # calculate the length
          r.dams.df = data.frame(r.dams)        # calculate the length per range 
          r.dams.df.out  = r.dams.df %>% 
            mutate(Dam_length_m = Length_m) %>% 
            group_by(Range) %>% 
            summarise(R_Dams_length_m = sum(Dam_length_m))
          
          # CORE: intersect with core and calculate length per range
          c.dams = st_intersection(b.core.r,r.dam.sf)   # intersect with ranges
          c.dams$Length_m = st_length(c.dams)    # calculate the length
          c.dams.df = data.frame(c.dams)        # calculate the length per range 
          c.dams.df.out  = c.dams.df %>% 
            mutate(Dam_length_m = Length_m) %>% 
            group_by(Range) %>% 
            summarise(C_Dams_length_m = sum(Dam_length_m))
          
          out.dams = left_join(r.dams.df.out,c.dams.df.out, all = both)
          out.dams[is.na(out.dams)] <- 0
          out.dams$P_Dams_length_m = out.dams$R_Dams_length_m - out.dams$C_Dams_length_m
         
          range.layer.cals <- left_join(range.layer.cals,out.dams) ; range.layer.cals [is.na(range.layer.cals )] <- 0

          # # UNION 5
          out.t = st_union(r.dams)
          out4 = st_union(out,out.t) ; plot(st_geometry(out4))
          out4 = st_union(out4); plot(st_geometry(out4))
          st_length(out4)
          rm(out,out.t)
          
# 11) Rail  
r.rail.sf <- st_read(dsn=Intersect,layer="B_rail_clip") # multipoly
r.rail.sf = st_transform(r.rail.sf ,3005)
r.rail.sf <- st_intersection(b.range,r.rail.sf)
      
      ## UNION 7 # may need to run this in stand alone R rather than R -studio
      out7 = st_union(out4,r.rail.sf) ; plot(st_geometry(out7))
      out7 = st_union(out7); plot(st_geometry(out7))
      st_length(out7)
      rm(out4)
      
      # 1) RANGE: calculate the range extent to use range extent 
      r.rail <- st_intersection(b.range,r.tran.sf)# intersect with ranges
      r.rail$Length_m = st_length(r.rail)
      r.rail.df = data.frame(r.rail)        # calculate the length per range 
      r.rail.df.out  = r.rail.df%>% 
        mutate(Rail_length_m = Length_m) %>% 
        group_by(Range) %>% 
        summarise(R_Rail_length_m = sum(Rail_length_m))
  
      # 2) CORE; intersect with core and calculate length per range 
      c.rail.int = st_intersection(b.core.r,r.tran.sf) # intersect with ranges
      c.rail.int$Length_m = st_length(c.rail.int)
      c.rail.int.df = data.frame(c.rail.int)        # calculate the length per core/range 
      c.rail.int.df.out  = c.rail.int.df %>% 
        mutate(Rail_length_m = Length_m) %>% 
        group_by(Range) %>% 
        summarise(C_Rail_length_m = sum(Rail_length_m))
      
      out.rail= left_join(r.rail.df.out,c.rail.int.df.out, all = both)
      out.rail[is.na(out.rail)] <- 0
      out.rail$P_Rail_length_m = out.rail$R_Rail_length_m - out.rail$C_Rail_length_m
      
      range.layer.cals <- left_join(range.layer.cals,out.rail) ; range.layer.cals [is.na(range.layer.cals )] <- 0
      head(range.layer.cals)
      
# so far we have 
#      - all data in asummary shapefile (out7)
#      - all data in a table (range.layer.cals)       
      
      
# write out the compiled dataset(out7)     
    
out7<- st_cast(out7, "LINESTRING")   
st_length(out7)
#st_write(out7, paste(temp.dir,"Dist_Linear_Ex_road_seis.shp",sep = ""))
      
# due to the size of the seismic and road files. We will run these seperately for each of the herds, then join table data. 
# to calculate the "all disturbance" we will run total disturbance (out7) all herds then add road (by herd) and seismic (by herd). 
# to reduce the time in processing
      
#########################################      
# read in Roads linear features 

# for these you will need to run script 1 (this will export the range extent for the roads. 
# within Arcmap convert this from polygon to line for example "Temp_roads_cal_line.
# then read these in (these can be done at the widest range ie boundary)

# note: 
# ground checked the linear features to ensure the length doesnt count on double lines

# Calander 
b.road1 = sf::st_read(dsn = Intersect , layer ="Temp_roads_cal_line" ) # multi surface
b.road1 = st_transform(b.road1,3005)
    # range
    b.road1 <- st_intersection(b.range,b.road1)# intersect with ranges
        # create a dissolved version to add to the "all disturbance layer" 
        #b.road.u <- st_union(b.road1) 
        #head(b.road.u)
    b.road1 <- st_cast(b.road1, "LINESTRING")
    b.road1$Length_m = st_length(b.road1)
    #st_is_valid(b.road1)
    b.road1.df = data.frame(b.road1)        # calculate the length per range 
    b.road1.df.out  = b.road1.df%>% 
      mutate(Road_length_m = Length_m) %>% 
      group_by(Range) %>% 
      summarise(R_Road_length_m = sum(Road_length_m))
# 2) CORE; intersect with core and calculate length per range 
    b.road1.c <- st_intersection(b.core.r,b.road1)# intersect with ranges
    b.road1.c <- st_cast(b.road1.c, "LINESTRING")
    b.road1.c.df = data.frame(b.road1.c)        # calculate the length per range 
    b.road1.c.df.out  = b.road1.c.df%>% 
      mutate(Road_length_m = Length_m) %>% 
      group_by(Range) %>% 
      summarise(C_Road_length_m = sum(Road_length_m))
out.road= left_join(b.road1.df.out,b.road1.c.df.out, all = both)
out.road[is.na(out.road)] <- 0
out.road$P_Road_length_m = out.road$R_Road_length_m - out.road$C_Road_length_m

# Maxhamish 
b.road1 = sf::st_read(dsn = Intersect , layer ="Temp_roads_max_line" ) # multi surface
b.road1 = st_transform(b.road1,3005)
    # range
    b.road1 <- st_intersection(b.range,b.road1)# intersect with ranges
    
      # create a dissolved version to add to the "all disturbance layer" 
      #b.road.u2 <- st_union(b.road1)
      #st_length(b.road.u); st_length(b.road.u2)
      #b.road.u.out <- st_union(b.road.u,b.road.u2)
      #b.road.u.out <- st_union(b.road.u.out)
      #head(b.road.u.out)
      #st_length(b.road.u.out)
      #rm(b.road.u.out)
      
    b.road1 <- st_cast(b.road1, "LINESTRING")
    b.road1$Length_m = st_length(b.road1)
    #st_is_valid(b.road1)
    b.road1.df = data.frame(b.road1)        # calculate the length per range 
    b.road1.df.out  = b.road1.df%>% 
      mutate(Road_length_m = Length_m) %>% 
      group_by(Range) %>% 
      summarise(R_Road_length_m = sum(Road_length_m))
    # 2) CORE; intersect with core and calculate length per range 
    b.road1.c <- st_intersection(b.core.r,b.road1)# intersect with ranges
    b.road1.c <- st_cast(b.road1.c, "LINESTRING")
    b.road1.c.df = data.frame(b.road1.c)        # calculate the length per range 
    b.road1.c.df.out  = b.road1.c.df%>% 
      mutate(Road_length_m = Length_m) %>% 
      group_by(Range) %>% 
      summarise(C_Road_length_m = sum(Road_length_m))

  out.road1= left_join(b.road1.df.out,b.road1.c.df.out, all = both)
  out.road1[is.na(out.road1)] <- 0
  out.road1$P_Road_length_m = out.road1$R_Road_length_m - out.road1$C_Road_length_m
  # add to other road data: 
  all.out = rbind(out.road,out.road1)

# West 
b.road1 = sf::st_read(dsn = Intersect , layer ="Temp_roads_west_line" ) # multi surface
b.road1 = st_transform(b.road1,3005)
    # range
    b.road1 <- st_intersection(b.range,b.road1)# intersect with ranges
        
      #b.road.u3 <- st_union(b.road1)
    
    b.road1 <- st_cast(b.road1, "LINESTRING")
    b.road1$Length_m = st_length(b.road1)
    #st_is_valid(b.road1)
    b.road1.df = data.frame(b.road1)        # calculate the length per range 
    b.road1.df.out  = b.road1.df%>% 
      mutate(Road_length_m = Length_m) %>% 
      group_by(Range) %>% 
      summarise(R_Road_length_m = sum(Road_length_m))
    # 2) CORE; intersect with core and calculate length per range 
    b.road1.c <- st_intersection(b.core.r,b.road1)# intersect with ranges
    b.road1.c <- st_cast(b.road1.c, "LINESTRING")
    b.road1.c.df = data.frame(b.road1.c)        # calculate the length per range 
    b.road1.c.df.out  = b.road1.c.df%>% 
      mutate(Road_length_m = Length_m) %>% 
      group_by(Range) %>% 
      summarise(C_Road_length_m = sum(Road_length_m))
    out.road1= left_join(b.road1.df.out,b.road1.c.df.out, all = both)
    out.road1[is.na(out.road1)] <- 0
    out.road1$P_Road_length_m = out.road1$R_Road_length_m - out.road1$C_Road_length_m
    # add to other road data: 
    all.out = rbind(all.out,out.road1)

# Snake
    b.road1 = sf::st_read(dsn = Intersect , layer ="Temp_roads_snake_line" ) # multi surface
    b.road1 = st_transform(b.road1,3005)
    # range
    b.road1 <- st_intersection(b.range,b.road1)# intersect with ranges
    
      #b.road.u4 <- st_union(b.road1)
    
    b.road1 <- st_cast(b.road1, "LINESTRING")
    b.road1$Length_m = st_length(b.road1)
    #st_is_valid(b.road1)
    b.road1.df = data.frame(b.road1)        # calculate the length per range 
    b.road1.df.out  = b.road1.df%>% 
      mutate(Road_length_m = Length_m) %>% 
      group_by(Range) %>% 
      summarise(R_Road_length_m = sum(Road_length_m))
    # 2) CORE; intersect with core and calculate length per range 
    b.road1.c <- st_intersection(b.core.r,b.road1)# intersect with ranges
    b.road1.c <- st_cast(b.road1.c, "LINESTRING")
    b.road1.c.df = data.frame(b.road1.c)        # calculate the length per range 
    b.road1.c.df.out  = b.road1.c.df%>% 
      mutate(Road_length_m = Length_m) %>% 
      group_by(Range) %>% 
      summarise(C_Road_length_m = sum(Road_length_m))
    out.road1= left_join(b.road1.df.out,b.road1.c.df.out, all = both)
    out.road1[is.na(out.road1)] <- 0
    out.road1$P_Road_length_m = out.road1$R_Road_length_m - out.road1$C_Road_length_m
    # add to other road data: 
    all.out = rbind(all.out,out.road1)

# chinchaga
b.road1 = sf::st_read(dsn = Intersect , layer ="Temp_roads_chin_line" ) # multi surface
b.road1 = st_transform(b.road1,3005)
    # range
    b.road1 <- st_intersection(b.range,b.road1)# intersect with ranges
     # create a dissolved version for the "all disturbance"
      #b.road.u5 <- st_union(b.road1)
    
      b.road1 <- st_cast(b.road1, "LINESTRING")
    b.road1$Length_m = st_length(b.road1)
    #st_is_valid(b.road1)
    b.road1.df = data.frame(b.road1)        # calculate the length per range 
    b.road1.df.out  = b.road1.df%>% 
      mutate(Road_length_m = Length_m) %>% 
      group_by(Range) %>% 
      summarise(R_Road_length_m = sum(Road_length_m))
    # 2) CORE; intersect with core and calculate length per range 
    b.road1.c <- st_intersection(b.core.r,b.road1)# intersect with ranges
    b.road1.c <- st_cast(b.road1.c, "LINESTRING")
    b.road1.c.df = data.frame(b.road1.c)        # calculate the length per range 
    b.road1.c.df.out  = b.road1.c.df%>% 
      mutate(Road_length_m = Length_m) %>% 
      group_by(Range) %>% 
      summarise(C_Road_length_m = sum(Road_length_m))
    out.road1= left_join(b.road1.df.out,b.road1.c.df.out, all = both)
    out.road1[is.na(out.road1)] <- 0
    out.road1$P_Road_length_m = out.road1$R_Road_length_m - out.road1$C_Road_length_m
    # add to other road data: 
    all.out = rbind(all.out,out.road1)
    all.out = all.out[-c(3,5),]
    
# add the roads layer to all other layers 
range.layer.cals <- left_join(all.out,range.layer.cals) ; range.layer.cals [is.na(range.layer.cals )] <- 0

write.csv(range.layer.cals,paste(temp.dir,"Linear_disturbance_RPC.csv"))

# uo to here we have 
# all disturbance (out7) + b.road.u - b.road.u5 (one per herd)

## Seismic lines 
## After months of trying to get this to run (slow GTS/incorrect geometry for the arcmap outputs )
## Unable to get this to work sufficiently so will need to do this independently in ArcMap for each herd due to the size of the files. 
## Went back to original data sets and clipped tot area of range, then merged the files and these will be read in below: 

## Herd 1) 
## maxhamish  
b.s1 = sf::st_read(dsn = Intersect , layer ="R_Seis_Max_4") 
b.s1  = st_transform(b.s1,3005)
    # 1) range
    b.road1 <- st_intersection(b.range,b.s1)# intersect with ranges
    
    #create a dissolved version for the "all disturbance layer"
    #b.seis.u <- st_union(b.road1)
    
    b.road1 <- st_cast(b.road1, "LINESTRING")
    b.road1$Length_m = st_length(b.road1)
    b.road1.df = data.frame(b.road1)        # calculate the length per range 
    b.road1.df.out  = b.road1.df%>% 
      mutate(Road_length_m = Length_m) %>% 
      group_by(Range) %>% 
      summarise(R_Seis_length_m = sum(Road_length_m))

    # 2) CORE; intersect with core and calculate length per range 
    b.road1.c <- st_intersection(b.core.r,b.s1)# intersect with ranges
    b.road1.c <- st_cast(b.road1.c, "LINESTRING")
    b.road1.c$Length_m = st_length(b.road1.c)
    b.road1.c.df = data.frame(b.road1.c)        # calculate the length per range 
    b.road1.c.df.out  = b.road1.c.df%>% 
      mutate(Road_length_m = Length_m) %>% 
      group_by(Range) %>% 
      summarise(C_Seis_length_m = sum(Road_length_m))
    
    out.road1= left_join(b.road1.df.out,b.road1.c.df.out, all = both)
    out.road1[is.na(out.road1)] <- 0
    out.road1$P_Seis_length_m = out.road1$R_Seis_length_m - out.road1$C_Seis_length_m
    all.out = out.road1
    all.out = all.out[1,]
    
## herd 2)  ## Snake 
b.s1 = sf::st_read(dsn = Intersect , layer ="Snake_Seis_misc_Merge") 
b.s1  = st_transform(b.s1 ,3005)
    # 1) range
    b.road1 <- st_intersection(b.range,b.s1) # intersect with ranges # this takes a long time run outside r-studio if possible. 
        #create a dissolved version for the "all disturbance layer"
        #b.seis.u2 <- st_union(b.road1)
    b.road1 <- st_cast(b.road1, "LINESTRING")
    b.road1$Length_m = st_length(b.road1)
    b.road1.df = data.frame(b.road1)        # calculate the length per range 
    b.road1.df.out  = b.road1.df%>% 
      mutate(Road_length_m = Length_m) %>% 
      group_by(Range) %>% 
      summarise(R_Seis_length_m = sum(Road_length_m))
    
    # 2) CORE; intersect with core and calculate length per range 
    b.road1.c <- st_intersection(b.core.r,b.s1)# intersect with ranges
    b.road1.c <- st_cast(b.road1.c, "LINESTRING")
    b.road1.c$Length_m = st_length(b.road1.c)
    b.road1.c.df = data.frame(b.road1.c)        # calculate the length per range 
    b.road1.c.df.out  = b.road1.c.df%>% 
      mutate(Road_length_m = Length_m) %>% 
      group_by(Range) %>% 
      summarise(C_Seis_length_m = sum(Road_length_m))
    
    out.road1= left_join(b.road1.df.out,b.road1.c.df.out, all = both)
    out.road1[is.na(out.road1)] <- 0
    out.road1$P_Seis_length_m = out.road1$R_Seis_length_m - out.road1$C_Seis_length_m
    all.out= rbind(all.out,out.road1)
    all.out = all.out[-c(2,3),]

    #x = all.out
    
## herd 3)  ##Cal 
    b.s1 = sf::st_read(dsn = Intersect , layer ="Cal_Seis_misc_Merge") 
    b.s1  = st_transform(b.s1 ,3005)
    # 1) range
    b.road1 <- st_intersection(b.range,b.s1) # intersect with ranges # this takes a long time run outside r-studio if possible. 
    #create a dissolved version for the "all disturbance layer"
    b.road1 <- st_cast(b.road1, "LINESTRING")
    b.road1$Length_m = st_length(b.road1)
    b.road1.df = data.frame(b.road1)        # calculate the length per range 
    b.road1.df.out  = b.road1.df%>% 
      mutate(Road_length_m = Length_m) %>% 
      group_by(Range) %>% 
      summarise(R_Seis_length_m = sum(Road_length_m))
    
    # 2) CORE; intersect with core and calculate length per range 
    b.road1.c <- st_intersection(b.core.r,b.s1)# intersect with ranges
    b.road1.c <- st_cast(b.road1.c, "LINESTRING")
    b.road1.c$Length_m = st_length(b.road1.c)
    b.road1.c.df = data.frame(b.road1.c)        # calculate the length per range 
    b.road1.c.df.out  = b.road1.c.df%>% 
      mutate(Road_length_m = Length_m) %>% 
      group_by(Range) %>% 
      summarise(C_Seis_length_m = sum(Road_length_m))
    
    out.road1= left_join(b.road1.df.out,b.road1.c.df.out, all = both)
    out.road1[is.na(out.road1)] <- 0
    out.road1$P_Seis_length_m = out.road1$R_Seis_length_m - out.road1$C_Seis_length_m
    out.road1 = out.road1[1,]
  
    # add to other road data: 
    all.out = rbind(all.out,out.road1)
    #x = all.out 

## herd 4) # west 
    b.s1 = sf::st_read(dsn = Intersect , layer ="West_Seis_misc_Merge")  
    b.s1  = st_transform(b.s1 ,3005)
    # 1) range
    b.road1 <- st_intersection(b.range,b.s1) # intersect with ranges # this takes a long time run outside r-studio if possible. 
    #create a dissolved version for the "all disturbance layer"
    b.road1 <- st_cast(b.road1, "LINESTRING")
    b.road1$Length_m = st_length(b.road1)
    b.road1.df = data.frame(b.road1)        # calculate the length per range 
    b.road1.df.out  = b.road1.df%>% 
      mutate(Road_length_m = Length_m) %>% 
      group_by(Range) %>% 
      summarise(R_Seis_length_m = sum(Road_length_m))
    
    # 2) CORE; intersect with core and calculate length per range 
    b.road1.c <- st_intersection(b.core.r,b.s1)# intersect with ranges
    b.road1.c <- st_cast(b.road1.c, "LINESTRING")
    b.road1.c$Length_m = st_length(b.road1.c)
    b.road1.c.df = data.frame(b.road1.c)        # calculate the length per range 
    b.road1.c.df.out  = b.road1.c.df%>% 
      mutate(Road_length_m = Length_m) %>% 
      group_by(Range) %>% 
      summarise(C_Seis_length_m = sum(Road_length_m))
    
    out.road1= left_join(b.road1.df.out,b.road1.c.df.out, all = both)
    out.road1[is.na(out.road1)] <- 0
    out.road1$P_Seis_length_m = out.road1$R_Seis_length_m - out.road1$C_Seis_length_m
    
    #out.road1 = out.road1[,]
    # add to other road data: 
    all.out = rbind(all.out,out.road1)
    x = all.out

    ## herd 5) # Chin 
    b.s1 = sf::st_read(dsn = Intersect , layer ="Chin_Seis_misc_Merge") 
    b.s1  = st_transform(b.s1 ,3005)
    # 1) range
    b.road1 <- st_intersection(b.range,b.s1) # intersect with ranges # this takes a long time run outside r-studio if possible. 
    #create a dissolved version for the "all disturbance layer"
    b.road1 <- st_cast(b.road1, "LINESTRING")
    b.road1$Length_m = st_length(b.road1)
    b.road1.df = data.frame(b.road1)        # calculate the length per range 
    b.road1.df.out  = b.road1.df%>% 
      mutate(Road_length_m = Length_m) %>% 
      group_by(Range) %>% 
      summarise(R_Seis_length_m = sum(Road_length_m))
    
    # 2) CORE; intersect with core and calculate length per range 
    b.road1.c <- st_intersection(b.core.r,b.s1)# intersect with ranges
    b.road1.c <- st_cast(b.road1.c, "LINESTRING")
    b.road1.c$Length_m = st_length(b.road1.c)
    b.road1.c.df = data.frame(b.road1.c)        # calculate the length per range 
    b.road1.c.df.out  = b.road1.c.df%>% 
      mutate(Road_length_m = Length_m) %>% 
      group_by(Range) %>% 
      summarise(C_Seis_length_m = sum(Road_length_m))
    
    out.road1= left_join(b.road1.df.out,b.road1.c.df.out, all = both)
    out.road1[is.na(out.road1)] <- 0
    out.road1$P_Seis_length_m = out.road1$R_Seis_length_m - out.road1$C_Seis_length_m
    
    # add to other road data: 
    all.out = rbind(all.out,out.road1)
    
    
# add the roads layer to all other layers 

range.layer.cals <- left_join(all.out,range.layer.cals) ; range.layer.cals [is.na(range.layer.cals )] <- 0
write.csv(range.layer.cals,paste(temp.dir,"Linear_disturbance_RPC.csv"))


## make memory room!
#rm('out1','out2','out3','out4','out5','out6','out7','r.agr.sf','r.air.sf','r.dam.sf','r.mine.sf')
#rm('r.pipe','r.rail.sf','r.rec.sf','r.tran','r.tran.sf','r.urban.sf','r.wells.sf')
#rm('b.r.c0.80')

### To calculate the total linear features ArcMap was used to merge the output (out7) with the road and seismic layers (the same as those read in)
### The layers are merged, then dissolved to calculate a total (non-overlapping ) length 
### these are then output into a csv files "Total_linear_dis_edit.csv" 

# final 
#range.layer.cals 
#Herd_key


#################################################################################################
## Create length and density tables for Core, Periphery and Range for disturbance linear features
#################################################################################################

total.dis = read.csv(paste(temp.dir,"Total_linear_dis_edit.csv",sep = '')) 

l.range = read.csv(paste(temp.dir," Linear_disturbance_RPC.csv",sep = '')) 
## or this if you are running the top part of script in the same session 
#l.range  = range.layer.cals

all.out = left_join(l.range,total.dis)

# convert Ha to kms2
Herd_key_km <- Herd_key %>% 
  mutate(R_area_km2 = R_area_ha/100, P_area_km2 = P_area_ha/100,C_area_km2 = C_area_ha/100) %>% 
  dplyr::select(-c(R_area_ha,P_area_ha,C_area_ha))

all.out <- left_join(Herd_key_km,all.out)
all.out[is.na(all.out)] <- 0


all.outl = reshape2::melt(all.out)
all.outl<- all.outl %>% 
    mutate(Dist_type = gsub(".*R_|_length.*","", variable)) %>%
    mutate(Dist_type = gsub("C_","",Dist_type)) %>%
    mutate(Dist_type = gsub("P_","",Dist_type)) %>%
    mutate(Boundary = substring(variable,1,1)) 
   
  lr = all.outl  %>% gather(key = variable, value = value, 3) 

# output density tables
herds = c('Calendar','Chinchaga','Maxhamish','Snake-Sahtahneh','Westside Fort Nelson')

for (i in herds) { 
  # write out core area details. 
  t.l = lr %>% filter(Range == paste(i))
    # get the total areas (km2 to calculate density)
    t.r = t.l %>% filter(Boundary =='R' & Dist_type == "area_km2") %>%  dplyr::select(value)
    t.p = t.l %>% filter(Boundary =='P' & Dist_type == "area_km2") %>%  dplyr::select(value)
    t.c = t.l %>% filter(Boundary =='C' & Dist_type == "area_km2") %>%  dplyr::select(value)
  # add columns to the totals for RPC  
  t.l <- t.l %>% mutate(Total_area_km2 = ifelse(Boundary =='R',t.r,
                                     ifelse(Boundary =='P',t.p,
                                     ifelse(Boundary == 'C',t.c,0))))
  # calculate the length (km) and density (km2) 
  t.l <- t.l %>% 
    mutate(Length_km = value/1000) %>% 
    mutate(Density_km_km2 = Length_km / as.numeric(Total_area_km2)) %>%
    dplyr::select(-c(variable, value)) 
 
   # this creates a table with the Total areas (Total_area_km2) for eqach of the range/core/periphery
  #t.total <- t.l %>% 
  #  dplyr::filter(Dist_type == "Road") %>%
  #  dplyr::select(-c(Length_km,Density_km_km2,Dist_type)) %>%  
  #  spread(key = Boundary,value = Total_area_km2) 

   # get rid of the area_total rows and random X row
  t.l = t.l[-(grep("area_km2",t.l$Dist_type)),]
  t.l = t.l[-(grep("X",t.l$Dist_type)),]
  t.l = t.l %>% dplyr::select(-c(Total_area_km2))
  
  t.length <- t.l %>% 
  dplyr::select(-c(Density_km_km2)) %>%  
  spread(.,key = Boundary,value = Length_km)
  
  write.csv(t.length, paste(out.dir,i,"_Disturb_RCP_length.csv",sep = ''))
  
  t.density = t.l %>% 
    dplyr::select(-c(Length_km)) %>%  
    spread(.,key = Boundary,value = Density_km_km2)
    
  # write out the csv   
  write.csv(t.density, paste(out.dir,i,"_Disturb_RCP_density.csv",sep = ''))
  
} 
  
  
### Ammalgamate the length and density files and calculate a % of each of the component in excel ### 

 
  
  
  
  
  
  
  
  
  
  
  
 


 
  
  
##################### OLD SCRIPTING  ##########################################  
  

 ## OLD PLOT INFO  
  p = ggplot(t.l, aes(Decade,value)) + try(facet_wrap(~var_name))+ geom_bar(stat = 'identity') 
  p = p + ylab("Percentage") + xlab("Decade") + ggtitle(paste("Percent Cutblock (per area) from 1958 - 2017 (",i," herd)",sep = "" ))
  p = p + theme_bw()
  ggsave(paste(out.dir,i,"_cutblock_temp1.png",sep = ""))  
  
  
  
  
  p1 = ggplot() + geom_sf(data = merged.all, aes(fill = range.all)) + ggtitle("Range") + theme_bw()
  p1  = p1 + guides(fill=guide_legend(title="Density (km/km2"))
  
  p2 = ggplot() + geom_sf(data = merged.all, aes(fill = per.all)) + ggtitle("Periphery") + theme_bw()
  p2  = p2 + guides(fill=guide_legend(title="Density (km/km2"))
  
  p3 = ggplot() + geom_sf(data = merged.all, aes(fill = core.all)) + ggtitle("Core") + theme_bw()
  p3  = p3 + guides(fill=guide_legend(title="Density (km/km2"))
  
  
  
  
  
  

  
  if(length(t.c.c$Range)>0){
    # plot 1
    p = ggplot(t.c.c, aes(Decade,value)) + try(facet_wrap(~var_name))+ geom_bar(stat = 'identity') 
    p = p + ylab("Percentage") + xlab("Decade") + ggtitle(paste("Percent Cutblock (per area) from 1958 - 2017 (",i," herd)",sep = "" ))
    p = p + theme_bw()
    ggsave(paste(out.dir,i,"_cutblock_temp1.png",sep = ""))  
    # plot 2
    p1 = ggplot(t.c.c, aes(var_name,value)) + facet_wrap(~Decade)+ geom_bar(stat = 'identity') 
    p1 = p1 + ylab("Percentage") + xlab("") + ggtitle(paste("Percent Cutblock (per area) from 1958 - 2017 (",i," herd)",sep = "" ))
    p1 = p1 + theme_bw()
    ggsave(paste(out.dir,i,"_cutblock_temp2.png",sep = ""))  
    # plot 3
    p2 = ggplot(t.c.group, aes(var_name,sum)) + facet_wrap(~Age_group)+ geom_bar(stat = 'identity') 
    p2 = p2 + ylab("Percentage") + xlab("") + ggtitle(paste("Percent Cutblock (per area) for harvest (0-40 years) and 41-80 years(",i," herd)",sep = "" ))
    p2 = p2 + theme_bw()
  } 
} 

l.range = read.csv(paste(out.dir,"Boreal_range_disturb_length_km.csv",sep = ''))
#l.range$brdy = 'range'; 
l.range = l.range %>% dplyr::select(-(X))
l.rangel = reshape2::melt(l.range)
l.rangel$range = l.rangel$value
l.core = read.csv(paste(out.dir,"Disturbance_length_core_by_ranges.csv",sep = ""))
l.core = l.core %>% dplyr::select(-(X))
l.core = l.core %>% mutate(Calendar = Calendar/1000)  %>% 
  mutate(Chinchaga = Chinchaga/1000) %>% 
  mutate(Maxhamish = Maxhamish/1000) %>%
  mutate(Snake.Sahtahneh = Snake.Sahtahneh/1000)  %>% 
  mutate(Westside.Fort.Nelson = Westside.Fort.Nelson/1000)
l.corel = reshape2::melt(l.core)
l.corel$core = l.corel$value
out = merge(l.rangel,l.corel, by = c("Type",'variable'))
out = out %>% dplyr::select(-c(value.x,value.y))

out = out %>% dplyr::filter(!(grepl(pattern = 'Dams',Type)))
out$peri = out$range-out$core

out.l = left_join(out,Herd_key, by = c('variable' = 'Range'))
out.l = out.l %>% 
  mutate(range.l = range/ (R_area_ha/100))%>%
  mutate(per.l = peri/ (P_area_ha/100))%>%
  mutate(core.l = core/ (C_area_ha/100))

out.l = out.l %>% dplyr::select(-c(R_area_ha,C_area_ha,P_area_ha))

write.csv(out.l, paste(out.dir,"Disturb_RCP_length_density.csv",sep = ''))





    mutate(Disturbance_km = ifelse(grep("_ha",variable),variable/100,variable*100))
           
# create a table with disturbance length (km) , density (km/km2) and 


# table with total linear 

l.range = l.range %>% 
  mutate(R_Pipe_length_km = R_Pipe_length_m*1000)  %>% 
  mutate(C_Pipe_length_km = C_Pipe_length_m *1000) %>% 
  mutate(Maxhamish = Maxhamish/1000) %>%
  mutate(Snake.Sahtahneh = Snake.Sahtahneh/1000)  %>% 
  mutate(Westside.Fort.Nelson = Westside.Fort.Nelson/1000)

# convert to km/km2  = 1 km2 = 100 ha 
l.rangel = reshape2::melt(l.range)

l.rangel$range = l.rangel$value
l.core = read.csv(paste(out.dir,"Disturbance_length_core_by_ranges.csv",sep = ""))
l.core = l.core %>% dplyr::select(-(X))
l.core = l.core %>% mutate(Calendar = Calendar/1000)  %>% 
  mutate(Chinchaga = Chinchaga/1000) %>% 
  mutate(Maxhamish = Maxhamish/1000) %>%
  mutate(Snake.Sahtahneh = Snake.Sahtahneh/1000)  %>% 
  mutate(Westside.Fort.Nelson = Westside.Fort.Nelson/1000)
l.corel = reshape2::melt(l.core)
l.corel$core = l.corel$value
out = merge(l.rangel,l.corel, by = c("Type",'variable'))
out = out %>% dplyr::select(-c(value.x,value.y))

out = out %>% dplyr::filter(!(grepl(pattern = 'Dams',Type)))
out$peri = out$range-out$core

out.l = left_join(out,Herd_key, by = c('variable' = 'Range'))
out.l = out.l %>% 
  mutate(range.l = range/ (R_area_ha/100))%>%
  mutate(per.l = peri/ (P_area_ha/100))%>%
  mutate(core.l = core/ (C_area_ha/100))

out.l = out.l %>% dplyr::select(-c(R_area_ha,C_area_ha,P_area_ha))

write.csv(out.l, paste(out.dir,"Disturb_RCP_length_density.csv",sep = ''))


##########################################################################
# Join the data to spatial data and map by density 

out.d = out.l %>% dplyr::select(-c(range,core,peri))
out.d$Range = out.d$variable 
out.d$Range = gsub('Snake.Sahtahneh','Snake-Sahtahneh', out.d$Range) 
out.d$Range = gsub('Westside.Fort.Nelson','Westside Fort Nelson', out.d$Range) 

merged = merge(b.range.sf,out.d, by = "Range")
merged <- st_as_sf(merged)

merged.all <- merged %>% group_by(Range) %>% summarise(range.all = sum(range.l), per.all = sum(per.l), core.all = sum(core.l))


p1 = ggplot() + geom_sf(data = merged.all, aes(fill = range.all)) + ggtitle("Range") + theme_bw()
p1  = p1 + guides(fill=guide_legend(title="Density (km/km2"))

p2 = ggplot() + geom_sf(data = merged.all, aes(fill = per.all)) + ggtitle("Periphery") + theme_bw()
p2  = p2 + guides(fill=guide_legend(title="Density (km/km2"))

p3 = ggplot() + geom_sf(data = merged.all, aes(fill = core.all)) + ggtitle("Core") + theme_bw()
p3  = p3 + guides(fill=guide_legend(title="Density (km/km2"))


p <- cowplot::plot_grid(p1,p2,p3, ncol = 3)

ggsave(paste(out.dir,"Linear_density_RPC.png",sep = ""),p)










