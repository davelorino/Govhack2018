#CONSOLIDATE DATA

#This file consolidates census data. The census data is sourced from the 2016 census
# https://datapacks.censusdata.abs.gov.au/datapacks/

# This code consolidates:
#  GCP data
#  WPP data
#into one consolidated file called data_gcp_and_wpp.csv
#
# Note: The code references a site subfolder called "data_all" which is not
# on Github, as there are too many fricking megabytes. But it you can run this
# code by downloading your own census WPP and GCP census data from the datapacks
# site.
#I've also combined them into one large consolidated file called data_all.csv

# 0) PREPARE ENVIRONMENT --------------------------------------------------
    #Clear environment
    rm( list = ls() ) #Clear global environment
    if ( dev.cur()[[1]] != 1 ) { dev.off() } #Clear plots
    gc() #Clear memory
    cat("\014") #Clear console
    
    #Load libraries
    library(tidyverse) # Essential. All Hadley Wickham's libraries.
    library(here) # Share your project without messing up the file system!
    library(rgdal) # Shape file library
    #Set working directory. Oh wait! No need! We're using library(here). Hurrah!

#A) GET CONSOLIDATED DATA ------------------------------------------------------
are_we_consolidated <- 1    

if (are_we_consolidated != 1) {
    
# A1 Get Census Data ------------------------------------------------------
    
    
    #A 1.1 Get GCP data ####
    
        #Determine path
        gcp_path <- here("data_all"
                         , "2016_GCP_ALL_for_AUS_short-header"
                         , "2016 Census GCP All Geographies for AUST"
                         , "SA2"
                         , "AUST"
                         )
    
        #Make a list of .csv files in that path
        gcp_files <- paste0(gcp_path
                            , "/"
                            , dir(path = gcp_path, pattern = "*.csv")
                            )
        
        #Open each file, and consolidate the data into one data frame
        gcp_data <- gcp_files %>%
            map(read_csv) %>%
            reduce(left_join, by = "SA2_MAINCODE_2016")
        
        #Change the short header column names to G numbers. 
        # (That is, each column name should be "G" followed by a number, 
        # like "G47" or "G13571".)
        # (The reason this is a good idea is because the existing short column 
        #  names are not unique, but G numbers would be).
        names(gcp_data) <- c("SA2_MAINCODE_2016", paste0("G", 1:15535))
        
        #Clean up
        rm(list = c("gcp_files", "gcp_path"))

    #A 1.2 Get WPP data ####

        #Determine path
        wpp_path <- here("data_all"
                         , "2016_WPP_ALL_for_AUS_short-header"
                         , "2016 Census WPP All Geographies for AUST"
                         , "SA2"
                         , "AUST"
                         )
        
        #Make a list of .csv files in that path
        wpp_files <- paste0(wpp_path
                            , "/"
                            , dir(path = wpp_path, pattern = "*.csv")
                            )
        
        #Open each file and conslidate the data into one data frame
        wpp_data <- wpp_files %>%
            map(read_csv) %>%
            reduce(left_join, by = "SA2_MAINCODE_2016")
        
        #Change the short header column names to W numbers
        names(wpp_data) <- c("SA2_MAINCODE_2016", paste0("W", 1:13589))
        
        #Clean up
        rm(list = c("wpp_files", "wpp_path"))
        

#A2 Consolidate ----------------------------------------------------------

    #Join the two big census tables into one huge census table
    data_all <- left_join(x = gcp_data
                          , y = wpp_data
                          , by.x = "SA2_MAINCODE_2016"
                          , by.y = "SA2_MAINCODE_2016"
                          )
        
    #Clean up
    gc()
    

# A3 Save -----------------------------------------------------------------

    #A 3.1 Add metadata ####
        #Create a consolidated folder
        dir.create(here("data"
                        , "consolidated"
                        ))
    
        #Create a metadata folder
        dir.create(here("data"
                        , "consolidated"
                        , "metadata"
                        ))
        #Put the metadata into that folder
        file.copy(here("data_all"
                       , "2016_GCP_ALL_for_AUS_short-header"
                       , "Metadata"
                       , "2016_GCP_Sequential_Template.xlsx"
                       )
                  , here("data"
                         , "consolidated"
                         , "metadata"
                         , "2016_GCP_Sequential_Template.xlsx"
                         )
                  )
        file.copy(here("data_all"
                       , "2016_WPP_ALL_for_AUS_short-header"
                       , "Metadata"
                       , "2016_WPP_Sequential_Template.xlsx"
                       )
                  , here("data"
                         , "consolidated"
                         , "metadata"
                         , "2016_WPP_Sequential_Template.xlsx"
                         )
                  )
    
    
    # A 3.2 Save the census data ####
    write_csv(x = data_all
              , path = here("data"
                            , "consolidated"
                            , "data_gcp_and_wpp.csv"
                            )
              )
} else {
    data_all <- read_csv(here("data"
                              , "consolidated"
                              , "data_gcp_and_wpp.csv"
                              )
                         )
}


# B) CLEAN DATA -----------------------------------------------------------

    #B1 JUST SHOW IMPORTANT STUFF
    data_income <- data_all %>%
        select(SA2_MAINCODE_2016, G110, G112, G115) %>%
        mutate(  income = G115 * 52
                , mortgage = G110 * 12
                , rent = G112 * 52
                , house_price = 1000000
                ) %>%
        select(SA2_MAINCODE_2016, income, mortgage, rent, house_price) 

# C) WRITE THE CLEAN DATA TO DISK ----------------------------------------------
    write_csv(x = data_income
              , path = here("data", "consolidated", "data_income.csv")
              )
    
# D) ADD SHAPES ----------------------------------------------------------------
    shapes_aust <- rgdal::readOGR(dsn = here("data"
                                             , "shapefiles"
                                             , "SA2_2016_AUST.shp"
                                             )
                                  )
    # Merge GCP census data with ShapeFiles ####
    oz_merged <- sp::merge(shapes_aust #Shapefiles
                           , data_income #Clean data
                           , by.x = "SA2_MAIN16" #SA2 code in data table
                           , by.y = "SA2_MAINCODE_2016"
    )   
    write_rds(x = oz_merged, path = here("data", "consolidated", "oz_merged.rds"))
    
# E) PREPARE SA2 SUMMARY -------------------------------------------------------
    # The SA2 Summary is supposed to just contain the data of data_income, but 
    # also have the first 12 columns of oz_merged
    sa2_summary <- oz_merged@data
    glimpse(sa2_summary)    
    write_csv(x = sa2_summary, path = here("data", "consolidated", "sa2_summary.csv"))

# F) SENSE-CHECK DATA ----------------------------------------------------------
    data <- 