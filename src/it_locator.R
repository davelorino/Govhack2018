# IT LOCATOR

# This file locates where people in the IT industry live. It is based on WPP
# census data.


# 0) PREPARE ENVIRONMENT --------------------------------------------------

    #Clear environment
    rm(list = ls() )
    if ( dev.cur()[[1]] != 1 ) { dev.off() }
    gc()
    cat("\014")    

    #Load libraries
    library(tidyverse) 
    library(here)   # Easier navigation
    library(maptools) #Creates Spatial Polygons Data Frames
    library(sp) #Merges maps
    
    setwd("/Users/durandsinclair/Documents/Training/MDSI/Subjects/DADM/IT_WPP")
    getwd()
    
# 1) GET DATA -------------------------------------------------------------

    #Get WPP census data ----
        #(that is, data about the working population)
    data_gcp_and_wpp <- read_csv(here("data", "data_gcp_and_wpp.csv"))    
    data_gcp_and_wpp <- read_csv(paste0(getwd(), "/data/data_gcp_and_wpp.csv"))
    # Get PEP data ------
    if ( !exists( here( "data", "pep_data.csv" ))) {
        
        #Determine path
        pep_path <- here("data"
                         , "2016_PEP_SA2_for_NSW_short-header"
                         , "2016 Census PEP Statistical Area 2 for NSW"
                         )
        #Make a list of .csv files in that path
        pep_files <- paste0(pep_path
                            , "/"
                            , dir(path = pep_path, pattern = "*.csv")
                            )
        #Open each file and consolidate into one dataframe
        pep_data <- pep_files %>%
            map(read_csv) %>%
            reduce(left_join, by = "SA2_MAINCODE_2016")
        
        #CHange the header to P numbers. 
            #That is, each column should be a P followed by a number, like 
            # P35 or P8712. The existing short column names are not unique, 
            # so can't be used in a data frame.
        names(pep_data) <- c("SA2_MAINCODE_2016", paste0("P", 1:8201))
        
        #Clean up
        rm(list = c("pep_files", "pep_path"))
        gc() #Run garbage collector
        
        write_csv(x = pep_data
                  , path = here("data"
                                , "pep_data.csv"
                                )
                  )
    } else {
        pep_data <- read_csv(here("data", "pep_data.csv"))
    }
    
    #Get shapefile data ----
    # SA2 level 
    shapes_aust <- maptools::readShapePoly(here("data"
                                                , "shapefiles"
                                                , "SA2_2016_AUST.shp"
                                                )
                                           , delete_null_obj = TRUE
                                           )
        
# 2) CLEAN DATA -----------------------------------------------------------
    
    #Working population data
    wpp_data_raw <- data_gcp_and_wpp %>%
        select(c(1, W5436, W5448, W5457, W5433, W5583)) %>%
        mutate(ICT_Professionals_N = W5436) %>%
        mutate(ICT_Professionals_P = W5436 / W5583) %>%
        mutate(ICT_Techs_N = W5448 ) %>%
        mutate(ICT_Techs_P = W5448 / W5583) %>%
        mutate(Health_Pros_N = W5433) %>%
        mutate(Health_Pros_P = W5433 / W5583) %>%
        mutate(Total_N = W5436 + W5448 + W5433) %>%
        mutate(Total_P = Total_N / W5583) %>%
        select(-c(W5436, W5448, W5457, W5433, W5583)) %>%
        arrange(desc(Total_P))
    glimpse(wpp_data_raw)

    #Join Working population data to shape file 
    oz_merged <- sp::merge(shapes_aust #Shapefiles
                           , wpp_data_raw # Working population data
                           , by.x = "SA2_MAIN16" #SA2 code in shapefiles 
                           , by.y = "SA2_MAINCODE_2016" #SA2 code in WPP data
                           )


    # Place of Enumeration data
        #7675 Information Media and Telecom Professionals
        #7705 Professional Scientific & Technical service professionals
        #7745 Health care & Social Assistance professionals
    
    
    pep_data_raw <- pep_data %>%
        select(c(1, P7675, P7705, P7745, P7793)) %>%
        mutate(Info_Media_Telecom_N = P7675) %>%
        mutate(Info_Media_Telecom_P = P7675 / P7793) %>%
        mutate(SciTech_N = P7705 ) %>%
        mutate(SciTech_P = P7705 / P7793) %>%
        mutate(Health_N = P7745) %>%
        mutate(Health_P = P7745  / P7793) %>%
        mutate(TotalP_N = P7675 + P7705 + P7745) %>%
        mutate(TotalP_P = TotalP_N / P7793) %>%
        select(-c(P7675, P7705, P7745, P7793)) %>%
        arrange(desc(TotalP_N))
    
    oz_merged_pep <- sp::merge(shapes_aust
                                , pep_data_raw # Working population data
                                , by.x = "SA2_MAIN16" #SA2 code in shapefiles 
                                , by.y = "SA2_MAINCODE_2016" #SA2 code in WPP data
                               )                                   
        
    glimpse(oz_merged_pep@data)
    
    
# 3) VISUALISE ------------------------------------------------------------

    #List top ten SA2s in NSW, by number of professionals
    top_twenty_p <- oz_merged_pep@data %>%
        arrange(desc(TotalP_N)) %>%
        top_n(n = 20) %>%
        select(`Area Name` = SA2_NAME16
               , `ICT Media Telecoms` = Info_Media_Telecom_N
               , `Science & Tech` = SciTech_N
               , `Health & Social Services` = Health_N
               , `Total` = TotalP_N
               #, `As a % of Working Population` = TotalP_P
        )
    
    #-------------------- TABLE 1 FOR DOCUMENT -------------------------------
    write_csv(x = top_twenty_p, path = "top_twenty_areas.csv")
    

# 4) SCHOOLS --------------------------------------------------------------

    private_schools <- read_csv(here("data", "private_schools.csv"))
    public_schools <- read_csv(here("data", "public_schools.csv"))
    
    glimpse(private_schools)
    glimpse(public_schools)
    
    #List top ten SA2s in NSW, by %
    top_ten_p <- oz_merged@data %>%
        arrange(desc(Total_N)) %>%
        mutate(location = paste(SA2_NAME16, STE_NAME16, " : ", Total_N, "professionals"))
    glimpse(top_ten_p)
    
# 3) TRANSFORM DATA -------------------------------------------------------
    
    
    # Transform School data into a spatial points data frame ####
    schools_spdf <- SpatialPointsDataFrame(coords = private_schools %>%
                                               dplyr::select(longitude
                                                             , latitude
                                               ) 
                                           , data = private_schools
    )
    
    # Count students and schools in each SA2 ####
    schools <- over(schools_spdf, oz_merged) %>% #Find SA2 for each school
        dplyr::select(SA2_MAIN16) %>%
        cbind(private_schools) %>% #Add the private schools dataset
        dplyr::select(SA2_MAIN16
                      , postcode
                      , student_number
                      , level_of_schooling
                      , selective_school
                      , school_subtype
                      , preschool_ind
                      , distance_education
                      , intensive_english_centre
                      , school_gender
                      , latitude
                      , longitude
        ) %>% # Reduce the number of columns
        dplyr::filter(preschool_ind == "N") %>% #Remove pre-schools
        dplyr::filter(student_number != "np") %>% # Remove non-numeric student numbers
        dplyr::mutate(student_number = as.numeric(student_number)) %>% #Convert to numeric
        dplyr::select(SA2_MAIN16, student_number) %>%
        dplyr::group_by(SA2_MAIN16) %>%
        dplyr::summarise(school_count = n() 
                         , student_count = sum(student_number)
        )
    
    # Count students and schools in each SA2 ####
    school_list <- over(schools_spdf, oz_merged) %>% #Find SA2 for each school
        dplyr::select(1:12) %>%
        cbind(public_schools_complete) %>% #Add the public schools dataset
        dplyr::filter(preschool_ind == "N") %>% #Remove pre-schools
        dplyr::filter(student_number != "np") %>% # Remove non-numeric student numbers
        dplyr::mutate(student_number = as.numeric(student_number)) #%>% #Convert to numeric
    
    
    # Prepare clean Australia table ####
    oz_data <- oz_merged@data %>%
        mutate(children = G9 + G12) %>%
        dplyr::select(1:12, children) %>%
        left_join(y = schools
                  , by.x = SA2_MAIN16
                  , by.y = SA2_MAIN16
        ) %>%
        dplyr::mutate_all(funs(replace(., is.na(.), 0 )))
    