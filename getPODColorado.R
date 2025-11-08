#' This function returns all Colorado points of diversion (or more specifically,
#' "structures used to divert, release, store, and manage water") within a user-supplied area of interest, plus
#' water right information associated with those diversions.
#' A table of all column name descriptions can be found at
#' https://dwr.state.co.us/Tools/Structures/DataDictionary/GetDataDictionary and
#' https://dwr.state.co.us/Rest/GET/Help/Api/GET-api-v2-waterrights-netamount
#'
#' @param aoi An sf polygon object used to grab Colorado POD and water rights data.
#' @param dist The distance (in the same units used by `aoi`) to buffer around the area of interest.
#'
#' @return An `sf` object of Colorado PODs within the aoi and its buffer.

getPODColorado <- function(aoi, dist = 0){
  
  call <- "https://dnrftp.state.co.us/CDSS/GIS/Structure.zip" #download boundary
  temp1 <- base::tempfile()
  utils::download.file(paste0(call), destfile = temp1, method = "curl")
  
  temp2 <- base::tempfile()
  utils::unzip(temp1, exdir = temp2)
  
  # add buffer around park boundary
  aoi_buffer <- aoi %>%
    sf::st_zm() %>% # ADDED sf::
    sf::st_buffer(dist = dist)
  
  co_wr <- sf::st_read(dsn = temp2) %>%
    sf::st_make_valid() %>% 
    sf::st_transform(4269)
  
  #join sf object and table attributes and filter to area of interest
  nearby_wr <- co_wr %>%
    .[aoi_buffer,] %>%
    dplyr::filter(stringr::str_detect(CurrInUse, "(U)|(A)|(F)")) # ADDED stringr:: # Filter for active structures only
  
  if(base::nrow(nearby_wr) == 0){
    base::print("No PODs found within the user-supplied `aoi`")
    
    return(nearby_wr)}
  
  div_downloader <- function(WDID){
    
    call <- paste0("https://dwr.state.co.us/Rest/GET/api/v2/structures/?wdid=", WDID)
    
    dat <- httr::GET(call)
    
    # convert content to text
    dat_text <- httr::content(dat, "text", encoding = "UTF-8")
    
    if(dat_text == "This URL is properly formatted, but returns zero records from CDSS."){final_df <- NULL} else {
      #parse data in JSON
      dat_json <- jsonlite::fromJSON(dat_text, flatten = TRUE)
      
      final_df <- tibble::as_tibble(dat_json) # ADDED tibble::
    }
    
    return(final_df)
  }
  
  base::try(divs_api <- nearby_wr$WDID %>%
              purrr::map(~ base::try(div_downloader(.), silent = TRUE)) %>%
              dplyr::bind_rows() %>%
              .$ResultList %>%
              dplyr::distinct(.keep_all = TRUE), silent = TRUE)
  
  wr_downloader <- function(WDID){
    
    call <- paste0("https://dwr.state.co.us/Rest/GET/api/v2/waterrights/netamount/?format=json&wdid=", WDID)
    
    dat <- httr::GET(call)
    
    # convert content to text
    dat_text <- httr::content(dat, "text", encoding = "UTF-8")
    
    if(dat_text == "This URL is properly formatted, but returns zero records from CDSS."){final_df <- NULL} else {
      #parse data in JSON
      dat_json <- jsonlite::fromJSON(dat_text, flatten = TRUE)
      
      final_df <- tibble::as_tibble(dat_json) # ADDED tibble::
    }
    
    return(final_df)
  }
  
  base::try(wr_api <- nearby_wr$WDID %>%
              purrr::map(~ base::try(wr_downloader(.), silent = TRUE)) %>%
              dplyr::bind_rows() %>%
              .$ResultList %>%
              dplyr::distinct(.keep_all = TRUE), silent = TRUE)
  
  base::try(final_wr <- dplyr::left_join(nearby_wr, divs_api, by = c("WDID"= "wdid")) %>% # ADDED dplyr::
              dplyr::left_join(., wr_api,by = c("WDID"= "wdid")) %>% # ADDED dplyr::
              dplyr::rename(associatedCaseNumbers = associatedCaseNumbers.x,
                            waterDistrict = waterDistrict.x, 
                            structureType = structureType.x, 
                            gnisId = gnisId.x, 
                            division = division.x, 
                            locationAccuracy = locationAccuracy.x, 
                            streamMile = streamMile.x, 
                            structureName = structureName.x) %>%
              # duplicate info columns
              dplyr::select(-c(county.x, county.y, gnisId.y, division.y, latitude, longitude, locationAccuracy.y, pm.x, pm.y, 
                               range.y, section.y, streamMile.y, structureName.y, structureType.y, township.y, waterDistrict.y,
                               waterSource.x, waterSource.y, associatedCaseNumbers.y))
            , silent = TRUE)
  
  return(final_wr)
  
}

