library(assertthat)
library(magrittr)
library(jsonlite)

# APIs Endpoints ----------------------------------------------------------

#' Provide API endpoints
#' @param api string, which api to use?
#' @return string, endpoint of api
get_endpoint <- function(api){

    ep <- list(
        "amap" = "http://restapi.amap.com/v3/geocode/geo?",
        "baidu" = "http://api.map.baidu.com/geocoder/v2/?",
        "geoconv" = "http://api.map.baidu.com/geoconv/v1/?"
    )


    ep[[api]]
}


# Geocoding ----------------------------------------------------------------

#' Geocode address
#'
#' Geocode a particular address.
#'
#' @param address a string, address to geocode, more specific the better
#' @param api which API to use, currently supports "Amap" or "Baidu"
#' @param key API application key
#' @param output specify output type, JSON or XML
#' @return
#' Geocode of address (latitude, longitude)  \emph{# take note of the order}
#' @examples
#' \dontrun{
#' geocode("北京市朝阳区朝阳首府", api = "amap", key = "Your_Api_Key_Here")
#'
#' # set application key via environment variables
#' file.edit("~/.Renviron")
#' # restart R to take effect
#' amap_ak <- Sys.getenv("amap_ak")
#' geocode("北京市朝阳区朝阳首府", key = amap_ak, output = "xml")
#' }
#'
#' @importFrom magrittr %>%
#' @export
geocode <- function(address, api = "amap", key = NULL, output = "json"){

    # Make standard

    api <- tolower(api)

    # Check if key exists

    if(!is.null(key)){
        key = key
    }else if(!is.null(getOption(paste0(api, "_key")))){
        key = getOption(paste0(api, "_key"))
    }else{
        stop("Application key not found.")
    }


    # Send request

    get_endpoint(api) %>%
        paste0("&output=", tolower(output)) %>%
        paste0("&ak=", key) %>%
        paste0("&key=", key) %>%
        paste0("&address=", address) %>%
        jsonlite::fromJSON()

}




# Geocode Conversion -----------------------------------------------------------

#' Geocode conversion
#'
#' Convert non-Baidu standard geocode to Baidu standard geocode
#'
#' @param lat geocode latitude, string or numeric
#' @param lon geocode longitude, string or numeric
#' @param key Baidu map API application key
#' @return
#' Baidu standard geocode (latitude, longitude) \emph{# take note of the order}
#' @examples
#' \dontrun{
#' geoconv("114.21892734521", "29.575429778924", key = "Your_Api_Key_Here")
#' }
#'
#' @importFrom magrittr %>%
#' @export
geoconv <- function(lat, lon, key = NULL){

    # Make sure Baidu key exists

    if(!is.null(key)){
        key = key
    }else if(!is.null(getOption("baidu_key"))){
        key = getOption("baidu_key")
    }else{
        stop("Baidu application key not found")
    }


    # Send request

    get_endpoint("geoconv") %>%
        paste0("&coords=", lat, ",", lon) %>%
        paste0("&ak=", key) %>%
        # Convert from Google, Aliyun, Amap
        paste0("&from=3") %>%
        # to Baidu's bd0911
        paste0("&to=5") %>%
        jsonlite::fromJSON()

}
