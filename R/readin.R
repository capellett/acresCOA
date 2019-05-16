#' Read spreadsheets downloaded as described in the vignette.
read_downloaded_files <- function(directory) {
  lapply(
    list.files(directory),
    function(i) {
      readr::read_csv(
        file.path(directory, i),
        guess_max=10000) %>%
        dplyr::select(Year, State, County, Commodity, `Data Item`,
                      Domain, `Domain Category`, Value, `CV (%)`)
    } )
}

munge_land_download <- function(x) {
  x2 <- stringr::str_split(x$`Data Item`, fixed(" - "), n=2, simplify=TRUE)
  x$LandType <- x2[,1]
  x$Domain <- x2[,2]
  x$Value <- as.numeric(str_remove(x$Value, fixed(',')))
  x$`CV (%)` <- as.numeric(x$`CV (%)`)
  x$High <- (1+(x$`CV (%)`/100))* x$Value
  x$Low <- (1-(x$`CV (%)`/100))* x$Value
  x[,c('Commodity', 'Domain Category', 'Data Item')] <- NULL
  x
}

munge_crop_download <- function(x) {
  x2 <- str_split(x$`Data Item`, fixed(" - "), n=2, simplify=TRUE)
  x$CropType <- x2[,1]
  x$Domain <- x2[,2]
  x$type <- if_else(
    str_detect(x$CropType, fixed('IRRIGATED')),
    'Irrigated', 'All')
  x$CropType <- str_remove(x$CropType, fixed(', IRRIGATED'))
  x$CommodityComma <- paste0(x$Commodity, ', ')
  x$CropType <- str_remove(x$CropType, fixed(x$CommodityComma))
  x$CropType <- str_remove(x$CropType, fixed(x$Commodity))
  x[,c('Data Item', 'CommodityComma')] <- NULL
  x$Value <- as.numeric(str_remove(x$Value, fixed(',')))
  x$`CV (%)` <- as.numeric(x$`CV (%)`)
  x$High <- (1+(x$`CV (%)`/100))* x$Value
  x$Low <- (1-(x$`CV (%)`/100))* x$Value
  x$Crop <- x$Commodity
  x$Commodity <- NULL
  x
}
