#' dwn_shp_zip
#' 
#' Download a zipped *shapefile* and returns it as an `sf` object. 
#'
#' @param url       a url for a zipped file containing one *shapefile* 
#' @param cname_old When not `NA`, and a so-called column exists, the returned object will have only that column
#' @param cname_new When not `NA`, 
#' @param crs       If not `NULL`, 
#' @param out_path  If not `NULL`, 
#' @param top       If not `NULL`, returns only the first `top` complete records, disregarding all other options.
#' @param verbose   When `TRUE` highlight with messages all operations.
#'
#' @return an `sf` object
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @importFrom sf st_read st_transform st_crs st_make_valid
#' @importFrom dplyr rename
#' @importFrom utils download.file unzip
#' @importFrom rlang :=
#'
#' @export
#'
dwn_shp_zip <- function(url, cname_old = NA, cname_new = NA, crs = NULL, out_path = NULL, top = NULL, verbose = FALSE){
    if(verbose) message('Downloading...')
    tmpf <- tempfile()
    download.file(url, tmpf, quiet = TRUE)
    if(verbose) message('Unzipping...')
    tpflag <- FALSE
    if(is.null(out_path)){
        opath <- tempdir()
        tpflag <- TRUE
    } else {
        if(!dir.exists(out_path)){
            warning('`out_path` is not a valid folder. Using temporary storage.')
            opath <- tempdir() 
            tpflag <- TRUE
        } else { opath <- out_path }
    }
    unzip(tmpf, exdir = opath)
    yn <- grep('shp$', unzip(tmpf, list = TRUE)$Name, value = TRUE)
    unlink(tmpf)
    if(!is.null(top)){
        if(suppressWarnings(is.na(as.numeric(top)))) stop('`top` should be a valid integer')
        top <- abs(as.numeric(top))
        y <- st_read(
                file.path(opath, yn), 
                query = paste0('SELECT * FROM \"', gsub('(.*)\\.shp$', '\\1', yn), '\" LIMIT ', top), 
                quiet = TRUE
        )
        if(tpflag) unlink(opath)
        y
    } else {
        if(verbose) message('Reading...')
        y <- st_read(file.path(opath, yn), quiet = TRUE) |> st_make_valid()
        if(!is.na(cname_old)){
            if(!cname_old %in% names(y)){
                warning('`cname_old` is not a valid name. Return all columns.')
            } else {
                y <- y |> dplyr::select(cname_old)
                if(!is.na(cname_new)) y <- y |> rename(!!cname_new := 1)
            }
        }
        if(tpflag) unlink(opath)
        if(is.null(crs)){
            y 
        } else {
            if(suppressWarnings(is.na(st_crs(as.numeric(gsub('[^0-9]', '\\1', crs)))))){
                warning('`crs` is not a valid reference (see `https://epsg.io/`). Return original CRS: ', st_crs(y)[[1]], ', ', gsub('.*[,]([0-9].*)]]$', '\\1', st_crs(y)[[2]]))
                y
            } else { y |> st_transform(crs) }
        }
    }
}
