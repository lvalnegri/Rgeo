#' dwn_shp_zip
#' 
#' Download a zipped *shapefile* and return it as an `sf` object. 
#'
#' @param url      A url for a zipped file containing one or more *shapefile* (see `pos_shf` on how to deal with multiple files)
#' @param cnms_old When not `NULL`, and all the involved columns exist, the returned object will have only those columns
#' @param cnms_new When not `NULL`, and `cnms_old` and `cnms_new` fit together in length, names in `cnms_old` are orderly changed with the ones in `cnms_new`
#' @param crs      If not `NULL`, and a valid *CRS*, the spatial object is transformed accordingly
#' @param out_path If not `NULL`, and `out_path` is a valid folder, the *shapefile* will be unzipped therein.
#'                 Otherwise, a temporary folder is used, and dropped afterwards.
#' @param as_dt    When `TRUE`, the geometry column is dropped, returning a `data.table`.
#' @param top      If not `NULL`, only the first `top` complete records are returned, *disregarding all other options*.
#' @param lst      If `TRUE`, only the names of the included *shapefile(s)* are returned, *disregarding all other options*.
#' @param pos_shf  If not `NA`, returns the $pos_shf$-th included *shapefile* when all are sorted alphabetically
#'                 You should use `lst = TRUE` beforehand to be sure of the correct position of the desired files.
#' @param verbose  When `TRUE`, the start of all ongoing operations is highlighted with a message.
#'
#' @return an `sf` object or a `data.table`
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table
#' @importFrom sf st_read st_transform st_crs st_make_valid st_drop_geometry
#' @importFrom dplyr select
#' @importFrom utils download.file unzip
#'
#' @export
#'
dwn_shp_zip <- function(url, 
                        cnms_old = NULL, 
                        cnms_new = NULL, 
                        crs = NULL, 
                        out_path = NULL, 
                        as_dt = FALSE, 
                        top = NULL, 
                        lst = FALSE,
                        pos_shf = NA,
                        verbose = FALSE
               ){
    if(verbose) message('Downloading zip file')
    tmpf <- tempfile()
    download.file(url, tmpf, quiet = TRUE)
    yns <- unzip(tmpf, list = TRUE)$Name
    yn <- sort(grep('shp$', yns, value = TRUE))
    if(lst){ 
        unlink(tmpf)
        return(yn)
    }
    nf <- length(yn)
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
    if(verbose) message('Unzipping in ', opath)
    if(!is.na(pos_shf)){
        if(suppressWarnings(is.na(as.numeric(pos_shf)))) stop('`pos_shf` should be a valid integer')
        pos_shf <- as.integer(pos_shf)
        if(pos_shf < 0) warning('`pos_shf` must be greater than zero. Returning the first shapefile.')
        if(pos_shf > length(yn)) warning('`pos_shf` must be lower than ', length(yn), ', the number of included shapefiles. Returning the last shapefile.')
        yn <- yn[max(1, min(length(yn), as.numeric(pos_shf)))]
    } else {
        yn <- yn[1]
        if(nf > 1) message('The zip file contains multiple shapefiles but no position was given. Returning the first shapefile: ', gsub('(.*)\\.shp$', '\\1', yn))
    }
    ync <- gsub('(.*)\\.shp$', '\\1', yn)
    unzip(tmpf, files = grep(ync, yns, value = TRUE), exdir = opath)
    unlink(tmpf)
    if(!is.null(top)){
        if(suppressWarnings(is.na(as.numeric(top)))) stop('`top` should be a valid integer')
        top <- abs(as.numeric(top))
        y <- st_read( file.path(opath, yn),  query = paste0('SELECT * FROM \"', ync, '\" LIMIT ', top), quiet = TRUE )
        if(tpflag) unlink(opath)
        return(y)
    } else {
        if(verbose) message('Reading the *shapefile*')
        y <- st_read(file.path(opath, yn), quiet = TRUE) |> st_make_valid()
        if(!is.null(cnms_old)){
            cnms_old <- unique(cnms_old)
            if(sum(cnms_old %in% names(y)) == 0){
                warning('`cnms_old` contains no valid names. Return all columns.')
                cnms_old <- names(y)
            } else if(sum(cnms_old %in% names(y)) < length(cnms_old)){
                warning('`cnms_old` contains invalid name(s): ', paste(cnms_old[!cnms_old %in% names(y)], collapse = ', '), '.')
                cnms_old <- cnms_old[cnms_old %in% names(y)]
            }
            y <- y |> select(cnms_old)
            if(!is.null(cnms_new)){
                cnms_new <- unique(cnms_new)
                if(length(cnms_old) != length(cnms_new)){
                    warning('`cnms_old` and `cnms_new` have different lengths. Return original names.')
                } else {
                    y <- y |> setnames(cnms_old, cnms_new)
                }
            }
        }
        if(tpflag) unlink(opath)
        if(!is.null(crs)){
            if(suppressWarnings(is.na(st_crs(as.numeric(gsub('[^0-9]', '\\1', crs)))))){
                warning('`crs` is not a valid reference (see `https://epsg.io/`). Return original CRS: ', st_crs(y)[[1]], ', ', gsub('.*[,]([0-9].*)]]$', '\\1', st_crs(y)[[2]]))
            } else { 
                if(verbose) message('Changing CRS')
                y <- y |> st_transform(crs) 
            }
        }
        if(verbose) message('Job Done! Returning a ', ifelse(as_dt, 'data.table', 'spatial object'))
        if(as_dt) data.table(y |> st_drop_geometry()) else y
    }
}
