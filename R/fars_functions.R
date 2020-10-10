#'
#' Lee un archivo CSV 
#'
#' @description
#' La funcion lee un archivo CSV definino por \code{filename} argumento y retorna
#' un tibble. Si el path esta incorrecto, la funcion termina con un error.
#'
#' @param filename Camino al archivo CSV (character)
#'
#' @return La funcion retura un tibble (data.frame) basado en el archivo CSV.
#'
#' @examples
#' \dontrun{
#' accident_2015 <- fars_read("./data/accident_2013.csv.bz2")
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Crea un filename
#'
#' @description
#' La funcion creea un filename para el archivo .csv.bz2 basado en el \code{year}
#' argumento con la forma "accident_<year>.csv.bz2". Requires un valor de entrada numerico o
#' entero, de otra forma finaliza con un error.
#'
#' @param año Numerico o entero que indica el año del set de datos.
#'
#' @return Returna un string de caracteres en el formato "accident_<year>.csv.bz2" que puede 
#' ser usado como un nombre de archivo
#'
#' @examples
#' \dontrun{
#' makefilename(2016)
#' }
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Lee un archivo de accidentes para un mes y año
#'
#' @description
#' La funcion acepta un vector o lista de años y retorna una lista de data
#' frames con columnas MONTH y year basados en los datos del archivo  "accident_<year>.csv.bz2
#' El archivo necesita estar ubicado en el directorio de trabajo.
#'
#' @param years Un vector o lista de años en formato numerico entero.
#'
#' @return Retorna un alista de tibbles (data frames) con el mismo numero de filas
#' que los datos en el archivo "accident_<year>.csv.bz2" y dos columnas- MONTH y
#' year. Retorna NULL y un warning si el archivo no existe.
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013:2015)
#' fars_read_years(list(2013, 2014))
#'
#' # Resulta en un warning
#' fars_read_years(2016)
#' }
#'
#' @importFrom dplyr %>% mutate select
#'
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>% 
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Cuenta el numero de accidentes por mes y anio
#'
#' Basado en la lista de anios, la funcion calculael numero de accidentes
#' en USA en una base mensual. El archivo de accidentes necesita estar en el directorio 
#' de trabajo, el anio puede ser pasado como lista o vector.
#'
#' @param years UN vector o lista de anios (numericos o enteros) que van a ser buscados en
#' los datos.
#'
#' @return Retorna unpivot tibble (data frame) con meses en las filas y los anios seleccionados
#' en las columnas conteniendo el numero de accidentes. Retorna un warning por cada anio de 
#' entrada que no exista en el datasets. Retorna un error (no
#' results) si un numero diferente a entero es presentado.
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2013:2015)
#' }
#'
#' @importFrom dplyr %>% bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plotea los accidentes en un mapa de USA
#'
#' La funcion acepta un numero de estado y un anio y plotea los accidentes en un mapa 
#' simple. El numero de estado debe ser entero o numerico y debe existir
#' en los datos FARS, de otra forma la funcion termina con un error. Tambien
#' returna un error si el archivo de datos para ese anio no existe.
#'
#' @param state.num El numero del estado de USA como usado en FARS.
#' Debe ser numerico entero.
#' @param year El anio del analisis(numerico o entero)
#'
#' @return Returna un ploteo de accidentes basado en las entradas \code{state.num} y
#' \code{year}. Returna un error si el estado o el anio no existen en el conjunto
#' de datos.
#'
#' @examples
#' \dontrun{
#' fars_map_state(45, 2014)
#'
#' # Results in an error
#' fars_map_state(43, 2013)
#' fars_map_state(50, 2014)
#' }
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
