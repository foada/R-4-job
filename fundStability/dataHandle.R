library(data.table)
# library(aztools)

# function
setDtEnc <- function(x, .f) {
  .f <- match.fun(.f)
  stopifnot(data.table::is.data.table(x))
  key <- data.table::key(x)
  
  cols <- names(x)[purrr::map_lgl(x, is.character)]
  
  if (length(cols) > 0L) {
    for (i in seq_along(cols)) {
      col <- cols[i]
      data.table::set(x, i = NULL, j = col, value = .f(x[[col]]))
    }
  }
  data.table::setnames(x, colnames(x), .f(colnames(x)))
  if (!is.null(key)) data.table::setkeyv(x, .f(key)) else data.table::setkey(x, NULL)
  invisible(x[])
}


setDtTz <- function(x, .f, tz) {
  .f <- match.fun(.f)
  stopifnot(data.table::is.data.table(x))
  key <- data.table::key(x)
  
  cols <- names(x)[purrr::map_lgl(x, assertthat::is.time)]
  
  if (length(cols) > 0L) {
    for (i in seq_along(cols)) {
      col <- cols[i]
      colValue <- .f(x[[col]], tzone = tz)
      data.table::set(x, i = NULL, j = col, value = colValue)
    }
    data.table::setkeyv(x, key)
  }
  
  invisible(x[])
}


write_feather <- function(x, path) {
  if (data.table::is.data.table(x)) {
    x <- data.table::copy(x)
    setDtEnc(x, enc2utf8)
    setDtTz(x, lubridate::with_tz, tz = "UTC")
  }
  if (!dir.exists(dir <- dirname(path))) dir.create(dir, recursive = TRUE)
  feather::write_feather(x, path = path)
}


read_feather <- function(paths, key = NULL, columns = NULL) {
  if (length(paths) == 1L) {
    x <- data.table::setDT(
      feather::read_feather(paths, columns = columns)
    )
  } else {
    x <-
      purrr::map(paths, function(path) {
        feather::read_feather(path, columns = columns)
      }) %>%
      data.table::rbindlist(.)
  }
  data.table::setkeyv(x, cols = key)
  setDtEnc(x, enc2utf8)
  setDtTz(x, lubridate::with_tz, tz = "Asia/Shanghai")
  x[]
}



to_date <- function(x) {
  stopifnot(!is.null(x))
  if (all(is.na(x))) {
    rep(as.Date(NA_character_), length(x))
  } else if (is.character(x) || is.numeric(x)) {
    x %>%
      lubridate::parse_date_time(., orders = c("ymd", "ymd HMS")) %>%
      lubridate::date(.)
  } else if (lubridate::is.timepoint(x)) {
    lubridate::date(x)
  } else {
    stop("only char, numeric or timepoint is allowed")
  }
  
}


db_sql <- function(conn, sql, key = NULL, date_fields = NULL,
                   encoding = "UTF-8", db_encoding = "GB2312", tz = "Asia/Shanghai") {
  
  db_tz <- tz
  db_tz_server <- tz
  
  #tbl <- DBI::dbGetQuery(conn, sql)
  tbl <- aztools::db_read(conn, sql)
  data.table::setDT(tbl)
  data.table::setnames(tbl, toupper(colnames(tbl)))
  #setDtEnc(tbl, function(x) iconv(x, from = db_encoding, to = encoding))
  
  setDtTz(tbl, .f = lubridate::force_tz, tz = db_tz)
  if (!is.null(date_fields)) {
    tbl[, c(date_fields) := purrr::map(
      .SD, ~{
        if (lubridate::is.timepoint(.)) {
          to_date(lubridate::with_tz(., tzone = db_tz_server))
        } else {
          to_date(.)
        }
      }
    ), .SDcols = c(date_fields)]
  }
  if (!is.null(key)) data.table::setkeyv(tbl, cols = key)
  tbl[]
}


save_rds <- function(x, path, ...) {
  if (data.table::is.data.table(x)) {
    x <- data.table::copy(x)
    setDtEnc(x, enc2utf8)
    setDtTz(x, lubridate::with_tz, tz = "UTC")
  }
  if (!dir.exists(dir <- dirname(path))) dir.create(dir, recursive = TRUE)
  readr::write_rds(x, file = path, ...)
}
