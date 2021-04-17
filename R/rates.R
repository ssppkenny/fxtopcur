get_from_date <- function(year_to, month_to, day_to, years) {
  next_date <- lubridate::make_datetime(year_to-years, month_to, day_to) + lubridate::days(1)
  list("year"=lubridate::year(next_date), "month"=lubridate::month(next_date), "day"=lubridate::day(next_date))
}

make_cur_range <- function(cur_from="CHF", cur_to="RUB", day_to=lubridate::day(Sys.Date()), month_to=lubridate::month(Sys.Date()), year_to=lubridate::year(Sys.Date()),
                           day_from=get_from_date(year_to, month_to, day_to)$day, month_from=get_from_date(year_to, month_to, day_to)$month,
                           year_from=year_to-1) {
  structure(list(cur_from=cur_from, cur_to=cur_to, day_from=day_from, day_to=day_to, month_from=month_from,
              month_to=month_to, year_from=year_from, year_to=year_to), class="CurrencyRange")
}

#' Fetches time series with conversion rates for two currencies and year count
#'
#' @param cur_from string (3-letter ISO Code) source currency
#' @param cur_to string (3-letter ISO code) target currency
#' @param years a number of years
#' @return A data frame
#' @examples
#' fetch_cur_data()
#' fetch_cur_data(from_cur="USD")
#' @export
fetch_cur_data <- function(cur_from="CHF", cur_to="RUB", years=1) {
  sys_date <- Sys.Date()
  day_to <- lubridate::day(sys_date)
  month_to <- lubridate::month(sys_date)
  year_to <- lubridate::year(sys_date)
  ranges <- list()
  for (i in 1:years) {
    day_from=get_from_date(year_to, month_to, day_to, i)$day
    month_from=get_from_date(year_to, month_to, day_to, i)$month
    year_from=get_from_date(year_to, month_to, day_to, i)$year
    r <- make_cur_range(cur_from=cur_from, cur_to=cur_to, day_to=day_to, month_to=month_to, year_to=year_to-i+1,
                        day_from=day_from, month_from=month_from, year_from=year_from)
    ranges[[i]] <- r
  }
  l <- detectCores()
  if (length(ranges)<l) {
    l <- length(ranges)
  }
  if (length(ranges)==1) {
    df <- get_rates(ranges[[1]])
    dplyr::tibble(df)
  } else {
    cl <- parallel::makeCluster(l)
    parallel::clusterExport(cl, "pad")
    parallel::clusterExport(cl, "%>%")
    parallel::clusterExport(cl, "str_replace")
    lst <- parallel::parLapply(cl, ranges, get_rates)
    parallel::stopCluster(cl)
    df <- data.table::rbindlist(lst)
    dplyr::tibble(df)
  }
  
}

pad <- function(x) {
    stringr::str_pad(x, 2, "left", pad="0")
}

get_rates <- function(rng) {
  cur_from <- rng["cur_from"]
  cur_to <- rng["cur_to"]
  day_from <- pad(rng["day_from"])
  day_to <- pad(rng["day_to"])
  month_from <- pad(rng["month_from"])
  month_to <- pad(rng["month_to"])
  year_from <- rng["year_from"]
  year_to <- rng["year_to"]
  s <- "https://fxtop.com/en/historical-exchange-rates.php?A=1&C1=${cur_from}&C2=${cur_to}&TR=1&DD1=${day_from}&MM1=${month_from}&YYYY1=${year_from}&B=1&P=&I=1&DD2=${day_to}&MM2=${month_to}&YYYY2=${year_to}&btnOK=Go%21"
  myurl <- stringr::str_interp(s)
  from_date <- as.Date(stringr::str_interp("${year_from}-${month_from}-${day_from}"), format="%Y-%m-%d")
  to_date <- as.Date(stringr::str_interp("${year_to}-${month_to}-${day_to}"), format="%Y-%m-%d")
  days <- as.numeric(to_date - from_date, units="days")
  rates <- htmltab::htmltab(doc=myurl, which="//table[@border=1]")[1:days,]
  rate_col <- paste(cur_from, cur_to, sep="/")
  rates <- rates %>% dplyr::rename("value"=2, "percent"=3)
  loc <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  rates <- rates %>% dplyr::mutate("Date"=as.Date(Date, format="%A %d %B %Y")) %>% dplyr::rename("day"=all_of("Date"))
  Sys.setlocale("LC_TIME", loc)
  rates <- rates %>% dplyr::mutate(percent=str_replace(percent, "%", "")) %>% dplyr::mutate(percent=as.double(percent), value=as.double(value)) %>% dplyr::arrange(day)
  rates <- rates %>% dplyr::select(day, value, percent)

  return(rates)
}



