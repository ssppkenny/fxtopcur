library(stringr)
library(dplyr)
library(purrr)
library(parallel)
library(htmltab)
library(lubridate)
library(data.table)
library(ggplot2)

#' Creates a currency range object
#'
#' @param from_cur string (3-letter ISO Code) from which currency to convert
#' @param to_cur string (3-letter ISO code) to which currency to convert
#' @param day string (3-letter ISO code) to which currency to convert
#' @param month string (3-letter ISO code) to which currency to convert
#' @param year_from from which year to convert
#' @param year_to to which year to convert
#' @return A currency range object
#' @examples
#' make_cur_range()
#' make_cur_range(from_cur="USD")
#' @export
make_cur_range <- function(from_cur="CHF", to_cur="RUB", day=lubridate::day(Sys.Date()),
                           month=lubridate::month(Sys.Date()), year_from=lubridate::year(Sys.Date())- 1,
                           year_to=lubridate::year(Sys.Date())) {
  structure(list(from_cur=from_cur, to_cur=to_cur, day=day, month=month, year_from=year_from, year_to=year_to), class="CurrencyRange")
}

make_cur_data <- function(cur_range, data) {
  structure(list(cur_range=cur_range, data=data), class="CurrencyData")
}

#' Fetches currency conversion data
#' @export
fetch <- function(x) UseMethod("fetch")

#' Fetches currency conversion data
#'
#' @param x currency range object
#' @return A currency data object
#' @examples
#' fetch(rng)
#' @export
fetch.CurrencyRange <- function(x) {
  ranges <- create_ranges(x$from_cur, x$to_cur, x$day, x$month, x$year_from, x$year_to)
  make_cur_data(x, get_cur_rates(ranges))
}

#' Plots currency conversion data
#'
#' @param x currency data object
#' @return A ggplot2 plot of time series
#' @examples
#' plot(cur_data)
#' @export
plot.CurrencyData <- function(x) {
  x$data %>% ggplot(aes(x=day,y=value)) + geom_line()
}

cur_range <- function(from_cur="CHF", to_cur="RUB", day=lubridate::day(Sys.Date()),
                      month=lubridate::month(Sys.Date()), year_from=lubridate::year(Sys.Date())- 1, year_to=lubridate::year(Sys.Date())) {
  list(from_cur=from_cur, to_cur=to_cur, day=day, month=month, year_from=year_from, year_to=year_to)
}

create_ranges <- function(from_cur, to_cur, day, month, year_from, year_to) {
  diff <- year_to - year_from
  1:diff %>% map(function(x) {cur_range(from_cur, to_cur, day, month, year_to - x, year_to - x + 1 )})
}

get_cur_rates <- function(rng) {
  lst <- mclapply(rng, function(x) {
    get_rates(x)
  }, mc.cores = detectCores())
  rbindlist(lst)
}

get_rates <- function(rng) {
  from_cur = rng["from_cur"]
  to_cur = rng["to_cur"]
  day = str_pad(rng["day"], 2, "left", pad="0")
  month = str_pad(rng["month"], 2, "left", pad="0")
  year_from = rng["year_from"]
  year_to = rng["year_to"]
  s <- "https://fxtop.com/en/historical-exchange-rates.php?A=1&C1=${from_cur}&C2=${to_cur}&TR=1&DD1=${day}&MM1=${month}&YYYY1=${year_from}&B=1&P=&I=1&DD2=${day}&MM2=${month}&YYYY2=${year_to}&btnOK=Go%21"
  myurl <- str_interp(s)
  from_date = as.Date(str_interp("${year_from}-${month}-${day}"), format = "%Y-%m-%d")
  to_date = as.Date(str_interp("${year_to}-${month}-${day}"), format = "%Y-%m-%d")
  days <- as.numeric(to_date - from_date, units="days")
  rates <- htmltab(doc = myurl, which = "//table[@border=1]")[1:days,]
  rate_col <- paste(from_cur, to_cur, sep="/")
  rates <- rates %>% rename("value" = 2, "percent" = 3)
  rates <- rates %>% mutate("Date" = as.Date(Date, format = "%A %d %B %Y")) %>% rename("day" = all_of("Date"))
  rates <- rates %>% mutate(percent = str_replace(percent, "%", "")) %>% mutate(percent=as.double(percent), value=as.double(value)) %>% arrange(day)
  rates <- rates %>% select(day, value, percent)
  return(rates)
}



