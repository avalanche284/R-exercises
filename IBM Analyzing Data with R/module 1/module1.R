#SB
library(tidyverse)
url <- "https://dax-cdn.cdn.appdomain.cloud/dax-airline/1.0.1/lax_to_jfk.tar.gz"
download.file(url, destfile = "lax_to_jfk.tar.gz")
untar("lax_to_jfk.tar.gz", tar = "internal")
sub_airline <- read_csv("lax_to_jfk/lax_to_jfk.csv",
                        col_types = cols(
                          'DivDistance' = col_number(),
                          'DivArrDelay' = col_number()
                        ))
head(sub_airline)
tail(sub_airline)
dim(sub_airline)
colnames(sub_airline)
# check data type
sapply(sub_airline, typeof)
x = 10.4
class(x)
k <- 1
class(k)
y <- as.integer(4)
class(y)
z <- 0i
class(z)
logical_values <- c(TRUE, T, FALSE, F)
class(logical_values)
class("this is a value")

sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarise(avg_carrier_delay = mean(CarrierDelay, na.rm = TRUE))

sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarize(sd_carrier_delay = sd(CarrierDelay, na.rm = TRUE))


sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarize(avg_arr_delay = mean(ArrDelay, na.rm = TRUE))

glimpse(sub_airline)

