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



