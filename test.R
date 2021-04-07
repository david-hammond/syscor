library(ProjectTemplate)
library(hammond)
library(tidyverse)
library(systr)
#install.packages("magick")
library(crop)
#only uncomment if need to updat from database
hammond::hdb_login(host = "192.168.0.67", db = "nationalhdb_march2021", user = "admin", password = "admin")
x = hdb_search("iep") %>% filter(from_file %in% c("02-ppi.R", "01-gpi.R"))
pos = grepl("Banded", x$seriescode) | x$from_file == "02-ppi.R"
x = x[pos,]
# pos = !grepl(":", x$seriescode) | x$from_file == "01-gpi.R"
# x = x[pos,]
meta = x %>% select(uid, variablename)
#corpus = corpus %>% left_join(hammond::hcountryinfo) %>% filter(region == "Central America And Caribbean")
corpus = hdb_get(x) %>% select(uid, geocode, year, value) %>% filter(geocode != "PSE")
#corpus = corpus %>% left_join(hammond::hcountryinfo) %>% filter(region == "Central America And Caribbean") %>% select(uid, geocode, year, value)
#setwd("./data")
newscale = c(1,5)

systr_setup(corpus, meta, newscale)
corpus = corpus %>% filter(geocode == "MEX")
mex = systr_granger(corpus)


