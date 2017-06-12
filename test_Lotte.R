# testing
# doel: bij testdate komen. 

source("inlog_testdata.R")


files <- readRDS("data/testdata.rda")
read.table(files[3], sep=",")

read.table(files[1], sep=",")
