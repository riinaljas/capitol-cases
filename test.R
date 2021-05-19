library(tidyverse)
library(googlesheets4)

mydata <- read_csv('keywords_2021-05-18')
ss <- googlesheets4::sheet_write(mydata)
print(ss)
