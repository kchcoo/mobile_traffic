library(tidyverse)
library(readxl)

data<-read_excel("tms_raw.xlsx", sheet = "raw_hourly", na = c("불량", "교정", "단절"))
