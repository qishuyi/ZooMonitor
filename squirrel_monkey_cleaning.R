library(readr)
library(tidyr)
library(dplyr)
library(stringr)

squirrel_monkey_data <- read_csv("https://raw.githubusercontent.com/qishuyi/ZooMonitor/master/report_study_1583445158.csv", col_types = cols(Notes = col_character()))
