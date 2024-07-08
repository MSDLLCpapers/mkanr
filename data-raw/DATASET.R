## code to prepare `DATASET` dataset goes here
library(random.cdisc.data)
data("cadsl")
data("cadtte")
adsl<-cadsl
adtte<-cadtte

usethis::use_data(adsl, overwrite = TRUE)
usethis::use_data(adtte, overwrite = TRUE)
