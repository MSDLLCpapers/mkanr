# Export data to CSV for easy import to SAS
## ADSL ---
write.csv(adsl,paste0(home_path,"/data-raw/adsl.csv"),row.names=TRUE, quote=FALSE, na = "")
## ADTTE ---
write.csv(adtte,paste0(home_path,"/data-raw/adtte.csv"),row.names=TRUE, quote=FALSE, na = "")