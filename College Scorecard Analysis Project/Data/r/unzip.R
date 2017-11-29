## UMD Server 
temp <- tempfile()
download.file("http://electricpub.umd.edu/clients/data/raw-data.csv.zip",temp)
data <- read.table(unz(temp, "raw-data.csv"), fill = TRUE)
unlink(temp)

## UMD GitHub infm600/College Scorecard Analysis Project/Data/raw-data.csv.zip
temp <- tempfile()
download.file("http://github.com/brooksrelyt/infm600/blob/master/College%20Scorecard%20Analysis%20Project/Data/raw-data.csv.zip?raw=true",temp)
data2 <- read.table(unz(temp, "raw-data.csv"), fill = TRUE)
unlink(temp)