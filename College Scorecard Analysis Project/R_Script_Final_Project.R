#Before running this script, it will be necessary to read the written summary document explaining the overview of our entire project scenario, the various research steps and methods/logic, and the results
#URL: https://github.com/brooksrelyt/infm600/blob/master/College%20Scorecard%20Analysis%20Project/College%20Scorecard%20Research%20Analysis.docx
#There is too much information regarding this research project and analysis to explain it all in the comments of this script, so the above written summary was necessary

# Use getwd() to find the R working directory
# Download the College Scorecard dataset ZIP file from our GitHub directory below
# https://github.com/brooksrelyt/infm600/blob/master/College%20Scorecard%20Analysis%20Project/Data/raw-data.csv.zip
# Unzip that downloaded file, and save the CSV file inside into your current working directory
# This file should be named "raw-data.csv"

#Saving "raw-data.csv" as a data frame to be referenced within this script
AllData<-read.csv("raw-data.csv",header = TRUE);


# **IF NEEDED** We need to make sure that we have all packages intalled that will be used for plotting
install.packages("ggplot2");
install.packages("Hmisc");
install.packages("dplyr");
install.packages("reshape");
install.packages("lme4");

#We need to make sure that we load needed libraries into the current R session
library(ggplot2);
library(Hmisc);
library(dplyr);
library(reshape);
library(lme4);
library(nlme);
theme_set(theme_grey(base_size = 12));


#Only keeping the columns that we determined may be useful for analysis
#Reference "Columns and Variables Used From Raw Data File.xlsx" on our GitHub repository in order to know which variables/columns are being kept below
#URL: https://github.com/brooksrelyt/infm600/blob/master/College%20Scorecard%20Analysis%20Project/Data/Columns%20and%20Variables%20Used%20From%20Raw%20Data%20File.xlsx
#Potential values for each variable are also explained in a Data Dictonary that has been prepared by the US Dept of Ed, "College_Scorecard_Data_Dictionary.xlsx", link is below
#URL: https://github.com/brooksrelyt/infm600/blob/master/College%20Scorecard%20Analysis%20Project/Data/College_Scorecard_Data_Dictionary.xlsx
CollegeScoreCardData <- AllData[c(2,3,4,5,8,12,13,15,16,17,18,19,20,24,25,26,27,28,29,30,31,32,33,34,35,36,37,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,291,292,293,294,295,296,297,298,299,300,301,304,316,317,318,319,320,321,322,323,324,325,326,327,328,329,330,353,354,357,358,359,360,361,362,363,364,365,366,377,378,379,380,381,382,383,384,385,386,387,388,397,398,399,400,401,402,403,404,405,411,412,413,414,415,416,417,418,419,429,430,431,432,438,439,440,441,1605,1606,1607,1610,1611,1613,1614,1615,1616,1637,1638,1639,1640,1641,1642,1643,1644,1645,1709,1710,1711,1774,1775,1776,1777,628)];

#Only keeping institutions in our dataset that are degree-granting
CollegeScoreCardData <- subset(CollegeScoreCardData, HIGHDEG == 1 | HIGHDEG == 2 | HIGHDEG == 3 | HIGHDEG == 4);

#Only keeping institutions in our dataset that are no currently on Heightened Cash Monitoring by the Department of Education
CollegeScoreCardData <- subset(CollegeScoreCardData, HCM2 == 0);

#Only keeping institutions in our dataset that are currently operating
CollegeScoreCardData <- subset(CollegeScoreCardData, CURROPER == 1);

#Only keeping institutions in our dataset that are not exclusively at the Graduate/Professional level
CollegeScoreCardData <- subset(CollegeScoreCardData, CCSIZSET == 1 | CCSIZSET == 2 | CCSIZSET == 3 | CCSIZSET == 4 | CCSIZSET == 5 | CCSIZSET == 6 | CCSIZSET == 7 | CCSIZSET == 8 | CCSIZSET == 9 | CCSIZSET == 10 | CCSIZSET == 11 | CCSIZSET == 12 | CCSIZSET == 13 | CCSIZSET == 14 | CCSIZSET == 15 | CCSIZSET == 16 | CCSIZSET == 17);

#Only keeping institutions in our dataset that are currently within the 50 states or District of Columbia
CollegeScoreCardData <- subset(CollegeScoreCardData, ST_FIPS == 1 | ST_FIPS == 2 | ST_FIPS == 4 | ST_FIPS == 5 | ST_FIPS == 6 | ST_FIPS == 8 | ST_FIPS == 9 | ST_FIPS == 10 | ST_FIPS == 11 | ST_FIPS == 12 | ST_FIPS == 13 | ST_FIPS == 15 | ST_FIPS == 16 | ST_FIPS == 17 | ST_FIPS == 18 | ST_FIPS == 19 | ST_FIPS == 20 | ST_FIPS == 21 | ST_FIPS == 22 | ST_FIPS == 23 | ST_FIPS == 24 | ST_FIPS == 25 | ST_FIPS == 26 | ST_FIPS == 27 | ST_FIPS == 28 | ST_FIPS == 29 | ST_FIPS == 30 | ST_FIPS == 31 | ST_FIPS == 32 | ST_FIPS == 33 | ST_FIPS == 34 | ST_FIPS == 35 | ST_FIPS == 36 | ST_FIPS == 37 | ST_FIPS == 38 | ST_FIPS == 39 | ST_FIPS == 40 | ST_FIPS == 41 | ST_FIPS == 42 | ST_FIPS == 43 | ST_FIPS == 44 | ST_FIPS == 45 | ST_FIPS == 46 | ST_FIPS == 47 | ST_FIPS == 48 | ST_FIPS == 49 | ST_FIPS == 50 | ST_FIPS == 51 | ST_FIPS == 53 | ST_FIPS == 54 | ST_FIPS == 55 | ST_FIPS == 56);

#Finding number of rows in the CollegeScoreCardData element, so that the for loops below function properly
NumRows <- nrow(CollegeScoreCardData);
#Add a column to CollegeScoreCard to be the definition/description for the numeric coded values of the Locale of the instutution (LOCALE). This makes plotting/labeling easier later as there are several numerically coded values for this variable
CollegeScoreCardData$LOCALE_DESC <- as.character(CollegeScoreCardData$LOCALE);

#This for loop goes through each element of the LOCALE column, and inserts the description of the coded variable value, according to the Data Dictionary
for(i in 1:NumRows) {
  if (CollegeScoreCardData$LOCALE[i] == 11){CollegeScoreCardData$LOCALE_DESC[i] <- "City: Large";}
  else if (CollegeScoreCardData$LOCALE[i] == 12){CollegeScoreCardData$LOCALE_DESC[i] <- "City: Midsize"}
  else if (CollegeScoreCardData$LOCALE[i] == 13){CollegeScoreCardData$LOCALE_DESC[i] <- "City: Small"}
  else if (CollegeScoreCardData$LOCALE[i] == 21){CollegeScoreCardData$LOCALE_DESC[i] <- "Suburb: Large"}
  else if (CollegeScoreCardData$LOCALE[i] == 22){CollegeScoreCardData$LOCALE_DESC[i] <- "Suburb: Midsize"}
  else if (CollegeScoreCardData$LOCALE[i] == 23){CollegeScoreCardData$LOCALE_DESC[i] <- "Suburb: Small"}
  else if (CollegeScoreCardData$LOCALE[i] == 31){CollegeScoreCardData$LOCALE_DESC[i] <- "Town: Fringe"}
  else if (CollegeScoreCardData$LOCALE[i] == 32){CollegeScoreCardData$LOCALE_DESC[i] <- "Town: Distant"}
  else if (CollegeScoreCardData$LOCALE[i] == 33){CollegeScoreCardData$LOCALE_DESC[i] <- "Town: Remote"}
  else if (CollegeScoreCardData$LOCALE[i] == 41){CollegeScoreCardData$LOCALE_DESC[i] <- "Rural: Fringe"}
  else if (CollegeScoreCardData$LOCALE[i] == 42){CollegeScoreCardData$LOCALE_DESC[i] <- "Rural: Distant"}
  else if (CollegeScoreCardData$LOCALE[i] == 43){CollegeScoreCardData$LOCALE_DESC[i] <- "Rural: Remote"}
  else {CollegeScoreCardData$LOCALE_DESC[i] <- "NULL"}
};

#Add a column which is the definition/description for the numeric coded values of the US Address State of the instutution (ST_FIPS). This makes plotting easier later as there are several numerically coded values for this variable
CollegeScoreCardData$ST_FIPS_DESC <- as.character(CollegeScoreCardData$ST_FIPS);

#This for loop goes through each element of the ST_FIPS column, and inserts the description of the coded variable value, according to the Data Dictionary
for(i in 1:NumRows) {
  if (CollegeScoreCardData$ST_FIPS[i] == 1){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Alabama";}
  else if (CollegeScoreCardData$ST_FIPS[i] == 2){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Alaska"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 4){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Arizona"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 5){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Arkansas"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 6){CollegeScoreCardData$ST_FIPS_DESC[i] <- "California"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 8){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Colorado"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 9){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Connecticut"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 10){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Delaware"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 11){CollegeScoreCardData$ST_FIPS_DESC[i] <- "District of Columbia"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 12){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Florida"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 13){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Georgia"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 15){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Hawaii"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 16){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Idaho"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 17){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Illinois"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 18){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Indiana"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 19){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Iowa"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 20){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Kansas"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 21){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Kentucky"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 22){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Louisiana"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 23){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Maine"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 24){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Maryland"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 25){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Massachusetts"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 26){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Michigan"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 27){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Minnesota"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 28){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Mississippi"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 29){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Missouri"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 30){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Montana"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 31){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Nebraska"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 32){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Nevada"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 33){CollegeScoreCardData$ST_FIPS_DESC[i] <- "New Hampshire"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 34){CollegeScoreCardData$ST_FIPS_DESC[i] <- "New Jersey"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 35){CollegeScoreCardData$ST_FIPS_DESC[i] <- "New Mexico"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 36){CollegeScoreCardData$ST_FIPS_DESC[i] <- "New York"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 37){CollegeScoreCardData$ST_FIPS_DESC[i] <- "North Carolina"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 38){CollegeScoreCardData$ST_FIPS_DESC[i] <- "North Dakota"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 39){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Ohio"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 40){CollegeScoreCardData$ST_FIPS_DESC[i] <- "OKlahoma"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 41){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Oregon"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 42){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Pennsylvania"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 44){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Rhode Island"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 45){CollegeScoreCardData$ST_FIPS_DESC[i] <- "South Carolina"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 46){CollegeScoreCardData$ST_FIPS_DESC[i] <- "South Dakota"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 47){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Tennessee"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 48){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Texas"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 49){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Utah"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 50){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Vermont"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 51){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Virginia"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 53){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Washington"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 54){CollegeScoreCardData$ST_FIPS_DESC[i] <- "West Virginia"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 55){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Wisconsin"}
  else if (CollegeScoreCardData$ST_FIPS[i] == 56){CollegeScoreCardData$ST_FIPS_DESC[i] <- "Wyoming"}
  else {CollegeScoreCardData$LOCALE_DESC[i] <- "NULL"}
};

#Add a column which is the definition/description for the numeric coded values of the Control, public vs private, of the instutution (CONTROL). This makes plotting easier later as there are several numerically coded values for this variable
CollegeScoreCardData$CONTROL_DESC <- as.character(CollegeScoreCardData$CONTROL);

#This for loop goes through each element of the CONTROL column, and inserts the description of the coded variable value, according to the Data Dictionary
for(i in 1:NumRows) {
  if (CollegeScoreCardData$CONTROL[i] == 1){CollegeScoreCardData$CONTROL_DESC[i] <- "Public"}
  else if (CollegeScoreCardData$CONTROL[i] == 2){CollegeScoreCardData$CONTROL_DESC[i] <- "Private nonprofit"}
  else if (CollegeScoreCardData$CONTROL[i] == 3){CollegeScoreCardData$CONTROL_DESC[i] <- "Private for-profit"}
  else {CollegeScoreCardData$CONTROL_DESC[i] <- "NULL"}
};

#Add a column which is the definition/description for the numeric coded values of the most predominately awarded degree of the instutution (PREDDEG). This makes plotting easier later as there are several numerically coded values for this variable
CollegeScoreCardData$PREDDEG_DESC <- as.character(CollegeScoreCardData$PREDDEG);

#This for loop goes through each element of the PREDDEG column, and inserts the description of the coded variable value, according to the Data Dictionary
for(i in 1:NumRows) {
  if (CollegeScoreCardData$PREDDEG[i] == 0){CollegeScoreCardData$PREDDEG_DESC[i] <- "Not classified"}
  else if (CollegeScoreCardData$PREDDEG[i] == 1){CollegeScoreCardData$PREDDEG_DESC[i] <- "Predominantly certificate-degree granting"}
  else if (CollegeScoreCardData$PREDDEG[i] == 2){CollegeScoreCardData$PREDDEG_DESC[i] <- "Predominantly associate's-degree granting"}
  else if (CollegeScoreCardData$PREDDEG[i] == 3){CollegeScoreCardData$PREDDEG_DESC[i] <- "Predominantly bachelor's-degree granting"}
  else if (CollegeScoreCardData$PREDDEG[i] == 4){CollegeScoreCardData$PREDDEG_DESC[i] <- "Entirely graduate-degree granting"}
  else {CollegeScoreCardData$PREDDEG_DESC[i] <- "NULL"}
};

#Add a column which is the definition/description for the numeric coded values of the highest degree awarded of the instutution (HIGHDEG). This makes plotting easier later as there are several numerically coded values for this variable
CollegeScoreCardData$HIGHDEG_DESC <- as.character(CollegeScoreCardData$HIGHDEG);

#This for loop goes through each element of the HIGHDEG column, and inserts the description of the coded variable value, according to the Data Dictionary
for(i in 1:NumRows) {
  if (CollegeScoreCardData$HIGHDEG[i] == 0){CollegeScoreCardData$HIGHDEG_DESC[i] <- "Non-degree-granting"}
  else if (CollegeScoreCardData$HIGHDEG[i] == 1){CollegeScoreCardData$HIGHDEG_DESC[i] <- "Certificate degree"}
  else if (CollegeScoreCardData$HIGHDEG[i] == 2){CollegeScoreCardData$HIGHDEG_DESC[i] <- "Associate degree"}
  else if (CollegeScoreCardData$HIGHDEG[i] == 3){CollegeScoreCardData$HIGHDEG_DESC[i] <- "Bachelor's degree"}
  else if (CollegeScoreCardData$HIGHDEG[i] == 4){CollegeScoreCardData$HIGHDEG_DESC[i] <- "Graduate degree"}
  else {CollegeScoreCardData$HIGHDEG_DESC[i] <- "NULL"}
};

#Add a column which is the definition/description for the numeric coded values of the Carnegie Basic Classification of the instutution (CCBASIC). This makes plotting easier later as there are several numerically coded values for this variable
CollegeScoreCardData$CCBASIC_DESC <- as.character(CollegeScoreCardData$CCBASIC);

#This for loop goes through each element of the CCBASIC column, and inserts the description of the coded variable value, according to the Data Dictionary
for(i in 1:NumRows) {
  if (CollegeScoreCardData$CCBASIC[i] == -2){CollegeScoreCardData$CCBASIC_DESC[i] <- "Not applicable"}
  else if (CollegeScoreCardData$CCBASIC[i] == 0){CollegeScoreCardData$CCBASIC_DESC[i] <- "Not classified"}
  else if (CollegeScoreCardData$CCBASIC[i] == 1){CollegeScoreCardData$CCBASIC_DESC[i] <- "Associate's Colleges: High Transfer-High Traditional"}
  else if (CollegeScoreCardData$CCBASIC[i] == 2){CollegeScoreCardData$CCBASIC_DESC[i] <- "Associate's Colleges: High Transfer-Mixed Traditional/Nontraditional"}
  else if (CollegeScoreCardData$CCBASIC[i] == 3){CollegeScoreCardData$CCBASIC_DESC[i] <- "Associate's Colleges: High Transfer-High Nontraditional"}
  else if (CollegeScoreCardData$CCBASIC[i] == 4){CollegeScoreCardData$CCBASIC_DESC[i] <- "Associate's Colleges: Mixed Transfer/Vocational & Technical-High Traditional"}
  else if (CollegeScoreCardData$CCBASIC[i] == 5){CollegeScoreCardData$CCBASIC_DESC[i] <- "Associate's Colleges: Mixed Transfer/Vocational & Technical-Mixed Traditional/Nontraditional"}
  else if (CollegeScoreCardData$CCBASIC[i] == 6){CollegeScoreCardData$CCBASIC_DESC[i] <- "Associate's Colleges: Mixed Transfer/Vocational & Technical-High Nontraditional"}
  else if (CollegeScoreCardData$CCBASIC[i] == 7){CollegeScoreCardData$CCBASIC_DESC[i] <- "Associate's Colleges: High Vocational & Technical-High Traditional"}
  else if (CollegeScoreCardData$CCBASIC[i] == 8){CollegeScoreCardData$CCBASIC_DESC[i] <- "Associate's Colleges: High Vocational & Technical-Mixed Traditional/Nontraditional"}
  else if (CollegeScoreCardData$CCBASIC[i] == 9){CollegeScoreCardData$CCBASIC_DESC[i] <- "Associate's Colleges: High Vocational & Technical-High Nontraditional"}
  else if (CollegeScoreCardData$CCBASIC[i] == 10){CollegeScoreCardData$CCBASIC_DESC[i] <- "Special Focus Two-Year: Health Professions"}
  else if (CollegeScoreCardData$CCBASIC[i] == 11){CollegeScoreCardData$CCBASIC_DESC[i] <- "Special Focus Two-Year: Technical Professions"}
  else if (CollegeScoreCardData$CCBASIC[i] == 12){CollegeScoreCardData$CCBASIC_DESC[i] <- "Special Focus Two-Year: Arts & Design"}
  else if (CollegeScoreCardData$CCBASIC[i] == 13){CollegeScoreCardData$CCBASIC_DESC[i] <- "Special Focus Two-Year: Other Fields"}
  else if (CollegeScoreCardData$CCBASIC[i] == 14){CollegeScoreCardData$CCBASIC_DESC[i] <- "Baccalaureate/Associate's Colleges: Associate's Dominant"}
  else if (CollegeScoreCardData$CCBASIC[i] == 15){CollegeScoreCardData$CCBASIC_DESC[i] <- "Doctoral Universities: Highest Research Activity"}
  else if (CollegeScoreCardData$CCBASIC[i] == 16){CollegeScoreCardData$CCBASIC_DESC[i] <- "Doctoral Universities: Higher Research Activity"}
  else if (CollegeScoreCardData$CCBASIC[i] == 17){CollegeScoreCardData$CCBASIC_DESC[i] <- "Doctoral Universities: Moderate Research Activity"}
  else if (CollegeScoreCardData$CCBASIC[i] == 18){CollegeScoreCardData$CCBASIC_DESC[i] <- "Master's Colleges & Universities: Larger Programs"}
  else if (CollegeScoreCardData$CCBASIC[i] == 19){CollegeScoreCardData$CCBASIC_DESC[i] <- "Master's Colleges & Universities: Medium Programs"}
  else if (CollegeScoreCardData$CCBASIC[i] == 20){CollegeScoreCardData$CCBASIC_DESC[i] <- "Master's Colleges & Universities: Small Programs"}
  else if (CollegeScoreCardData$CCBASIC[i] == 21){CollegeScoreCardData$CCBASIC_DESC[i] <- "Baccalaureate Colleges: Arts & Sciences Focus"}
  else if (CollegeScoreCardData$CCBASIC[i] == 22){CollegeScoreCardData$CCBASIC_DESC[i] <- "Baccalaureate Colleges: Diverse Fields"}
  else if (CollegeScoreCardData$CCBASIC[i] == 23){CollegeScoreCardData$CCBASIC_DESC[i] <- "Baccalaureate/Associate's Colleges: Mixed Baccalaureate/Associate's"}
  else if (CollegeScoreCardData$CCBASIC[i] == 24){CollegeScoreCardData$CCBASIC_DESC[i] <- "Special Focus Four-Year: Faith-Related Institutions"}
  else if (CollegeScoreCardData$CCBASIC[i] == 25){CollegeScoreCardData$CCBASIC_DESC[i] <- "Special Focus Four-Year: Medical Schools & Centers"}
  else if (CollegeScoreCardData$CCBASIC[i] == 26){CollegeScoreCardData$CCBASIC_DESC[i] <- "Special Focus Four-Year: Other Health Professions Schools"}
  else if (CollegeScoreCardData$CCBASIC[i] == 27){CollegeScoreCardData$CCBASIC_DESC[i] <- "Special Focus Four-Year: Engineering Schools"}
  else if (CollegeScoreCardData$CCBASIC[i] == 28){CollegeScoreCardData$CCBASIC_DESC[i] <- "Special Focus Four-Year: Other Technology-Related Schools"}
  else if (CollegeScoreCardData$CCBASIC[i] == 29){CollegeScoreCardData$CCBASIC_DESC[i] <- "Special Focus Four-Year: Business & Management Schools"}
  else if (CollegeScoreCardData$CCBASIC[i] == 30){CollegeScoreCardData$CCBASIC_DESC[i] <- "Special Focus Four-Year: Arts, Music & Design Schools"}
  else if (CollegeScoreCardData$CCBASIC[i] == 31){CollegeScoreCardData$CCBASIC_DESC[i] <- "Special Focus Four-Year: Law Schools"}
  else if (CollegeScoreCardData$CCBASIC[i] == 32){CollegeScoreCardData$CCBASIC_DESC[i] <- "Special Focus Four-Year: Other Special Focus Institutions"}
  else if (CollegeScoreCardData$CCBASIC[i] == 33){CollegeScoreCardData$CCBASIC_DESC[i] <- "Tribal Colleges"}
  else {CollegeScoreCardData$CCBASIC_DESC[i] <- "NULL"}
};

#Add a column which is the definition/description for the numeric coded values of the Carnegie Basic Size Classification of the instutution (CCSIZSET). This makes plotting easier later as there are several numerically coded values for this variable
CollegeScoreCardData$CCSIZSET_DESC <- as.character(CollegeScoreCardData$CCSIZSET);

#This for loop goes through each element of the CCSIZSET column, and inserts the description of the coded variable value, according to the Data Dictionary
for(i in 1:NumRows) {
  if (CollegeScoreCardData$CCSIZSET[i] == -2){CollegeScoreCardData$CCSIZSET_DESC[i] <- "Not applicable"}
  else if (CollegeScoreCardData$CCSIZSET[i] == 0){CollegeScoreCardData$CCSIZSET_DESC[i] <- "Not classified"}
  else if (CollegeScoreCardData$CCSIZSET[i] == 1){CollegeScoreCardData$CCSIZSET_DESC[i] <- "Two-year, very small"}
  else if (CollegeScoreCardData$CCSIZSET[i] == 2){CollegeScoreCardData$CCSIZSET_DESC[i] <- "Two-year, small"}
  else if (CollegeScoreCardData$CCSIZSET[i] == 3){CollegeScoreCardData$CCSIZSET_DESC[i] <- "Two-year, medium"}
  else if (CollegeScoreCardData$CCSIZSET[i] == 4){CollegeScoreCardData$CCSIZSET_DESC[i] <- "Two-year, large"}
  else if (CollegeScoreCardData$CCSIZSET[i] == 5){CollegeScoreCardData$CCSIZSET_DESC[i] <- "Two-year, very large"}
  else if (CollegeScoreCardData$CCSIZSET[i] == 6){CollegeScoreCardData$CCSIZSET_DESC[i] <- "Four-year, very small, primarily nonresidential"}
  else if (CollegeScoreCardData$CCSIZSET[i] == 7){CollegeScoreCardData$CCSIZSET_DESC[i] <- "Four-year, very small, primarily residential"}
  else if (CollegeScoreCardData$CCSIZSET[i] == 8){CollegeScoreCardData$CCSIZSET_DESC[i] <- "Four-year, very small, highly residential"}
  else if (CollegeScoreCardData$CCSIZSET[i] == 9){CollegeScoreCardData$CCSIZSET_DESC[i] <- "Four-year, small, primarily nonresidential"}
  else if (CollegeScoreCardData$CCSIZSET[i] == 10){CollegeScoreCardData$CCSIZSET_DESC[i] <- "Four-year, small, primarily residential"}
  else if (CollegeScoreCardData$CCSIZSET[i] == 11){CollegeScoreCardData$CCSIZSET_DESC[i] <- "Four-year, small, highly residential"}
  else if (CollegeScoreCardData$CCSIZSET[i] == 12){CollegeScoreCardData$CCSIZSET_DESC[i] <- "Four-year, medium, primarily nonresidential"}
  else if (CollegeScoreCardData$CCSIZSET[i] == 13){CollegeScoreCardData$CCSIZSET_DESC[i] <- "Four-year, medium, primarily residential"}
  else if (CollegeScoreCardData$CCSIZSET[i] == 14){CollegeScoreCardData$CCSIZSET_DESC[i] <- "Four-year, medium, highly residential"}
  else if (CollegeScoreCardData$CCSIZSET[i] == 15){CollegeScoreCardData$CCSIZSET_DESC[i] <- "Four-year, large, primarily nonresidential"}
  else if (CollegeScoreCardData$CCSIZSET[i] == 16){CollegeScoreCardData$CCSIZSET_DESC[i] <- "Four-year, large, primarily residential"}
  else if (CollegeScoreCardData$CCSIZSET[i] == 17){CollegeScoreCardData$CCSIZSET_DESC[i] <- "Four-year, large, highly residential"}
  else if (CollegeScoreCardData$CCSIZSET[i] == 18){CollegeScoreCardData$CCSIZSET_DESC[i] <- "Exclusively graduate/professional"}
  else {CollegeScoreCardData$CCSIZSET_DESC[i] <- "NULL"}
};

#Convert the retention columns to numeric, instead of as a factor(string)
CollegeScoreCardData$RET_FT4 <- as.numeric(as.character(CollegeScoreCardData$RET_FT4));
CollegeScoreCardData$RET_FTL4 <- as.numeric(as.character(CollegeScoreCardData$RET_FTL4));

#Add a column which consolidates the values of two other columns representing first-time, full-time retention rate. One column is for 4-year schools and one column if for less than 4-year schools, so that is why we wanted to combine them into one consolidated column
CollegeScoreCardData$RET_FT_COMBINE <- CollegeScoreCardData$RET_FT4;

#This for loop goes through each element of the RET_FT4 and RET_FTL4 columns,and inserts the non-null value into the new RET_FT_COMBINE column
for(i in 1:NumRows) {
  ifelse(is.na(CollegeScoreCardData$RET_FT4[i]), CollegeScoreCardData$RET_FT_COMBINE[i] <- CollegeScoreCardData$RET_FTL4[i], CollegeScoreCardData$RET_FT_COMBINE[i] <- CollegeScoreCardData$RET_FT4[i])
};

#Convert the completion rate columns to numeric, instead of as a factor(string)
CollegeScoreCardData$C150_4 <- as.numeric(as.character(CollegeScoreCardData$C150_4));
CollegeScoreCardData$C150_L4 <- as.numeric(as.character(CollegeScoreCardData$C150_L4));

#Add a column which consolidates the values of two other columns representing first-time, full-time completion rate. One column is for 4-year schools and one column if for less than 4-year schools, so that is why we wanted to combine them into one consolidated column
CollegeScoreCardData$C150_COMBINE <- CollegeScoreCardData$C150_4;

#This for loop goes through each element of the C150 and C150_L4 columns,and inserts the non-null value into the new RET_FT_COMBINE column
for(i in 1:NumRows) {
  ifelse(is.na(CollegeScoreCardData$C150_4[i]), CollegeScoreCardData$C150_COMBINE[i] <- CollegeScoreCardData$C150_L4[i], CollegeScoreCardData$C150_COMBINE[i] <- CollegeScoreCardData$C150_4[i])
};


#We need to compute the two "Super Score" institutional ratings, Student Financial Stability and Student Academic Success
#Both of those "Super Scores" ratings are explained in points 4 and 5 in our written summary document below
#URL: https://github.com/brooksrelyt/infm600/blob/master/College%20Scorecard%20Analysis%20Project/College%20Scorecard%20Research%20Analysis.docx
#Find mean and standard deviation for 2-year default rates (CDR2)
CDR2_Data <- as.numeric(as.character(CollegeScoreCardData$CDR2));
CDR2_Mean <- mean(CDR2_Data, na.rm=TRUE);
CDR2_SD <- sd(CDR2_Data, na.rm=TRUE);

#Find mean and standard deviation for 3-year default rates (CDR3)
CDR3_Data <- as.numeric(as.character(CollegeScoreCardData$CDR3));
CDR3_Mean <- mean(CDR3_Data, na.rm=TRUE);
CDR3_SD <- sd(CDR3_Data, na.rm=TRUE);

#Find mean and standard deviation for mean earnings of students working and not enrolled 10 years after entry (MN_EARN_WNE_P10)
MN_EARN_Data <- as.numeric(as.character(CollegeScoreCardData$MN_EARN_WNE_P10));
MN_EARN_Mean <- mean(MN_EARN_Data, na.rm=TRUE);
MN_EARN_SD <- sd(MN_EARN_Data, na.rm=TRUE);

#Find mean and standard deviation for median debt of completers (GRAD_DEBT_MDN_SUPP)
GRAD_DEBT_Data <- as.numeric(as.character(CollegeScoreCardData$GRAD_DEBT_MDN_SUPP));
GRAD_DEBT_Mean <- mean(GRAD_DEBT_Data, na.rm=TRUE);
GRAD_DEBT_SD <- sd(GRAD_DEBT_Data, na.rm=TRUE);

#Find mean and standard deviation for In-state tuition and fees (TUITIONFEE_IN)
TUIT_IN_Data <- as.numeric(as.character(CollegeScoreCardData$TUITIONFEE_IN));
TUIT_IN_Mean <- mean(TUIT_IN_Data, na.rm=TRUE);
TUIT_IN_SD <- sd(TUIT_IN_Data, na.rm=TRUE);

#Find mean and standard deviation for Out-of-state tuition and fees (TUITIONFEE_OUT)
TUIT_OUT_Data <- as.numeric(as.character(CollegeScoreCardData$TUITIONFEE_OUT));
TUIT_OUT_Mean <- mean(TUIT_OUT_Data, na.rm=TRUE);
TUIT_OUT_SD <- sd(TUIT_OUT_Data, na.rm=TRUE);

#Find mean and standard deviation for first-time, full-time, student retention rates (RET_FT_COMBINE)
RET_FT_Data <- as.numeric(as.character(CollegeScoreCardData$RET_FT_COMBINE));
RET_FT_Mean <- mean(RET_FT_Data, na.rm=TRUE);
RET_FT_SD <- sd(RET_FT_Data, na.rm=TRUE);

#Find mean and standard deviation for first-time, full-time, student completion rates (C150_COMBINE)
C150_Data <- as.numeric(as.character(CollegeScoreCardData$C150_COMBINE));
C150_Mean <- mean(C150_Data, na.rm=TRUE);
C150_SD <- sd(C150_Data, na.rm=TRUE);

#Find mean and standard deviation for Proportion of faculty that is full-time (PFTFAC)
PFTFAC_Data <- as.numeric(as.character(CollegeScoreCardData$PFTFAC));
PFTFAC_Mean <- mean(PFTFAC_Data, na.rm=TRUE);
PFTFAC_SD <- sd(PFTFAC_Data, na.rm=TRUE);

#Find mean and standard deviation for Percent withdrawn from original institution within 3 years (WDRAW_ORIG_YR3_RT)
WDRAW_Data <- as.numeric(as.character(CollegeScoreCardData$WDRAW_ORIG_YR3_RT));
WDRAW_Mean <- mean(WDRAW_Data, na.rm=TRUE);
WDRAW_SD <- sd(WDRAW_Data, na.rm=TRUE);

#Next, we will find the Z-Scores for each of the above Super Score variables, which will help us to calculate the Super Scores
CollegeScoreCardData$CDR2_Z_Score <- (CDR2_Data - CDR2_Mean)/CDR2_SD;
CollegeScoreCardData$CDR3_Z_Score <- (CDR3_Data - CDR3_Mean)/CDR3_SD;
CollegeScoreCardData$MN_EARN_Z_Score <- (MN_EARN_Data - MN_EARN_Mean)/MN_EARN_SD;
CollegeScoreCardData$GRAD_DEBT_Z_Score <- (GRAD_DEBT_Data - GRAD_DEBT_Mean)/GRAD_DEBT_SD;
CollegeScoreCardData$TUIT_IN_Z_Score <- (TUIT_IN_Data - TUIT_IN_Mean)/TUIT_IN_SD;
CollegeScoreCardData$TUIT_OUT_Z_Score <- (TUIT_OUT_Data - TUIT_OUT_Mean)/TUIT_OUT_SD;
CollegeScoreCardData$RET_FT_Z_Score <- (RET_FT_Data - RET_FT_Mean)/RET_FT_SD;
CollegeScoreCardData$C150_Z_Score <- (C150_Data - C150_Mean)/C150_SD;
CollegeScoreCardData$PFTFAC_Z_Score <- (PFTFAC_Data - PFTFAC_Mean)/PFTFAC_SD;
CollegeScoreCardData$WDRAW_Z_Score <- (WDRAW_Data - WDRAW_Mean)/WDRAW_SD;

#Adding Z-Score variable categories together to make overall "Super Scores"
#Some Z-Scores need to be multiplies by -1, in the cases where having an observed variable value below the mean is actually better (for example, a lower default rate is better than a higher one)

#Student Financial Stability Super Score
CollegeScoreCardData$Financial_Stability_SuperScore <- (CollegeScoreCardData$CDR2_Z_Score*-1) + (CollegeScoreCardData$CDR3_Z_Score*-1) + (CollegeScoreCardData$MN_EARN_Z_Score) + (CollegeScoreCardData$GRAD_DEBT_Z_Score*-1) + (CollegeScoreCardData$TUIT_IN_Z_Score*-1) + (CollegeScoreCardData$TUIT_OUT_Z_Score*-1);

#Student Academic Student Success Super Score
CollegeScoreCardData$Academic_Student_Success_SuperScore <- (CollegeScoreCardData$RET_FT_Z_Score) + (CollegeScoreCardData$C150_Z_Score) + (CollegeScoreCardData$PFTFAC_Z_Score) + (CollegeScoreCardData$WDRAW_Z_Score*-1);

#-------PERSONA ANALYSIS BELOW--------#
#We created four personas typical of an 11th grade Prince George's County publich high school student/family, in order to help test our model of recommending schools for them to consider attending
#URL: https://github.com/brooksrelyt/infm600/blob/master/College%20Scorecard%20Analysis%20Project/Persona%20Examples%20for%20College%20ScoreCard%20Data.docx

#You may want to export datasets from R to Excel, but you will need the appropriate packages/libraries included, so use the following commands if that is what you want to do: install.packages("xlsx"); library("xlsx");

#THe columns will need to be converted from factors to numeric vectors, in order to filter by them for analysis of the various personas. In-state Tuition, Out-of-state Tuition, ACT Composite 25th Percentile of Admitted Students 
CollegeScoreCardData$TUITIONFEE_IN <- as.numeric(as.character(CollegeScoreCardData$TUITIONFEE_IN));
CollegeScoreCardData$TUITIONFEE_OUT <- as.numeric(as.character(CollegeScoreCardData$TUITIONFEE_OUT));
CollegeScoreCardData$ACTCM25 <- as.numeric(as.character(CollegeScoreCardData$ACTCM25));
CollegeScoreCardData$MN_EARN_WNE_P10 <- as.numeric(as.character(CollegeScoreCardData$MN_EARN_WNE_P10));


#For Persona #1, we want to apply the following filters, based on their personal criteria
#1. State = New York
#2. A Public institution
#3. Institution's admitted student ACT COmposite 25th percntile is 30 or below
#4. Institution's out-of-state tuition is $25,000 or lower, based on how much his family can afford/is willing to contribute to costs (family income $75,000 - $100,000)
Persona1Data <- subset(CollegeScoreCardData, ST_FIPS_DESC == "New York" & CONTROL == 1 & TUITIONFEE_OUT <= 26000 & ACTCM25 <= 30);

#After we use the individual persona to filter down to a subset of schools the student is interested in, we sort that list by the Student Financial Stability Super Score, and present a list of the top 5 ranking institutions 
Persona1_SFS_Sort <- head(Persona1Data[order(Persona1Data$Financial_Stability_SuperScore, decreasing = TRUE),], 5);
#Optional command to view the top 5 list: View(Persona1_SFS_Sort)
#Optional Command to export this top 5 list to Excel. It requires you to download the "xlsx" package and load the library as well.: write.xlsx(Persona1_SFS_Sort, file = "Persona1_SFS_Sort.xlsx", sheetName = "Sheet1");

#After we use the individual persona to filter down to a subset of schools the student is interested in, we sort that list by the Student Academic Success Super Score, and present a list of the top 5 ranking institutions 
Persona1_SAS_Sort <- head(Persona1Data[order(Persona1Data$Academic_Student_Success_SuperScore, decreasing = TRUE),], 5);
#Optional command to view the top 5 list: View(Persona1_SAS_Sort)
#Optional Command to export this top 5 list to Excel. It requires you to download the "xlsx" package and load the library as well.: write.xlsx(Persona1_SAS_Sort, file = "Persona1_SAS_Sort.xlsx", sheetName = "Sheet1");


#For Persona #2, we want to apply the following filters, based on their personal criteria
#1. State = Maryland, Virginia, or DC
#2. A Public institution
#3. Does not want to attend a school in a rural setting
#4. Average Retention rate of the school is at least 70%+ 
Persona2Data <- subset(CollegeScoreCardData, (ST_FIPS_DESC == "Maryland" | ST_FIPS_DESC == "Virginia" | ST_FIPS_DESC == "District of Columbia") & CONTROL == 1 & (LOCALE != "41" & LOCALE != "42" & LOCALE != "43") & RET_FT_COMBINE >0.7);

#After we use the individual persona to filter down to a subset of schools the student is interested in, we sort that list by the Student Financial Stability Super Score, and present a list of the top 5 ranking institutions 
Persona2_SFS_Sort <- head(Persona2Data[order(Persona2Data$Financial_Stability_SuperScore, decreasing = TRUE),], 5);
#Optional command to view the top 5 list: View(Persona2_SFS_Sort)
#Optional Command to export this top 5 list to Excel. It requires you to download the "xlsx" package and load the library as well.: write.xlsx(Persona2_SFS_Sort, file = "Persona2_SFS_Sort.xlsx", sheetName = "Sheet1");

#After we use the individual persona to filter down to a subset of schools the student is interested in, we sort that list by the Student Academic Success Super Score, and present a list of the top 5 ranking institutions 
Persona2_SAS_Sort <- head(Persona2Data[order(Persona2Data$Academic_Student_Success_SuperScore, decreasing = TRUE),], 5);
#Optional command to view the top 5 list: View(Persona2_SAS_Sort)
#Optional Command to export this top 5 list to Excel. It requires you to download the "xlsx" package and load the library as well.: write.xlsx(Persona2_SAS_Sort, file = "Persona2_SAS_Sort.xlsx", sheetName = "Sheet1");


#For Persona #3, we want to apply the following filters, based on their personal criteria
#1. State = Maryland, Virginia, DC, Delaware, or Pennsylvania
#2. An institution where the mean earnings 10 years after graduation are at least $60,000
#3. a 4-year school 
Persona3Data <- subset(CollegeScoreCardData, (ST_FIPS_DESC == "Maryland" | ST_FIPS_DESC == "Virginia" | ST_FIPS_DESC == "District of Columbia" | ST_FIPS_DESC == "Delaware" | ST_FIPS_DESC == "Pennsylvania") & MN_EARN_WNE_P10 >= 60000 & PREDDEG == "3");

#After we use the individual persona to filter down to a subset of schools the student is interested in, we sort that list by the Student Financial Stability Super Score, and present a list of the top 5 ranking institutions 
Persona3_SFS_Sort <- head(Persona3Data[order(Persona3Data$Financial_Stability_SuperScore, decreasing = TRUE),], 5);
#Optional command to view the top 5 list: View(Persona3_SFS_Sort)
#Optional Command to export this top 5 list to Excel. It requires you to download the "xlsx" package and load the library as well.: write.xlsx(Persona3_SFS_Sort, file = "Persona3_SFS_Sort.xlsx", sheetName = "Sheet1");

#After we use the individual persona to filter down to a subset of schools the student is interested in, we sort that list by the Student Academic Success Super Score, and present a list of the top 5 ranking institutions 
Persona3_SAS_Sort <- head(Persona3Data[order(Persona3Data$Academic_Student_Success_SuperScore, decreasing = TRUE),], 5);
#Optional command to view the top 5 list: View(Persona3_SAS_Sort)
#Optional Command to export this top 5 list to Excel. It requires you to download the "xlsx" package and load the library as well.: write.xlsx(Persona3_SAS_Sort, file = "Persona3_SAS_Sort.xlsx", sheetName = "Sheet1");


#For Persona #4, we want to apply the following filters, based on their personal criteria
#1. State = Pennsylvania or New Jersey
#2. Public school
#3. Small city locale setting
#4. 4-year school
Persona4Data <- subset(CollegeScoreCardData, (ST_FIPS_DESC == "Pennsylvania" | ST_FIPS_DESC == "New Jersey") & CONTROL == "1" & LOCALE == "13" & PREDDEG == "3");

#After we use the individual persona to filter down to a subset of schools the student is interested in, we sort that list by the Student Financial Stability Super Score, and present a list of the top 5 ranking institutions 
Persona4_SFS_Sort <- head(Persona4Data[order(Persona4Data$Financial_Stability_SuperScore, decreasing = TRUE),], 5);
#Optional command to view the top 5 list: View(Persona4_SFS_Sort)
#Optional Command to export this top 5 list to Excel. It requires you to download the "xlsx" package and load the library as well.: write.xlsx(Persona4_SFS_Sort, file = "Persona4_SFS_Sort.xlsx", sheetName = "Sheet1");

#After we use the individual persona to filter down to a subset of schools the student is interested in, we sort that list by the Student Academic Success Super Score, and present a list of the top 5 ranking institutions 
Persona4_SAS_Sort <- head(Persona4Data[order(Persona4Data$Academic_Student_Success_SuperScore, decreasing = TRUE),], 5);
#Optional command to view the top 5 list: View(Persona4_SAS_Sort)
#Optional Command to export this top 5 list to Excel. It requires you to download the "xlsx" package and load the library as well.: write.xlsx(Persona4_SAS_Sort, file = "Persona4_SAS_Sort.xlsx", sheetName = "Sheet1");


#In order to download the entire dataset with our Super Scores included, use the following command: write.csv(CollegeScoreCardData, file = "CollegeScoreCardData_Team_TAZY_Edit.csv");


#Below are some graphs/plots that we created, and used in our Power Point presentation (URL below)
#Power Point Presentation URL: https://github.com/brooksrelyt/infm600/blob/master/College%20Scorecard%20Analysis%20Project/College%20Selection%20Model%20Presentation.pptx
#Defining Colors to be used in the plots below
colors = c("red","yellow","green","violet","orange","blue","pink","cyan");

# Financial Stability vs Academic Success
plot(CollegeScoreCardData$Academic_Student_Success_SuperScore,CollegeScoreCardData$Financial_Stability_SuperScore, main="Financial Stability vs Academic Success", xlab="Academic Success", ylab="Financial Stability", col=colors)

# In-State Tuition vs Out-of-State Tuition
plot(as.numeric(as.character(CollegeScoreCardData$Academic_Student_Success_SuperScore)),as.numeric(as.character(CollegeScoreCardData$Financial_Stability_SuperScore)), main="Financial Stability vs Academic Success", xlab="Academic Success", ylab="Financial Stability", col=colors)

# Histogram of Financial Stability Superscore
hist(as.numeric(as.character(CollegeScoreCardData$Academic_Student_Success_SuperScore)), main="Histogram of Financial Stability Superscore", xlab="Financial SuperScore",  col="blue")
# Histogram of Academic Student Success
hist(as.numeric(as.character(CollegeScoreCardData$Academic_Student_Success_SuperScore)), main="Histogram of Academic Success Superscore", xlab="Academic Success SuperScore",  col="green")

# Histogram of default within 3yrs
hist(as.numeric(as.character(CollegeScoreCardData$CDR3)), main="Histogram of Default within 3yrs", xlab="Default within 3yrs",  col="blue")

# Histogram of Graduate Median Debt
hist(as.numeric(as.character(CollegeScoreCardData$GRAD_DEBT_MDN_SUPP)), main="Histogram of Graduate Median Debt", xlab="Graduate  Median Debt",  col="yellow")

#Histogram of Mean Earnings 10 years after graduating
hist(as.numeric(as.character(CollegeScoreCardData$MD_EARN_WNE_P10)), main="Histogram of Mean Earnings 10 yrs after graduating", xlab="Graduate  Median Debt",  col="cyan")

# Histogran of First-time full-time retention rate
hist(as.numeric(as.character(CollegeScoreCardData$RET_FT_COMBINE)), main="Histogram of First-time Full-time Retention Rate", xlab="First-Time Full-time Retention Rate",  col="blue")

# Histogram of First-time full-time completion rate
hist(as.numeric(as.character(CollegeScoreCardData$C150_COMBINE)), main="Histogram of First-time Full-time Completion Rate", xlab="First-Time Full-time Completion Rate",  col="green")

# Histogram of withdrawal from original institution within 3 yrs
hist(as.numeric(as.character(CollegeScoreCardData$WDRAW_ORIG_YR3_RT)), main="Histogram of Withdrwawal from Original Inst within 3 yrs", xlab="Withdrawal from original institution within 3yrs",  col="yellow")

# Histogram of Proportion of Faculty Full-time Staff
hist(as.numeric(as.character(CollegeScoreCardData$PFTFAC)), main="Histogram of Proportion of Faculty Full-time Staff", xlab="Proportion of Faculty Full-time Staff",  col="cyan")



