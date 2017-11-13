# Read and view the College Scorecard raw data of colleges with Mid East Region and Virginia
CollegeScoreCard<-read.csv("College_ScoreCard_Raw_Data_v5.csv",header = TRUE)
View(CollegeScoreCard)

# Create and view a Super Score Data subset that includes the State(ST_FIPS_DESC), 
# Locale(LOCALE_DESC), Control type(CONT_DESC), Carnegie Sizing and setting(CCSIZE_DESC),
# 2-year chort default rate(CDR2), 3-year cohort defaut rate(CDR3),
# mean earnings of students working and not enrolled(MN_EARN_WNE_P10),
# median debt of graduates(GRAD_DEBT_MDN_SUPP), in-state tuition(TUITIONFEE_IN),
# out-of-state tuition(TUITIONFEE_OUT)
SuperScoreData<-CollegeScoreCard[c(15,19,13,25,138,139,151,159,101,102)]
View(SuperScoreData)

# Create a vector with just the colleges with the States
state = SuperScoreData$ST_FIPS_DESC

# Find the frequency distribution of the colleges by States
state.freq = table(state)

# Display the results in column format
cbind(state.freq)

# Select a color palette
colors = c("red","yellow","green","violet","orange","blue","pink","cyan")

# Create a bar chart of the frequency distribution of the colleges by States
barplot(state.freq, col=colors)

# Replace median grad debt with NA for character strings that cannot be converted into numeric values
c_grad_debt<-as.numeric(as.character(SuperScoreData$GRAD_DEBT_MDN_SUPP))

# Range of median debt of students who have graduated from collges within the States
range(c_grad_debt,na.rm=TRUE)

# Mean of the median debt of students who have graduated from colleges within the States
mean(c_grad_debt,na.rm=TRUE)

# Standard deviation of the median debt of students who have graduated from colleges within the States
sd(c_grad_debt,na.rm=TRUE)

# Histogram of median debt of students who have graduated from colleges within the States
hist(c_grad_debt)

# Replace In-State Tuition with NA for character strings that cannot be converted into numeric values
c_tuitin<-as.numeric(as.character(SuperScoreData$TUITIONFEE_IN))

# Range of in-state tuition and fees of full-time, first-timme students of collges within the States
range(c_tuitin,na.rm=TRUE)

# Mean of in-state tuition and fees of full-time, first-time students of collges within the States
mean(c_tuitin,na.rm=TRUE)

# Standard deviation of in-state tuition and fees of full-time, first-time students of collges within the States
sd(c_tuitin,na.rm=TRUE)

# Histogram of in-state tuition and fees of full-time, first-time students of collges within the States
hist(c_tuitin)

# Replace Out-of-State Tuition with NA for character strings that cannot be converted into numeric values
c_tuitout<-as.numeric(as.character(SuperScoreData$TUITIONFEE_OUT))

# Range of out-of-state tuition and fees of full-time, first-timme students of collges within the States
range(c_tuitout,na.rm=TRUE)

# Mean of out-of-state tuition and fees of full-time, first-time students of collges within the States
mean(c_tuitout,na.rm=TRUE)

# Standard deviation of out-of-state tuition and fees of full-time, first-time students of collges within the States
sd(c_tuitout,na.rm=TRUE)

# Histogram of out-of-state tuition and fees of full-time, first-time students of collges within the States
hist(c_tuitout)

# Mean of median debt of graduates by State
tapply(as.numeric(as.character(SuperScoreData$GRAD_DEBT_MDN_SUPP)), SuperScoreData$ST_FIPS_DESC, mean, na.rm=TRUE)

# Barplot the mean of the median debt of graduates by State
barplot(tapply(as.numeric(as.character(SuperScoreData$GRAD_DEBT_MDN_SUPP)), SuperScoreData$ST_FIPS_DESC, mean, na.rm=TRUE),col=colors)

# Create a child data set for Maryland
mdstate = state == "Maryland"
mddata = SuperScoreData[mdstate,]
View(mddata)

# Create a vector with just college sizes in Mayland
mdccsize = mddata$CCSIZE_DESC

# Frequency distribution of the college sizes in Maryland
mdccsize.freq = table(mdccsize)

# Display the results in column format
cbind(mdccsize.freq)

# Create a pie chart of the college sizes in Maryland
pie(mdccsize.freq, col=colors)
# (RET_FT_COMBINE, which is a combination of RET_FT4, RET_FTL4) First-time, full-time student retention rate 
RET_FT_COMBINE <- as.numeric((as.character(df$RET_FT_COMBINE)))

mean(RET_FT_COMBINE, na.rm=TRUE)
sd(RET_FT_COMBINE, na.rm=TRUE)

hist(RET_FT_COMBINE, na.rm=TRUE)


# (C150_COMBINE , which is a combination of C150_4, C150_L4) Completion rate for first-time, full-time students 
C150_COMBINE <- as.numeric((as.character(df$C150_COMBINE)))

mean(C150_COMBINE, na.rm=TRUE)
sd(C150_COMBINE, na.rm=TRUE)

hist(C150_COMBINE, na.rm=TRUE)


# (PFTFAC) Proportion of faculty that is full-time 
PFTFAC <- as.numeric((as.character(df$PFTFAC)))

mean(PFTFAC, na.rm=TRUE)
sd(PFTFAC, na.rm=TRUE)

hist(PFTFAC, na.rm=TRUE)
