# Use getwd() to find the R working directory
# Copy College_ScoreCard_Raw_Date_v5.csv into the working directory
# Read and view the College Scorecard raw data of colleges within Mid East Region and Virginia
CollegeScoreCard<-read.csv("College_ScoreCard_Raw_Data_v5.csv",header = TRUE);
View(CollegeScoreCard);

#Create manageable data subset of the columns that will be used to form our two "SuperScores".

# "Financial Stability" SuperScore
#(CDR2) 2 year default rate, (CDR3) 3 year default rate, 
#(MN_EARN_WNE_P10) average earnings 10 years after graduating,
#(GRAD_DEBT_MDN_SUPP) Median debt of completers, 
#(TUITIONFEE_IN) in state tuition costs, 
#(TUITIONFEE_OUT) Out-of-state tuition costs

# "Academic Student Success" SuperScore
#(RET_FT_COMBINE, which is a combination of RET_FT4, RET_FTL4) First-time, full-time student retention rate,
#(C150_COMBINE, which is a combination of C150_4, C150_L4) Completion rate for first-time, full-time students, 
#(PFTFAC) Proportion of faculty that is full-time,
#(RET_COMBINE, which is a combination of RET_PT4, RET_PTL4) First-time, part-time student retention rate,
#(WDRAW_ORIG_YR3_RT) Percent withdrawn from original institution within 3 years

SuperScoreData <- CollegeScoreCard[c(1,15,19,13,25,101,102,107,111,132,135,138,139,151,159,165)];

#The idea will be to find the mean and median for each variable.
#Then, we can find how many standard deviations each school is above/below the mean, in order to assign "SuperScore" points
#These points are added together to form the "SuperScore", which is a way to compare institution against each other in those categories.

#Find mean, standard deviation, and plot a histogram for CDR2
CDR2_Data <- as.numeric(as.character(SuperScoreData$CDR2));
CDR2_Mean <- mean(CDR2_Data, na.rm=TRUE);
CDR2_SD <- sd(CDR2_Data, na.rm=TRUE);
hist(CDR2_Data, breaks=40);

#Find mean, standard deviation, and plot a histogram for CDR3
CDR3_Data <- as.numeric(as.character(SuperScoreData$CDR3));
CDR3_Mean <- mean(CDR3_Data, na.rm=TRUE);
CDR3_SD <- sd(CDR3_Data, na.rm=TRUE);
hist(CDR3_Data, breaks=40);

#Find mean, standard deviation, and plot a histogram for MN_EARN_WNE_P10
MN_EARN_Data <- as.numeric(as.character(SuperScoreData$MN_EARN_WNE_P10));
MN_EARN_Mean <- mean(MN_EARN_Data, na.rm=TRUE);
MN_EARN_SD <- sd(MN_EARN_Data, na.rm=TRUE);
hist(MN_EARN_Data, breaks=40);

#Find mean, standard deviation, and plot a histogram for GRAD_DEBT_MDN_SUPP
GRAD_DEBT_Data <- as.numeric(as.character(SuperScoreData$GRAD_DEBT_MDN_SUPP));
GRAD_DEBT_Mean <- mean(GRAD_DEBT_Data, na.rm=TRUE);
GRAD_DEBT_SD <- sd(GRAD_DEBT_Data, na.rm=TRUE);
hist(GRAD_DEBT_Data, breaks=40);

#Find mean, standard deviation, and plot a histogram for TUITIONFEE_IN
TUIT_IN_Data <- as.numeric(as.character(SuperScoreData$TUITIONFEE_IN));
TUIT_IN_Mean <- mean(TUIT_IN_Data, na.rm=TRUE);
TUIT_IN_SD <- sd(TUIT_IN_Data, na.rm=TRUE);
hist(TUIT_IN_Data, breaks=40);

#Find mean, standard deviation, and plot a histogram for TUITIONFEE_OUT
TUIT_OUT_Data <- as.numeric(as.character(SuperScoreData$TUITIONFEE_OUT));
TUIT_OUT_Mean <- mean(TUIT_OUT_Data, na.rm=TRUE);
TUIT_OUT_SD <- sd(TUIT_OUT_Data, na.rm=TRUE);
hist(TUIT_OUT_Data, breaks=40);

#Find mean, standard deviation, and plot a histogram for RET_FT_COMBINE
RET_FT_Data <- as.numeric(as.character(SuperScoreData$RET_FT_COMBINE));
RET_FT_Mean <- mean(RET_FT_Data, na.rm=TRUE);
RET_FT_SD <- sd(RET_FT_Data, na.rm=TRUE);
hist(RET_FT_Data, breaks=40);

#Find mean, standard deviation, and plot a histogram for C150_COMBINE
C150_Data <- as.numeric(as.character(SuperScoreData$C150_COMBINE));
C150_Mean <- mean(C150_Data, na.rm=TRUE);
C150_SD <- sd(C150_Data, na.rm=TRUE);
hist(C150_Data, breaks=40);

#Find mean, standard deviation, and plot a histogram for PFTFAC
PFTFAC_Data <- as.numeric(as.character(SuperScoreData$PFTFAC));
PFTFAC_Mean <- mean(PFTFAC_Data, na.rm=TRUE);
PFTFAC_SD <- sd(PFTFAC_Data, na.rm=TRUE);
hist(PFTFAC_Data, breaks=40);

#Find mean, standard deviation, and plot a histogram for RET_COMBINE
RET_Data <- as.numeric(as.character(SuperScoreData$RET_COMBINE));
RET_Mean <- mean(RET_Data, na.rm=TRUE);
RET_SD <- sd(RET_Data, na.rm=TRUE);
hist(RET_Data, breaks=40);

#Find mean, standard deviation, and plot a histogram for WDRAW_ORIG_YR3_RT
WDRAW_Data <- as.numeric(as.character(SuperScoreData$WDRAW_ORIG_YR3_RT));
WDRAW_Mean <- mean(WDRAW_Data, na.rm=TRUE);
WDRAW_SD <- sd(WDRAW_Data, na.rm=TRUE);
hist(WDRAW_Data, breaks=40);


#Next, we will find the Z-Scores for each of the above variables, which will help us add points to form the "SuperScores"
CDR2_Z_Score <- (CDR2_Data - CDR2_Mean)/CDR2_SD;
CDR3_Z_Score <- (CDR3_Data - CDR3_Mean)/CDR3_SD;
MN_EARN_Z_Score <- (MN_EARN_Data - MN_EARN_Mean)/MN_EARN_SD;
GRAD_DEBT_Z_Score <- (GRAD_DEBT_Data - GRAD_DEBT_Mean)/GRAD_DEBT_SD;
TUIT_IN_Z_Score <- (TUIT_IN_Data - TUIT_IN_Mean)/TUIT_IN_SD;
TUIT_OUT_Z_Score <- (TUIT_OUT_Data - TUIT_OUT_Mean)/TUIT_OUT_SD;
RET_FT_Z_Score <- (RET_FT_Data - RET_FT_Mean)/RET_FT_SD;
C150_Z_Score <- (C150_Data - C150_Mean)/C150_SD;
PFTFAC_Z_Score <- (PFTFAC_Data - PFTFAC_Mean)/PFTFAC_SD;
RET_Z_Score <- (RET_Data - RET_Mean)/RET_SD;
WDRAW_Z_Score <- (WDRAW_Data - WDRAW_Mean)/WDRAW_SD;

#To be continued to find the "SuperScore" values......


#While we try to figure out that portion of the code, we have found other interesting data points relating to our dataset.

#State Distribution of the schools in our dataset
States_Data = SuperScoreData$ST_FIPS_DESC;
States_Data_freq = table(States_Data);
colors = c("red","yellow","green","violet","orange","blue","pink","cyan");
barplot(States_Data_freq, col=colors);

#Barplot the mean of the median debt of graduates by State
barplot(tapply(as.numeric(as.character(SuperScoreData$GRAD_DEBT_MDN_SUPP)), SuperScoreData$ST_FIPS_DESC, mean, na.rm=TRUE),col=colors);

# Frequency distribution of the college sizes in Maryland
mdstate = States_Data == "Maryland";
mddata = SuperScoreData[mdstate,];
mdccsize = mddata$CCSIZE_DESC;
mdccsize.freq = table(mdccsize);
cbind(mdccsize.freq);
pie(mdccsize.freq, col=colors);

# Plot In-state Tuition against Out-Of-State Tuition in Maryland
plot(as.numeric(as.character(mddata$TUITIONFEE_OUT)), as.numeric(as.character(mddata$TUITIONFEE_IN)))


# Plot in-state tuition against median debt of students who graduate in Maryland
plot(as.numeric(as.character(mddata$TUITIONFEE_IN)), as.numeric(as.character(mddata$GRAD_DEBT_MDN_SUPP)))


# plot in-state tuition against mean earning of students who graduate in Maryland
plot(as.numeric(as.character(mddata$TUITIONFEE_IN)), as.numeric(as.character(mddata$MN_EARN_WNE_P10)))










