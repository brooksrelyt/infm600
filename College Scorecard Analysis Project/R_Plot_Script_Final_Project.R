# Use getwd() to find the R working directory
# Copy College_ScoreCard_Raw_Date_v5.csv into the working directory
# Read and view the College Scorecard raw data of colleges within Mid East Region and Virginia
CollegeScoreCard<-read.csv("College_ScoreCard_Raw_Data_v5.csv",header = TRUE);
View(CollegeScoreCard);

#run these if you do not have the packages installed
#they will be useful for ggplot2 graphs
install.packages("ggplot2");
install.packages("Hmisc");
install.packages("dplyr");
install.packages("reshape");
install.packages("lme4");
#load libraries into R session
library(ggplot2);
library(Hmisc);
library(dplyr);
library(reshape);
library(lme4);
library(nlme);
theme_set(theme_grey(base_size = 12));

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
hist(CDR2_Data, breaks=40, main="Histogram of 2yrs default rate",xlab="2yrs default rate");

#Find mean, standard deviation, and plot a histogram for CDR3
CDR3_Data <- as.numeric(as.character(SuperScoreData$CDR3));
CDR3_Mean <- mean(CDR3_Data, na.rm=TRUE);
CDR3_SD <- sd(CDR3_Data, na.rm=TRUE);
hist(CDR3_Data, breaks=40, main="Histogram of 3yrs default rate", xlab="3yrs default rate");

#Find mean, standard deviation, and plot a histogram for MN_EARN_WNE_P10
MN_EARN_Data <- as.numeric(as.character(SuperScoreData$MN_EARN_WNE_P10));
MN_EARN_Mean <- mean(MN_EARN_Data, na.rm=TRUE);
MN_EARN_SD <- sd(MN_EARN_Data, na.rm=TRUE);
hist(MN_EARN_Data, breaks=40, main="Histogram of average earnings 10yrs after graduating",xlab="average earnings 10yrs after grad");

#Find mean, standard deviation, and plot a histogram for GRAD_DEBT_MDN_SUPP
GRAD_DEBT_Data <- as.numeric(as.character(SuperScoreData$GRAD_DEBT_MDN_SUPP));
GRAD_DEBT_Mean <- mean(GRAD_DEBT_Data, na.rm=TRUE);
GRAD_DEBT_SD <- sd(GRAD_DEBT_Data, na.rm=TRUE);
hist(GRAD_DEBT_Data, breaks=40, main="Histogram of Median Debt of Graduates",xlab="Median Debt of Graduates");

#Find mean, standard deviation, and plot a histogram for TUITIONFEE_IN
TUIT_IN_Data <- as.numeric(as.character(SuperScoreData$TUITIONFEE_IN));
TUIT_IN_Mean <- mean(TUIT_IN_Data, na.rm=TRUE);
TUIT_IN_SD <- sd(TUIT_IN_Data, na.rm=TRUE);
hist(TUIT_IN_Data, breaks=40, main="Histogram of In-State Tuition",xlab="In-State Tuition");

#Find mean, standard deviation, and plot a histogram for TUITIONFEE_OUT
TUIT_OUT_Data <- as.numeric(as.character(SuperScoreData$TUITIONFEE_OUT));
TUIT_OUT_Mean <- mean(TUIT_OUT_Data, na.rm=TRUE);
TUIT_OUT_SD <- sd(TUIT_OUT_Data, na.rm=TRUE);
hist(TUIT_OUT_Data, breaks=40, main="Histogram of Out-Of-State Tuition", xlab="Out-of-State Tuition");

#Find mean, standard deviation, and plot a histogram for RET_FT_COMBINE
RET_FT_Data <- as.numeric(as.character(SuperScoreData$RET_FT_COMBINE));
RET_FT_Mean <- mean(RET_FT_Data, na.rm=TRUE);
RET_FT_SD <- sd(RET_FT_Data, na.rm=TRUE);
hist(RET_FT_Data, breaks=40, main="Histogram of Full-time Retention Rate", xlab="First-time Full-time Student Retention Rate");

#Find mean, standard deviation, and plot a histogram for C150_COMBINE
C150_Data <- as.numeric(as.character(SuperScoreData$C150_COMBINE));
C150_Mean <- mean(C150_Data, na.rm=TRUE);
C150_SD <- sd(C150_Data, na.rm=TRUE);
hist(C150_Data, breaks=40, main="Histogram of Completion Rate",xlab="First-time Full-time Completion Rate");

#Find mean, standard deviation, and plot a histogram for PFTFAC
PFTFAC_Data <- as.numeric(as.character(SuperScoreData$PFTFAC));
PFTFAC_Mean <- mean(PFTFAC_Data, na.rm=TRUE);
PFTFAC_SD <- sd(PFTFAC_Data, na.rm=TRUE);
hist(PFTFAC_Data, breaks=40,main="Histogram of Proportion of Full-time Faculty Staff",xlab="Proportion of Faculty that is Full-time");

#Find mean, standard deviation, and plot a histogram for RET_COMBINE
#We ended up not using this vairable for our Academic Student Success SuperScore, because it had too many NA values
RET_Data <- as.numeric(as.character(SuperScoreData$RET_COMBINE));
RET_Mean <- mean(RET_Data, na.rm=TRUE);
RET_SD <- sd(RET_Data, na.rm=TRUE);

#Find mean, standard deviation, and plot a histogram for WDRAW_ORIG_YR3_RT
WDRAW_Data <- as.numeric(as.character(SuperScoreData$WDRAW_ORIG_YR3_RT));
WDRAW_Mean <- mean(WDRAW_Data, na.rm=TRUE);
WDRAW_SD <- sd(WDRAW_Data, na.rm=TRUE);
hist(WDRAW_Data, breaks=40, main="Histogram of percent withdrawn from orig inst within 3yrs",xlab="Percent Withdrawn from orig inst within 3yrs");

#Next, we will find the Z-Scores for each of the above variables, which will help us to form the "SuperScores"
CollegeScoreCard$CDR2_Z_Score <- (CDR2_Data - CDR2_Mean)/CDR2_SD;
CollegeScoreCard$CDR3_Z_Score <- (CDR3_Data - CDR3_Mean)/CDR3_SD;
CollegeScoreCard$MN_EARN_Z_Score <- (MN_EARN_Data - MN_EARN_Mean)/MN_EARN_SD;
CollegeScoreCard$GRAD_DEBT_Z_Score <- (GRAD_DEBT_Data - GRAD_DEBT_Mean)/GRAD_DEBT_SD;
CollegeScoreCard$TUIT_IN_Z_Score <- (TUIT_IN_Data - TUIT_IN_Mean)/TUIT_IN_SD;
CollegeScoreCard$TUIT_OUT_Z_Score <- (TUIT_OUT_Data - TUIT_OUT_Mean)/TUIT_OUT_SD;
CollegeScoreCard$RET_FT_Z_Score <- (RET_FT_Data - RET_FT_Mean)/RET_FT_SD;
CollegeScoreCard$C150_Z_Score <- (C150_Data - C150_Mean)/C150_SD;
CollegeScoreCard$PFTFAC_Z_Score <- (PFTFAC_Data - PFTFAC_Mean)/PFTFAC_SD;
CollegeScoreCard$RET_Z_Score <- (RET_Data - RET_Mean)/RET_SD;
CollegeScoreCard$WDRAW_Z_Score <- (WDRAW_Data - WDRAW_Mean)/WDRAW_SD;

#Adding Z-Score variable categories together to make overall "Super Scores"

#Financial Stability SuperScore
CollegeScoreCard$Financial_Stability_SuperScore <- (CollegeScoreCard$CDR2_Z_Score*-1) + (CollegeScoreCard$CDR3_Z_Score*-1) + (CollegeScoreCard$MN_EARN_Z_Score)
+ (CollegeScoreCard$GRAD_DEBT_Z_Score*-1) + (CollegeScoreCard$TUIT_IN_Z_Score*-1) + (CollegeScoreCard$TUIT_OUT_Z_Score*-1);

#Academic Student Success SuperScore
CollegeScoreCard$Academic_Student_Success_SuperScore <- (CollegeScoreCard$RET_FT_Z_Score) + (CollegeScoreCard$C150_Z_Score)
+ (CollegeScoreCard$PFTFAC_Z_Score) + (CollegeScoreCard$WDRAW_Z_Score*-1);

#State Distribution of the number of schools in our dataset
States_Data = SuperScoreData$ST_FIPS_DESC;
States_Data_freq = table(States_Data);
colors = c("red","yellow","green","violet","orange","blue","pink","cyan");
barplot(States_Data_freq, col=colors);
# Barplot of the mean of graduate debt by states within the region
barplot(tapply(as.numeric((as.character(SuperScoreData$GRAD_DEBT_MDN_SUPP))), SuperScoreData$ST_FIPS_DESC, mean, na.rm=TRUE), main="Mean of the grad debt by states within the region", ylab="Mean of Grad Debt",col=colors);

#Barplot the mean of in-state tuition by states within the region
barplot(tapply(as.numeric(as.character(SuperScoreData$TUITIONFEE_IN)), SuperScoreData$ST_FIPS_DESC, mean, na.rm=TRUE), main="Mean of In-State Tuition by States within the Region", ylab="Mean of In-State Tuition", col=colors);

#Barplot the mean of out-of-state tuition by states within the region
barplot(tapply(as.numeric(as.character(SuperScoreData$TUITIONFEE_OUT)), SuperScoreData$ST_FIPS_DESC, mean, na.rm=TRUE), main="Mean of Out-Of-State Tuition by States within the Region", ylab="Mean of Out-Of-State Tuition", col=colors);

#This uses the ggplot2 package to display a graph that shows a correlation between Average ACT scores 
# for admitted students and the financial stability associated with students attending a specific school
ggplot(data = CollegeScoreCard, aes(ACTCM75, y=Financial_Stability_SuperScore)) +
  xlab("ACT Composite 75th Percentile") +
  ylab("Financial Stability SuperScore") +
  geom_boxplot() +
  ggtitle("Admitted Student ACT Composite 75th Percentile and Financial Stability") +
  theme(plot.title = element_text(hjust = 0.5));

#This uses the ggplot2 package to display a graph that shows a correlation between Average ACT scores 
# for admitted students and the academic student success SuperScore associated with students attending a specific school
ggplot(data = CollegeScoreCard, aes(ACTCM75, y=Academic_Student_Success_SuperScore)) +
  xlab("ACT Composite 75th Percentile") +
  ylab("Academic Student Success SuperScore") +
  geom_boxplot() +
  ggtitle("Admitted Student ACT Composite 75th Percentile and Academic Student Success") +
  theme(plot.title = element_text(hjust = 0.5));

# Frequency distribution of the college sizes in Maryland
mdstate = States_Data == "Maryland";
mddata = SuperScoreData[mdstate,];
mdccsize = mddata$CCSIZE_DESC;
mdccsize.freq = table(mdccsize);
cbind(mdccsize.freq);
pie(mdccsize.freq, col=colors);

# Plot In-state Tuition against Out-Of-State Tuition in Maryland
plot(as.numeric(as.character(mddata$TUITIONFEE_OUT)), as.numeric(as.character(mddata$TUITIONFEE_IN)),main="Maryland Tuition Comparison", xlab="Out-Of-State Tuition", ylab="In-State Tution");

# Plot in-state tuition against median debt of graduates in Maryland
plot(as.numeric(as.character(mddata$TUITIONFEE_IN)), as.numeric(as.character(mddata$GRAD_DEBT_MDN_SUPP)),main="Maryland In-State Tuition vs Median Debt of Graduates", xlab="In-State Tuition",ylab="Median Debt of Graduates");

# plot in-state tuition against mean earning of graduates in Maryland
plot(as.numeric(as.character(mddata$TUITIONFEE_IN)), as.numeric(as.character(mddata$MN_EARN_WNE_P10)), main="Maryland In-State Tuition vs Mean Earning of Graduates", xlab="In-State-Tuition",ylab="Mean Earning of Graduates");
