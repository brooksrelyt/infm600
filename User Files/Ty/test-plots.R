#This uses the ggplot2 package to display a graph that shows a correlation between Average ACT scores 
# for admitted students and the financial stability associated with students attending a specific school
ggplot(data = CollegeScoreCard, aes(ACTCM75, y=Financial_Stability_SuperScore)) +
  xlab("ACT Composite 75th Percentile") +
  ylab("Financial Stability SuperScore") +
  geom_boxplot() +
  ggtitle("Admitted Student ACT Composite 75th Percentile and Financial Stability") +
  theme(plot.title = element_text(hjust = 0.5))

#This uses the ggplot2 package to display a graph that shows a correlation between Average ACT scores 
# for admitted students and the academic student success SuperScore associated with students attending a specific school
ggplot(data = CollegeScoreCard, aes(ACTCM75, y=Academic_Student_Success_SuperScore)) +
  xlab("ACT Composite 75th Percentile") +
  ylab("Academic Student Success SuperScore") +
  geom_boxplot() +
  ggtitle("Admitted Student ACT Composite 75th Percentile and Academic Student Success") +
  theme(plot.title = element_text(hjust = 0.5))

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


# =================================
# Testing
# =================================

# This uses the ggplot2 package to display a graph that shows a correlation between Average ACT scores 
# for admitted students and the academic student success SuperScore associated with students attending a specific school

ggplot(data = SuperScoreData, aes(RET_FT_COMBINE, y=Academic_Student_Success_SuperScore)) +
  xlab("ACT Composite 75th Percentile") +
  ylab("Academic Student Success SuperScore") +
  geom_boxplot() +
  ggtitle("Admitted Student ACT Composite 75th Percentile and Academic Student Success") +
  theme(plot.title = element_text(hjust = 0.5))






