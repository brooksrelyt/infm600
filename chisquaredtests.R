# TESTING:::: (CDR2) 2-year loan default rate vs. First-time, full-time student retention rate. (RET_FT_COMBINE)

# Ho: There is no relationship between 2 year loan default rate and First-time, full-time student retention rate
# Ha: There is a relationship " " " " " " "

# Students with a high 2-year loan default rate have a lower first-time, full-time retention rate.

# T-test and p-value

obs <- table(CollegeScoreCard$CDR2,CollegeScoreCard$RET_FT_COMBINE)

chisq = chisq.test(obs)
chisq
obs
chisq$expected



# "Financial Stability" SuperScore
# 1. (TUITIONFEE_IN) in state tuition costs vs. (CDR3) 3 year default rate

# Ho: There is no relationship between in state tuition costs and 3 year default rate
# Ha: There is a relationship " " " " " " "

# Conclusion: Because our p-value is less than our standard alpha of 0.05 we should reject the null hypothesis. 
# Students with higher in state tuition costs have a higher 3 year default rate.

# T-test and p-value

obs <- table(CollegeScoreCard$TUITIONFEE_IN,CollegeScoreCard$CDR3)

chisq = chisq.test(obs)
chisq
obs
chisq$expected

# Results:

#Pearson's Chi-squared test
# data:  obs
# X-squared = 121970, df = 116380, p-value < 2.2e-16

plot(as.numeric(as.character(CollegeScoreCard$GRAD_DEBT_MDN_SUPP)), as.numeric(as.character(CollegeScoreCard$MN_EARN_WNE_P10)))



# 2. (TUITIONFEE_OUT) Out-of-state tuition costs vs. (MN_EARN_WNE_P10) average earnings 10 years after graduating

# Ho: There is no relationship between Out-of-state tuition costs and average earnings 10 years after graduating
# Ha: There is a relationship " " " " " " "

# Conclusion: Because our p-value is less than our standard alpha of 0.05 we should reject the null hypothesis. 
# Students with higher out-of-state tuition costs have a higher average earnings 10 years after graduating.

# T-test and p-value

obs <- table(CollegeScoreCard$TUITIONFEE_OUT,CollegeScoreCard$MN_EARN_WNE_P10)

chisq = chisq.test(obs)
chisq
obs
chisq$expected

# Results:

#Pearson's Chi-squared test
# data:  obs
# X-squared = 217950, df = 205570, p-value < 2.2e-16






# 3. (TUITIONFEE_IN) in state tuition costs vs. (MN_EARN_WNE_P10) average earnings 10 years after graduating

# Ho: There is no relationship between in state tuition costs and average earnings 10 years after graduating
# Ha: There is a relationship " " " " " " "

# Conclusion: Because our p-value is less than our standard alpha of 0.05 we should reject the null hypothesis. 
# Students with higher in state tuition costs have a higher average earnings 10 years after graduating.

# T-test and p-value

obs <- table(CollegeScoreCard$TUITIONFEE_IN,CollegeScoreCard$MN_EARN_WNE_P10)

chisq = chisq.test(obs)
chisq
obs
chisq$expected

# Results:

#Pearson's Chi-squared test
# data:  obs
# X-squared = 216190, df = 203210, p-value < 2.2e-16



# "Academic Student Success" SuperScore
# 1. (RET_FT_COMBINE) First-time, full-time student retention rate vs. (C150_COMBINE) Completion rate for first-time, full-time students

# Ho: There is no relationship between First-time, part-time student retention rate and Completion rate for first-time, full-time students
# Ha: There is a relationship " " " " " " "

# Conclusion: Because our p-value is less than our standard alpha of 0.05 we should reject the null hypothesis. 
# Students with higher in state tuition costs have a higher average earnings 10 years after graduating.

# T-test and p-value

obs2 <- table(CollegeScoreCard$RET_FT_COMBINE,CollegeScoreCard$C150_COMBINE)

chisq = chisq.test(obs2)
chisq
obs2
chisq$expected


