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
