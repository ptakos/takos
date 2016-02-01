library(plyr)
library(ggplot2)


sbi <- read.csv("SBI.csv", header = TRUE)


sbi$CSP_Code <- as.factor(sbi$CSP_Code)
sbi$SBI_Account <- as.factor(sbi$SBI_Account)
summary(sbi)


rpt_csp_code <- sbi[which(duplicated(sbi$CSP_Code)),]
rpt_csp_code <- sbi[which(sbi$CSP_Code %in% rpt_csp_code$CSP_Code),]
rpt_customer <- rpt_csp_code[which(duplicated(rpt_csp_code$SBI_Account)),]
rpt_customer <- rpt_csp_code[which(rpt_csp_code$SBI_Account %in% rpt_customer$SBI_Account),]
head(rpt_customer[order(rpt_customer$SBI_Account),], 10)
summary(rpt_customer)
dim(rpt_customer)
head(rpt_csp_code[which(rpt_csp_code$SBI_Account %in% rpt_customer$SBI_Account),], 5)


rpt_target_account <- count(rpt_customer, vars = "SBI_Account")
target_account_frq <- count(rpt_target_account, vars = "freq")
colnames(target_account_frq) <- c("RepeatTimes", "Count")


qplot(RepeatTimes, Count, data=rpt_customer, geom = c("line","point"))
qplot(RepeatTimes, Count, data=target_account_frq)
qplot(RepeatTimes, Count, data=target_account_frq, size=Count)
qplot(RepeatTimes, Count, data=target_account_frq, geom = c("line","point"))
