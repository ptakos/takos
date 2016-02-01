#install packages
install.packages("plyr")
install.packages("ggplot2")
install.packages("qdapTools")
install.packages("openxlsx")
install.packages("arules")
install.packages("arulesViz")
install.packages("data.table")


library(plyr)
library(ggplot2)
library(qdapTools)
library(openxlsx)
library(arules)
library(arulesViz)
library(data.table)


sbi_dec14 <- "DecD2C_2014.xlsx"
sbi_jan15 <- "JanD2C_2015.xlsx"
data_dec14 <- read.xlsx(sbi_dec14, 1)
data_jan15 <- read.xlsx(sbi_jan15, 1)


colnames(data_dec14)
colnames(data_jan15)[13] <- "Transaction.fee"
temp <- data_jan15[,(colnames(data_jan15) %in% colnames(data_dec14))]
sbi <- rbind(data_dec14, temp)


sbi$CSP_Code <- as.factor(sbi$CSP_Code)


rpt_sbi_agent <- count(sbi, vars=c("CSP_Code", "SBI_Account"))
rpt_sbi_agent.sorted <- rpt_sbi_agent[order(rpt_sbi_agent$freq, decreasing = T),]
rpt_sbi_agent.sorted$Agent_Account_CB <- paste(rpt_sbi_agent.sorted$CSP_Code,rpt_sbi_agent.sorted$SBI_Account)
rpt_sbi_agent.table <- data.table(rpt_sbi_agent.sorted)
setkey(rpt_sbi_agent.table, "CSP_Code", "SBI_Account")
sbi$Money_Laundry_fq <- rpt_sbi_agent.table[J(sbi$CSP_Code, sbi$SBI_Account), freq]
sbi$Agent_Account_CB <- paste(sbi$CSP_Code, sbi$SBI_Account)


more_than_nine <- rpt_sbi_agent.sorted[which(rpt_sbi_agent.sorted$freq>30),]
barplot(more_than_nine$freq, col="Cyan", xlab="Different Agent Customer Combination", ylab="# of Transactions", axisnames = F)


top_laundry_agents <- sbi[which(sbi$Money_Laundry_fq>=80),]
top_laundry_agents <- top_laundry_agents[order(top_laundry_agents$Money_Laundry_fq, decreasing = T),]
top_laundry_agents$Tx_Time <- convertToDateTime(top_laundry_agents$Tx_Time)
top_laundry_agents$WeekDay <- strftime(top_laundry_agents$Tx_Time, format="%u")
top_laundry_agents$YearWeek <- strftime(top_laundry_agents$Tx_Time, format="%y-%U")
top_laundry_agents$Money_Laundry_fq <- as.factor(top_laundry_agents$Money_Laundry_fq)
ggplot(top_laundry_agents, aes(x=YearWeek, y=Amount, fill=WeekDay)) + geom_bar(stat= "identity") + facet_grid(. ~ Money_Laundry_fq)
