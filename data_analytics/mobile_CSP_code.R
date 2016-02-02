#install packages
library(plyr)
library(ggplot2)
library(qdapTools)

sbi <- read.csv("SBI.csv", header = TRUE)

CSP_Code <- data.frame(unique(sbi$CSP_Code))
CSP_Circle_list <- data.frame(unique(sbi$CSP_Circle))
CSP_Name_list <- data.frame(unique(sbi$CSP_Name))
SCSP_Code_list <- data.frame(unique(sbi$SCSP_Code))

# unique Depositor_Mobile per CSP_Code
rpt_customer.csp_code.depositor_mb <- data.frame(rpt_customer$CSP_Code, rpt_customer$Depositor_Mobile)
tbRpCs <- unique(rpt_customer.csp_code.depositor_mb)
tbRpCSC <- count(tbRpCs, vars = "rpt_customer.CSP_Code")
qplot(rpt_customer.CSP_Code, data=tbRpCs, geom="histogram")
