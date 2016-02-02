library(plyr)
library(ggplot2)
library(qdapTools)
library(openxlsx)
library(arules)
library(arulesViz)

sbi <- read.csv("SBI.csv", header = TRUE)
colnames(sbi)[13] <- "Transaction.fee"

sbi$CSP_Code <- as.factor(sbi$CSP_Code)

rpt_customer <- sbi[which(duplicated(sbi$Depositor_Mobile)),]
rpt_customer <- sbi[which(sbi$Depositor_Mobile %in% rpt_customer$Depositor_Mobile),]
rpt_customer <- rpt_customer[order(rpt_customer$Depositor_Mobile, rpt_customer$Tx_Time),]
txAna <- c(round((nrow(sbi)-nrow(rpt_customer))/nrow(sbi)*100, 2), round(nrow(rpt_customer)/nrow(sbi)*100,2))
txCount <- c((nrow(sbi)-nrow(rpt_customer)), nrow(rpt_customer))
lbls <- c(paste("From One-time Customer\n",txCount[1],",", txAna[1],"%"), paste("From Repeat Customer\n", txCount[2] ,",", txAna[2],"%"))
pie(txAna, labels=lbls, main="Transaction Source")

agents_rpt_customer <- data.frame(rpt_customer$CSP_Code, rpt_customer$Depositor_Mobile)
agents_rpt_customer <- unique(agents_rpt_customer)
agents_per_customer <- count(agents_rpt_customer, vars="rpt_customer.Depositor_Mobile")

ggplot(agents_per_customer, aes(agents_per_customer$freq)) + 
  geom_histogram(binwidth = 1, aes(fill=..count..)) +
  xlab("# of Agents") +
  ylab("# of Customers")

ag_per_cust_more_than_one <- agents_per_customer[which(agents_per_customer$freq>1),]
fickle_customers <- nrow(ag_per_cust_more_than_one)
loyal_customers <- nrow(agents_per_customer) - fickle_customers
percent_fickle_customers <- round(fickle_customers/(fickle_customers+loyal_customers)*100, 2)
percent_loyal_customers <- round(loyal_customers/(fickle_customers+loyal_customers)*100, 2)
lbls <- c(paste("Loyal Customer\n",loyal_customers,",", percent_loyal_customers,"%"),
          paste("Fickle Customer\n", fickle_customers ,",", percent_fickle_customers,"%"))
pie(c(loyal_customers, fickle_customers), labels=lbls, main="Customers", col=topo.colors(2))

# histrogram of # of agents per customer
custTableGlance <- data.frame(table(agents_per_customer$freq))
colnames(custTableGlance) <- c('# of Agents', 'Customer Count')

rpt_customer$numOfSwitch <- lookup(rpt_customer$Depositor_Mobile, agents_per_customer)
fickle_customers_transactions <- nrow(rpt_customer[which(rpt_customer$numOfSwitch>1),])
loyal_customers_transactions <- nrow(rpt_customer[which(rpt_customer$numOfSwitch==1),])
fickle_customers_transactions_percentage <- round(fickle_customers_transactions/nrow(rpt_customer)*100, 2)
loyal_customers_transactions_percentage <- round(loyal_customers_transactions/nrow(rpt_customer)*100, 2)
lbls <- c(paste("By Loyal Customer\n",loyal_customers_transactions,",", loyal_customers_transactions_percentage,"%"),
          paste("By Fickle Customer\n", fickle_customers_transactions , ",", fickle_customers_transactions_percentage,"%"))

pie(c(loyal_customers_transactions, fickle_customers_transactions), labels=lbls, main="Transaction Source", col=c("green","yellow"))
rpt_customer$Tx_Time <- convertToDateTime(rpt_customer$Tx_Time)

#sbi for agents who are switched
switched_agents <- rpt_customer[which(rpt_customer$numOfSwitch>1),]
#NthAgent ===> Nth agent for the same customer.
customer_name <- 0
agent_name <- 0
agent_list <- data.frame(name="")
for(i in 1:nrow(switched_agents)){
  if(switched_agents[i,"Depositor_Mobile"]!=customer_name){
    customer_name <- switched_agents[i, "Depositor_Mobile"]
    agent_name <- switched_agents[i, "CSP_Code"]
    agent_list <- data.frame(name=agent_name)
    switched_agents[i, "switchMove"] <- "First"
    switched_agents[i, "NthAgent"] <- 1
  }else{
    tmp_agent_name <- switched_agents[i, "CSP_Code"]
    if(tmp_agent_name==agent_name){
      switched_agents[i, "switchMove"] <- "The Same"
      switched_agents[i, "NthAgent"] <- which(agent_list$name==tmp_agent_name)
    }else{
      if((tmp_agent_name %in% agent_list$name)){
        switched_agents[i, "switchMove"] <- "Go Back"
        switched_agents[i, "NthAgent"] <- which(agent_list$name==tmp_agent_name)
      }
      else{
        switched_agents[i, "switchMove"] <- "Switch"
        switched_agents[i, "NthAgent"] <- nrow(agent_list)+1
        agent_list[nrow(agent_list)+1, "name"] <- tmp_agent_name
      }
      agent_name <- tmp_agent_name
    }
  }
}

switched_agents_table <- switched_agents[,c("Tx_Time", "CSP_Code", "Depositor_Mobile", "switchMove", "NthAgent")]
transaction_switch <- data.frame(lapply(switched_agents, factor))

#add feature list
CSP_Code <- data.frame(unique(sbi$CSP_Code))
CSP_Circle_list <- data.frame(unique(sbi$CSP_Circle))
CSP_Name_list <- data.frame(unique(sbi$CSP_Name))
SCSP_Name_list <- data.frame(unique(sbi$SCSP_Name))
SCSP_Code_list <- data.frame(unique(sbi$SCSP_Code))
customer_id <- data.frame(unique(sbi$Depositor_Mobile))


ggplot(transaction_switch, aes(x=SCSP_Code,fill=switchMove)) + geom_histogram()

ggplot(transaction_switch, aes(x=CSP_Circle,fill=switchMove)) + geom_histogram()

#Try Agent ID
first_agent <- count(transaction_switch[which(transaction_switch$switchMove=="First"),], vars="CSP_Code")
same_agent <- count(transaction_switch[which(transaction_switch$switchMove=="The Same"),], vars="CSP_Code")
switched_agent <- count(transaction_switch[which(transaction_switch$switchMove=="Switch"),], vars="CSP_Code")
return_agent <- count(transaction_switch[which(transaction_switch$switchMove=="Go Back"),], vars="CSP_Code")

CSP_Code$First_Agent <- lookup(CSP_Code[,1], first_agent)
CSP_Code$Same_Agent <- lookup(CSP_Code[,1], same_agent)
CSP_Code$Switched_Agent <- lookup(CSP_Code[,1], switched_agent)
CSP_Code$Return_Agent <- lookup(CSP_Code[,1], return_agent)

CSP_Code[is.na(CSP_Code)] <- 0
CSP_Code$total <- rowSums(CSP_Code[,2:5])
CSP_Code <- CSP_Code[which(CSP_Code$total!=0),]

CSP_Code[,2:5] <- CSP_Code[,2:5]/CSP_Code[,ncol(CSP_Code)]

CSP_Code <- CSP_Code[order(CSP_Code$First_Agent),]
barplot(t(CSP_Code[,2:5]),col=rainbow(7), xlab="Agent ID", ylab="Tx Type (%)", axisnames = F)

CSP_Code <- CSP_Code[order(CSP_Code$First_Agent),]
barplot(CSP_Code$First_Agent,col="Red", xlab="Agent ID", ylab="Tx Type (%)", axisnames = F)

CSP_Code <- CSP_Code[order(CSP_Code$Same_Agent),]
barplot(CSP_Code$Same_Agent,col="Yellow", xlab="Agent ID", ylab="Tx Type (%)", axisnames = F)

CSP_Code <- CSP_Code[order(CSP_Code$Switched_Agent),]
barplot(CSP_Code$Switched_Agent,col="Green", xlab="Agent ID", ylab="Tx Type (%)", axisnames = F)

CSP_Code <- CSP_Code[order(CSP_Code$Return_Agent),]
barplot(CSP_Code$Return_Agent,col="Cyan", xlab="Agent ID", ylab="Tx Type (%)", axisnames = F)

plot(c(1))
legend(1,1, c("First","Same","Switch","Go Back"),fill=rainbow(7))

transaction_switch$Amount_Group <- trunc(as.numeric(as.character(transaction_switch$Amount))/1000) * 1000
ggplot(transaction_switch, aes(Amount_Group, fill=switchMove)) + geom_histogram()
ggplot(transaction_switch, aes(Source, fill=switchMove)) + geom_histogram()

transaction_switch$Fee_Rate <- round(as.numeric(as.character(transaction_switch$Transaction.fee))/as.numeric(as.character(transaction_switch$Amount)), 2)
ggplot(transaction_switch, aes(Fee_Rate, fill=switchMove)) + geom_histogram()

transaction_switch$Com_Rate <- round(1-as.numeric(as.character(transaction_switch$Income))/as.numeric(as.character(transaction_switch$Transaction.fee)), 2)
ggplot(transaction_switch, aes(Com_Rate, fill=switchMove)) + geom_histogram()
