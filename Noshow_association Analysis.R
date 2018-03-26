

noshow_dt  <- read.csv(file.choose(), header = TRUE, sep = ",",fileEncoding="UTF-8-BOM")



View(noshow_dt)



table(noshow_few$Appt_Wait_Time)

noshow_dt <- noshow_dt[!(noshow_dt$Age=="-1"),]

noshow= noshow_dt[,c(4,10,11,12,13,14,15,20,21,22,24,16)]

a1 <- c(-1,0,7,30,90,180)
b1 <- c("Same_Day","One_week","One_month", "Three_months", "Six_months")
noshow$Appt_Wait_Time <- ordered(cut(as.numeric(noshow$Awaiting_time), a1, labels = b1, include.lowest = FALSE, right = TRUE))


View(noshow)

# Creating the dataset for Association analysis
noshow_data <- noshow[,c(1,2,7,8,10,11,13,14,12)]

View(noshow_data)


noshow$Dsease_cnt = noshow$Diabetes+noshow$Hipertension+noshow$Alcoholism+noshow$Handcap

View(noshow)

# Factoring the data
noshow$Gender <- as.factor(noshow$Gender)
noshow$Age <- as.factor(noshow$Age)
noshow$Scholarship <- as.factor(noshow$Scholarship)
noshow$Appt_Day <- as.factor(noshow$Appt_Day)
noshow$Alcoholism <- as.factor(noshow$Alcoholism)
noshow$Handcap <- as.factor(noshow$Handcap)
noshow$SMS_received <- as.factor(noshow$SMS_received)
noshow$No.show <- as.factor(noshow$No.show)
noshow$Diabetes <- as.factor(noshow$Diabetes)
noshow$tm_of_day <- as.factor(noshow$tm_of_day)
noshow$Hipertension <- as.factor(noshow$Hipertension)
noshow$Dsease_cnt <-as.factor(noshow$Dsease_cnt)

names(noshow)

noshow_new = noshow[,c(1,2,7,8,10,11,13,14,12)]

View(noshow_new)

names(noshow_dt)


View(noshow_few)

table(noshow_new$tm_of_day)
table(noshow_new$No.show)
table(noshow_new$Gender)
table(noshow_new$Scholarship)
table(noshow_new$Appt_Day)


str(noshow_new)

names(noshow_new)

noshow_subset = noshow_new[,c(1,4,5,6,7,9)]

names(noshow_subset)

noshow_new

library(arules)

rules <- apriori(noshow_new,parameter = list(minlen=1,maxlen=15, supp=0.005, conf=0.3),appearance = list(rhs=c("No.show=Yes"),default="lhs"),control = list(verbose=F))

rules.sorted <- sort(rules2, by="lift")
inspect(head(rules.sorted))
#Pruning Redundant Rules

# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1

which(redundant)

rules.pruned <- rules.sorted[!redundant]

library(arulesViz)
plot(rules)

plot(rules, method="graph",max=10, control=list(type="items"))

plot(rules, method="paracoord",max=1, control=list(reorder=TRUE))

names(noshow_subset)

dim(noshow)
noshow_transaction <- as(noshow_subset, "transactions")

summary(noshow_transaction)

transactionInfo(noshow_transaction)

itemFrequencyPlot(noshow_transaction, support = 0.3, cex.names=0.8)

rules_withappt = rules

rules_tran <- apriori(noshow_transaction,parameter = list(minlen=4,maxlen=12, supp=0.01, conf=0.3),appearance = list(rhs=c("No.show=Yes","No.show=No"),default="lhs"),control = list(verbose=F))

rules_tran <- apriori(noshow_transaction,parameter = list(minlen=3,maxlen=12, supp=0.01, conf=0.3),appearance = list(rhs=c("No.show=Yes"),default="lhs"),control = list(verbose=F))


rulesNoShow <- subset(rules_tran, subset = rhs %in% "No.show=Yes" & lift > 1.2)


plot(rules_tran)


rules.sorted <- sort(rules_tran, by="lift")
inspect(rules.sorted)


write(rules_tran, file = "data.csv", sep = ",", col.names = NA)

plot(rulesNoShow)
plot(rules_tran, method = NULL, measure = "support", shading = "lift",interactive = FALSE, data = NULL, control = NULL, engine = "interactive")
plot(rulesNoShow, shading="order", control=list(main = "Two-key plot"))

plot(rules_tran, measure=c("support", "confidence"), shading="lift")

sel <- plot(rulesNoShow, measure=c("support", "lift"), shading="confidence", interactive=TRUE)

subrules2 <- head(sort(rulesNoShow, by="lift"), 10)
plot(subrules2, method="graph")

plot(rulesNoShow, method="grouped", control=list(k=15))



rulesShow <- subset(rules, subset = rhs %in% "No.show=No" & lift > 1)
plot(rulesShow)
rulesShow.sorted <- sort(rulesShow, by="lift")
inspect(head(rulesShow.sorted, n = 10, by = "lift"))

inspect(head(rulesShow.sorted, n = 10, by = "confidence"))









