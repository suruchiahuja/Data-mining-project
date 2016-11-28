german_credit <- read.csv("C:/Users/Suruchi Ahuja/Desktop/R prog/New folder/546 Final Project/german_credit.csv")
#View(german_credit)

Train50 <- read.csv("C:/Users/Suruchi Ahuja/Desktop/R prog/New folder/546 Final Project/Training50.csv")
#View(Train50)

Test50 <- read.csv("C:/Users/Suruchi Ahuja/Desktop/R prog/New folder/546 Final Project/Test50.csv")
#View(Test50)

LogisticModel50 <- glm(Creditability ~ Account.Balance + Payment.Status.of.Previous.Credit + Purpose + Value.Savings.Stocks + Length.of.current.employment + Sex...Marital.Status + Most.valuable.available.asset + Type.of.apartment + Concurrent.Credits + Duration.of.Credit..month.+ Credit.Amount + Age..years., family=binomial, data = Train50)
LogisticModel50final <- glm(Creditability ~ Account.Balance + Payment.Status.of.Previous.Credit + Purpose + Length.of.current.employment + Sex...Marital.Status, family=binomial, data = Train50)
fit50 <- fitted.values(LogisticModel50S1)
Threshold50 <- rep(0,500)
for (i in 1:500)
  if(fit50[i] >= 0.5) Threshold50[i] <- 1

CrossTable(Train50$Creditability, Threshold50, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=F, data=Train50)

perf <- performance(pred, "tpr", "fpr")
plot(perf)