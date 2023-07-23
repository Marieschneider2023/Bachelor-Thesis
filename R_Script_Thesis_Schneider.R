#deskriptive Analyse Boxplot successful vs unsuccessful
setwd("C:/Users/Marie/OneDrive/Dokumente")
#Boxplots of specimen age ~ success of gut content sequencing:
successrate <-read.table("boxplot2804.txt",header=TRUE,row.names=1)
info <- boxplot(age ~ success, data=successrate, yaxp = c(0, 160, 16))
# info about the boxplots
median_yes <- info$stats[2, 1]
median_no <- info$stats[2, 2]
mean_yes <- mean(successrate$age[successrate$success == "successful"])
mean_no <- mean(successrate$age[successrate$success == "unsuccessful"])
to_agg<-read.table("merge_orders.txt")
agg<-aggregate(to_agg[,2:68],by=list(to_agg$V1), sum)
write.table(agg,"OTUtab_orders.txt",sep="\t")
#
#
#1. Chi square test: age classes and total OTU counts
#total OTU count
altersklassen <- c("A", "B", "C")
counts <- c(6, 17, 4)
df <- data.frame(cbind(altersklassen = as.factor(altersklassen),
                       counts = as.factor(counts)))
df
chisq <- chisq.test(x = counts, p = c(1/3, 1/3, 1/3))
chisq
# The chi square test showed that the observed counts per age class differ significantly from
# the expected counts per age class (chi square (2) = 27.25, p < .001)
#
#2. chi square test: age classes and orders
#orders
data4 <- read.table("total_orders.txt", header = TRUE)
data4$age_class <- as.factor(data4$age_class)
data4$order <- as.factor(data4$order)
levels(data4$order)
View(data4)
chisq.test(data4$age_class, data4$order)
# chi square(12) = 6.55, p > .05
# no significant differences of order in age class
#
#
#
#Creating Boxplot and stacked bar chart: Composition by age class
setwd("C:/Users/Marie/OneDrive/Dokumente")
#Boxplots of specimen age ~ success of gut content sequencing:
successrate <-read.table("boxplot2804.txt",header=TRUE,row.names=1)
boxplot(age ~ success, data=successrate)
to_agg<-read.table("merge_orders.txt")
agg<-aggregate(to_agg[,2:36],by=list(to_agg$V1), sum)
write.table(agg,"OTUtab_orders.txt",sep="\t")
to_agg<-read.table("merge.txt")
agg<-aggregate(to_agg[,2:19],by=list(to_agg$V1), sum)
write.table(agg,"ageclass_orders.txt",sep="\t")
#make stacked barplots of order composition by age 
install.packages("ggplot2")
library(ggplot2)
orders<-read.table("bars_ageclass_orders.txt", header = TRUE)
bars<-ggplot(orders, aes(fill=order, y=order_reads, x=age_class)) +
  geom_bar(position="stack", stat="identity") +
  theme_minimal() +
  labs(x="age_class", y="order_reads")
#
#
#
#Creating Boxplot and ANOVA, influence age on success
install.packages("ggplot2")
library(ggplot2)
data <- read.table("boxplot.cor.txt", header = TRUE)
View(data)
ggplot(data, aes(x = success, y = age))+
  geom_boxplot()
ggplot(data, aes(x=age))+
  geom_histogram()+
  facet_wrap(~success, ncol = 1)
# standard deviation/mean of age per group (success yes vs. no) 
aggregate(data$age, by=list(data$success), FUN = mean) 
aggregate(data$age, by=list(data$success), FUN = sd) 
model <- aov(age ~ success, data = data)
summary(model)
TukeyHSD(model)
#
#
#
#
#analyse1 age classes and prey reads, ANOVA
data2 <- read.table("anova.ratio.txt", header = TRUE)
View(data2)
ggplot(data2, aes(x = age_class, y = prey_reads))+
  geom_boxplot()
# mean of prey-reads per age class (A vs. B vs. C)
aggregate(data2$prey_reads, by=list(data2$age_class), FUN = mean) 
# standard deviation of prey-reads per age class (A vs. B vs. C)
aggregate(data2$prey_reads, by=list(data2$age_class), FUN = sd) 
model2 <- aov(prey_reads ~ age_class, data = data2)
summary(model2)
TukeyHSD(model2)
#
#analyse 2 age classes and spider reads, ANOVA
data5 <- read.table("ratiopreyvsnonprey.txt", header = TRUE)
View(data5)
ggplot(data5, aes(x = age_class, y = spider_reads))+
  geom_boxplot()
# mean of prey-reads per age class (A vs. B vs. C)
aggregate(data5$spider_reads, by=list(data5$age_class), FUN = mean) 
# standard deviation of prey-reads per age class (A vs. B vs. C)
aggregate(data5$spider_reads, by=list(data5$age_class), FUN = sd) 
model5 <- aov(spider_reads ~ age_class, data = data5)
summary(model5)
TukeyHSD(model5)
