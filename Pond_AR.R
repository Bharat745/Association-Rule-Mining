# Installing required packages
install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)
 
# Loading in the file 
P<- read.csv("C:/Users/Bharat/Desktop/Pond.csv",header = TRUE)
attach(P)
class(P)
str(P)

# Converting all variables to facotr
P1 <- lapply(P, function(x){as.factor(x)})
str(P1)

#converting the variables to transactions
Pt <- as(P, "transactions")

#Defining the rules
rules<-apriori(Pt, parameter = list(support=0.1, confidence=0.5, minlen=2))

# finding redundant rules
rules
inspect(rules)
redundant_rules <- is.redundant(rules)
redundant_rules
summary(redundant_rules)
rules<- rules[!redundant_rules]
rules

#inspecting rules based on different factors
inspect(sort(rules, by="lift")[1:5])
inspect(sort(rules, by="confidence")[1:5])
inspect(sort(rules, by="support")[1:5])

#Visualizing Rules
install.packages("arulesViz")
library(arulesViz)

plot(rules)
plot(rules, method="grouped")
plot(rules,method="graph")
plot(rules, method= "graph", interactive = T )

# RHS predefined rules

# Wage buckets based analysis
rw1<- apriori(data=P, parameter=list 
            (supp=0.001,conf = 0.5), appearance = list (rhs="Wage.buckets=0-10 USD"))
inspect(sort(rw1, by="lift"))
plot(rw1,method="graph")

rw2<- apriori(data=P, parameter=list 
            (supp=0.001,conf = 0.5), appearance = list (rhs="Wage.buckets=10-20 USD"))
inspect(sort(rw2, by="lift")[1:5])


rw3<- apriori(data=P, parameter=list 
            (supp=0.001,conf = 0.5), appearance = list (rhs="Wage.buckets=20-30 USD"))
inspect(sort(rw3, by="lift"))
 plot(rw3,method="graph")

rw4<- apriori(data=P, parameter=list 
               (supp=0.001,conf = 0.5), appearance = list (rhs="Wage.buckets=30-40 USD"))
inspect(sort(rw4, by="lift"))
plot(rw4,method="graph")

rw5<- apriori(data=P, parameter=list 
              (supp=0.001,conf = 0.5), appearance = list (rhs="Wage.buckets=40-50 USD"))
inspect(sort(rw5, by="lift"))
plot(rw5,method="graph")

rw6<- apriori(data=P, parameter=list 
              (supp=0.001,conf = 0.5), appearance = list (rhs="Wage.buckets=50-100 USD"))
inspect(sort(rw6, by="lift"))
plot(rw6,method="graph")

rw7<- apriori(data=P, parameter=list 
              (supp=0.001,conf = 0.5), appearance = list (rhs="Wage.buckets= >100 USD"))
inspect(sort(rw6, by="lift"))
plot(rw6,method="graph")

# Injury based analysis
ri1<- apriori(data=P, parameter=list 
            (supp=0.001,conf = 0.5), appearance = list (rhs="injury_segment=back"))
inspect(sort(ri1, by="lift")[1:5])

ri2<- apriori(data=P, parameter=list 
            (supp=0.001,conf = 0.5), appearance = list (rhs="injury_segment=hand"))
inspect(sort(ri2, by="lift")[1:5])

ri3<- apriori(data=P, parameter=list 
            (supp=0.001,conf = 0.5), appearance = list (rhs="injury_segment=leg"))
inspect(sort(ri3, by="lift")[1:5])

#JOb segments based analysis
rj1<- apriori(data=P, parameter=list 
              (supp=0.001,conf = 0.5), appearance = list (rhs="job_segment=Mechanical"))
inspect(sort(rj1, by="lift"))

rj2<- apriori(data=P, parameter=list 
              (supp=0.001,conf = 0.5), appearance = list (rhs="job_segment=transportation"))
inspect(sort(rj2, by="lift")[1:3])

rj3<- apriori(data=P, parameter=list 
              (supp=0.001,conf = 0.5), appearance = list (rhs="job_segment=Physical"))
inspect(sort(rj3, by="lift"))

#Age based analysis
ra1<- apriori(data=P, parameter=list 
              (supp=0.001,conf = 0.5), appearance = list (rhs="Age.bucket=Thirties"))
inspect(sort(ra1, by="lift")[1:5])

ra2<- apriori(data=P, parameter=list 
              (supp=0.001,conf = 0.5), appearance = list (rhs="Age.bucket=Twenties"))
inspect(sort(ra2, by="lift"))

ra3<- apriori(data=P, parameter=list 
              (supp=0.001,conf = 0.5), appearance = list (rhs="Age.bucket=Fourties"))
inspect(sort(ra3, by="lift")[1:5])
