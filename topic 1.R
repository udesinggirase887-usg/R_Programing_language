#topic 1
#Q1 (a)
tourists=matrix(data=c(9.303,9.536,9.918,7.959,7.736,8.875,15.224,15.629,16.090,0.905,0.894,0.883,17.463,18.635,20.148),nrow=5,byrow=T);tourists
row.names(tourists) <- c("Germany","France","Great Britain","USA","Rest of the world")
colnames(tourists) <- c("2003","2004","2005")
tourists
#(b) calculate the total of the rows and columns
row_total <- rowSums(tourists);row_total   #row totals 
col_total <- colSums(tourists);col_total   #col totals
#(c) calculate the mean of rows and columns
rm <- rowMeans(tourists);rm 
cm <- colMeans(tourists);cm


#Q2 solve system of equation
A <- matrix(c(3,2, 1,2,-3,1,1,1,1),nrow = 3,byrow = T);A
B <- c(10,-1,6);B #as vector form 
solve(A,B) #for solution of A %*% x =B 
solve(A) # for inverse 
round(rm) #round up the numerical value round(X,at place)
is.matrix(A) #check matrix in true or false
as.matrix(A) #convert data in matrix form


#Q3 different ways to enter 
data <- data.frame(age=c(25,30,18),
                   gender=c("M","F","F"),
                   weight=c(166,115,120));data
#OR
age=c(25,30,18);
gender=c("M","F","F");
weight=c(166,115,120);
data1 <- data.frame(age,gender,weight);data1
#OR
data2 <- data.frame(age=numeric(0),gender=character(0),weight=numeric(0))
data2 <- edit(data2);data2

#4 enter the following data in R and store it with the name leadership 
leadership <- data.frame(manager=numeric(0),date=numeric(0),country=character(0),gender=character(0),
                         age=numeric(0),q1=numeric(0),q2=numeric(0),q3=numeric(0),q4=numeric(0),q5=numeric(0))
leadership <- edit(leadership);leadership

#Q5 cars93 data set form the MASS library
library(MASS)
data("Cars93")
View(Cars93) #view data set 
attach(Cars93) #attach data set 
#(a) contingency table of origin by AirBags
table1 <- table(Origin,AirBags);table1
#(b)three way contingency table of origin with respect to AirBags and DrivenTrain 
ftable(Origin,AirBags,DriveTrain)
#(c) the totals and proportion by rows and columns for each
#row total
r_totala <- rowSums(table1);r_totala
#OR
margin.table(table1,1)
#col total
c_total <- colSums(table1);c_total
margin.table(table1,2)
#proportion table
prop.table(table1)

#Q6
x <- c(19,14,15,17,20,23,19,19,21,18)
x
treatment <- c(rep("A",5),rep("B",5))
tapply(x,treatment,mean) #for group summery 
#7
data("mtcars")
ls=lapply(mtcars,summary);ls #applies to each ele. list
ss=sapply(mtcars,summary);ss #applies to vector,matrix(not list)
typeof(ss)

#Q8 Consider 'College' data available in ISLR2 library in R.

#a) Obtain numerical summary of the variables in the data set.

library(ISLR2)
View(College)
help(College)

#a)numerical summary of the College data
summary(College)

#b)scatterplot matrix

pairs(College[,2:10])

#c)
attach(College)
boxplot(Outstate~Private,data=College)

#d)
Elite <- ifelse(Top10perc>50,"Yes","No")
table(Elite)
Elite[1:5]
detach(College)
