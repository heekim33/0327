a <- 1
a
b <- 2
b
c <- 3
c
a+b
4/b

var1 <- c(1,2,3)
var1
var2 <- c(1:5)
var2
var3 <- seq (1,10, by=2)
var3
var3+3
str1 <- "a"
str <- "text"
str3 <- "Hello World Is Good"
str3

mean(var3)
max(var3)

english <- c(90, 80, 60, 70)
math <- c(50, 60, 10, 20)

df_midterm <- data.frame(english, math)
mean(df_midterm$english)

sales <- data.frame(fruit = c("사과", "딸기", "수박"), price = c(1800, 1500, 3000), volume = c(24, 38, 13))

mean(sales$price)

df_csv_exam <- read.csv("csv_exam.csv") 
df_csv_exam
mean(df_csv_exam$math)
