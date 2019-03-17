generateSquares <- function(x) {
  for (i in 1:x){
    y <- i^2
    print(y)
  }
  return(x^2)
}

a = generateSquares(4)
print(a)

func_arguments <- function(a=1, b=2, c=3){
  res <- a + (b * c)
  print(res)
}

func_arguments()

func_arguments(4,5,6)

func_arguments(a = 4, b = 5, c = 3)

mydataframe <- data.frame(
  stu_id = c(1:5),
  stu_name = c("Bob", "Pat", "Jane", "Peter", "Han"),
  stringsAsFactors =  FALSE
)

res <- data.frame(mydataframe$stu_id, mydataframe$stu_name)
print(res)

#Problem 1
sales1<-c(12,14,16,29,30,45,19,20,16, 19, 34, 20)
sales2<-rpois(12,34)

par(bg="cornsilk")

plot(sales1, col="blue",type="o",ylim=c(0,100),xlab="Month",ylab="Sales")
title(main = "Sales by Month")

lines(sales2, type="o", pch=22, lty=2, col="red")
grid(nx=NA,ny=NULL)
legend("topright", inset=.05,c("Sales1","Sales2"),fill=c("blue","red"),horiz=TRUE)

#Problem 2
sales<-read.table(file.choose(), header = T)
sales
barplot(as.matrix(sales),main="Sales Data", ylab="Total",beside=T,col=rainbow(5))

#Problem 3
fn <- boxplot(sales, col=c("orange","green"))$stats

text(1.45,fn[3,2],paste("Median =", fn[3,2]),adj = 0, cex = .7)
text(0.45,fn[3,1],paste("Median =", fn[3,1]), adj = 0, cex = .7)
grid(nx=NA,ny=NULL)

#Problem 4
goog1 <- read.csv(file.choose())
amzn1 <- read.csv(file.choose())
par(bg="cornsilk")
plot(goog1$Adj.Close, col="blue",type="o",ylim=c(800,1800),xlab="Days",ylab="Price")
title(main="Google and Amazon Stocks")
lines(amzn1$Adj.Close, col="orange", type="o", pch=22, lty=2)
legend("bottomright",inset=.05,c("Google","Amazon"),fill=c("blue","orange"),horiz=TRUE)

hist(amzn1$Adj.Close,col=rainbow(8))

#Problem 5
data()

attach(airquality)
head(airquality)
summary(airquality)
detach(airquality)

library(help=datasets)
library(datasets)
head(uspop)
plot(uspop)

#Problem 6
library("ggmap")
library("maptools")
library(maps)
register_google(key=(key_get("GoogleAPI", keyring = "mykeyring")))
visited <- c("SFO", "Chennai", "London", "Melbourne","Lima,Peru","Johannesbury, SA")
ll.visited <- geocode(visited)
visit.x <- ll.visited$lon
visit.y <- ll.visited$lat
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))
points(visit.x,visit.y, col="red", pch=36)

library("ggmap")
library("maptools")
library(maps)
visited <- c("SFO", "New York", "Buffalo", "Dallas, TX")
ll.visited <- geocode(visited)
visit.x <- ll.visited$lon
visit.y <- ll.visited$lat
map("state", fill=TRUE, col=rainbow(50), bg="lightblue", mar=c(0,0,0,0))
points(visit.x,visit.y, col="yellow", pch=36)

#Problem 7
attach(mtcars)
head(mtcars)
plot(mtcars[c(1,3,4,5,6)], main="MTCARS Data")
plot(mtcars[c(1,3,4,6)], main="MTCARS Data")
plot(mtcars[c(1,3,4,6)], col=rainbow(5),main="MTCARS Data")

#Problem 8
library(ggplot2)
ggplot(mtcars, aes(x=mpg, y=disp)) + geom_point()

