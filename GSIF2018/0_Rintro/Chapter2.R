##########################################################################################################################
## Using R for Digital Soil Mapping
## R Script belong to Chapter 2 'R Literacy for Digital Soil Mapping'
## B. Malone, B. Minasny, A.B. McBratney
##########################################################################################################################


## R literacy: 2.2 Introduction to R

##########################################################################################################################
## 2.2.5 R basics: commands, expressions, assignments, operators, objects

## Add two numbers together
1+1

## assignent
x<-1+1
x

## Not the same
x< -1+1

## assignment 
x<-   1

## list objects
ls()

## remove objects
rm(x)
x

## multiple assignments to one value
x<-y<-z<- 1.0
ls()

## R is case sensitive
x<- 1+1
x
X

## commands can be multi-line
x<- 
  +  1+1
##########################################################################################################################


##########################################################################################################################
### 2.2.6 R Data Types


## Numeric
x<- 10.2
x

## Character
name<- "John Doe"
name

## ERROR!
name<- John


## Logical
a<- TRUE
a

## Logical
4 < 2

## logical
b<- 4 < 2
b

## complex number
cnum1<- 10 + 3i
cnum1

## figure out the class of things
class(name)
class(a)
class(x)
mode(x)
##########################################################################################################################


##########################################################################################################################
## 2.2.7 R data structures

## vector
x<- 1:12
x

## matrix
X<- matrix(1:12, nrow=3)
X

## array
Y<- array(1:30,dim=c(2,5,3))
Y

## data frame
dat<- (data.frame(profile_id= c("Chromosol","Vertosol","Sodosol"),
                  FID=c("a1","a10","a11"), easting=c(337859, 344059,347034), 
                  northing=c(6372415,6376715,6372740), visited=c(TRUE, FALSE, TRUE)))
dat

## list
summary.1<- list(1.2, x,Y,dat)
summary.1

## null
x<- NULL
##########################################################################################################################


##########################################################################################################################
## 2.2.8 Missing, indefinite, and infinite values

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
x<- NA
x-2

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
is.na(x)
!is.na(x)
##########################################################################################################################


##########################################################################################################################
## 2.2.9 Functions, arguments, and packages

## ----echo=TRUE, tidy=TRUE,cache=FALSE, background='white'----------------
sum(1,12.5,3.33,5,88)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=FALSE, background='white'-----
a<- 1:10
b<- a
plot(x=a, y=b)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=FALSE, background='white'-----
plot(a, b)

## ----echo=TRUE, tidy=TRUE,cache=FALSE, background='white'----------------
args(plot)

## ----echo=TRUE, tidy=TRUE,cache=FALSE, background='white'----------------
args(rnorm)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=FALSE, background='white'-----
plot(rnorm(10,sqrt(mean(c(1:5, 7,1,8,sum(8.4,1.2,7))))),1:10)


## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=FALSE, background='white'-----
install.packages("Cubist")

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
library(Cubist)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=FALSE, background='white'-----
library(devtools)
install_bitbucket("brendo1001/ithir/pkg")
library(ithir)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
detach("package:Cubist")
##########################################################################################################################


##########################################################################################################################
## 2.2.10 Getting Help

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=FALSE, background='white'-----
?cubist

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=FALSE, background='white'-----
??polygon

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=FALSE, background='white'-----
RSiteSearch("A Keyword")

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
apropos("mean")
##########################################################################################################################



## R literacy: 2.3 Vectors, matrices, and arrays

##########################################################################################################################
## 2.3.1 Creating and working with vectors


## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
1:5

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
seq(-10, 10,2)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
rep(4,5)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
c(2,1,5,100,2)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
c(a=2,b=1,c=5,d=100,e=2)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
v1<- c(2,1,5,100,2)
v1

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
probably.not_a.good_example.for.a.name.100<- seq(1,2,0.1)
probably.not_a.good_example.for.a.name.100

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
x<- rep(1:3)
y<- 4:10
z<- c(x,y)
z

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
x<- 1:10
x > 5

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
a<- x > 5
a
a * 1.4

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
paste("A", "B","C","D",TRUE, 42)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
month<- "April"
day<- 29
year<- 1770
paste("Captain Cook, on the ",
      day,"th day of ",month,", ", year
      , ", sailed into Botany Bay", sep="")

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
group<- 1:10
id<- LETTERS[1:10]
for(i in 1:10){
  print(paste("group =", group[i], "id =", id[i]))
}
##########################################################################################################################


##########################################################################################################################
## 2.3.2 Vector arithmetic, some common functions and vectorised formats

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
x<- 6:10
x
x+2

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
y<- c(4,3,7,1,1)
y
z<- x + y
z

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
x<- 1:10
m<- 0.8
b<- 2
y<- m * x + b
y

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, warning=TRUE, background='white'----
x<- 1:10
m<- 0.8
b<- c(2,1,1)
y<- m * x + b
y

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
pi

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, warning=TRUE, background='white'----
7 - 2 * 4

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, warning=TRUE, background='white'----
(7-2) * 4 

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, warning=TRUE, background='white'----
10^1:5

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, warning=TRUE, background='white'----
10^(1:5)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, warning=TRUE, background='white'----
x<- 1:10
sqrt(x)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, warning=TRUE, background='white'----
sqrt(1:10)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, warning=TRUE, background='white'----
sqrt(c(1,2,3,4,5,6,7,8,9,10))

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=FALSE, error=TRUE, background='white'----
sqrt(1,2,3,4,5,6,7,8,9,10)
##########################################################################################################################


##########################################################################################################################
## Matrices and arrays

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
X<- matrix(1:15, nrow= 5, ncol=3)
X

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
as.vector(X)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
X<- matrix(1:15, nrow= 5, ncol=3, byrow=T)
X

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
Y<- array(1:30, dim= c(5,3,2))
Y

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
Z<- matrix (1, nrow=5, ncol=3)
Z
X + Z

## ERROR! Dimensions do not match
Z<- matrix (1, nrow=3, ncol=3)
X + Z

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white', error=TRUE----
Z
x<- 1:9
Z + x
y<- 1:3
Z + y

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white', error=TRUE----
X<- matrix(c(1,2.5,6,7.5,4.9,5.6,9.9,7.8,9.3), nrow=3)
X
solve(X)

## ----echo=FALSE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'-----
X<- matrix(1:25, nrow= 5, ncol=5, byrow=T)
X
##########################################################################################################################


## R literacy: 2.4 Data frames, data import, and data export

##########################################################################################################################
## 2.4 Data frames, data import, and data export


## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
dat<- data.frame(profile_id= c("Chromosol","Vertosol","Sodosol"),
                 FID=c("a1","a10","a11"), easting=c(337859, 344059,347034), 
                 northing=c(6372415,6376715,6372740), visted=c(TRUE, FALSE, TRUE))
dat

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
str(dat)

##########################################################################################################################


##########################################################################################################################
## 2.4.1 Reading data from files

## ----ERROR!-----
soil.data<- read.table("USYD_soil1.txt", header=TRUE, sep=",")
str(soil.data)
head(soil.data)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
library(ithir)
data(USYD_soil1)
soil.data<- USYD_soil1
str(soil.data)
head(soil.data)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
which(is.na(soil.data$CEC))
soil.data[8:11,]

## ----May not work-----
soil.data<- edit(soil.data)
##########################################################################################################################


##########################################################################################################################
## 2.4.2 Creating data frames manually

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
soil<- c("Chromosol", "Vertosol", "Organosol", "Anthroposol")
carbon<- c(2.1, 2.9, 5.5, 0.2)
dat<- data.frame(soil.type=soil, soil.OC=carbon)
dat

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
names(dat)<- c("soil","SOC")
dat

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
dat<- data.frame(soil.type=soil, soil.OC=carbon, 
                 row.names=c("Ch","Ve","Or","An"))
dat
##########################################################################################################################


##########################################################################################################################
## 2.4.3 Working with data frames

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
names(soil.data)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
soil.data$ESP

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
mean(soil.data$ESP)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
mean(na.omit(soil.data$ESP))

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
attach(soil.data)
ESP

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=FALSE, background='white', error=TRUE----
## ## This will throw an error
## detach(soil.data)
## ESP

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=FALSE, background='white'-----
## soil.data[,10]

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
soil.data$Upper<- soil.data$Upper.Depth*100
soil.data$Lower<- soil.data$Lower.Depth*100
head(soil.data)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
soil.data$ESP
na.omit(soil.data$ESP)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=FALSE, background='white'-----
## soil.data.cleaned<- na.omit(soil.data)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
is.na(soil.data$ESP)
##########################################################################################################################


##########################################################################################################################
## 2.4.4 Writing data to file

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=FALSE, background='white'-----
write.table(soil.data, file= "file name.txt",
            col.names=TRUE, row.names=FALSE, sep="\t")
##########################################################################################################################


## R literacy: 2.5 Graphics: the basics

##########################################################################################################################
## 2.5.1 Introduction to the plot function

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=FALSE, background='white'-----
z<- rnorm(10)
plot (z)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=FALSE, background='white'-----
x<- -15:15
y<- x^2
plot(x,y)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=FALSE, background='white'-----
plot(x,y, type="o", xlim=c(-20,20), ylim=c(-10,300),
     pch=21, col="red", bg="yellow",
     xlab="The X variable", ylab="X squared")

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=FALSE, background='white'-----
plot(1:25, rep(1,25), pch=1:25, ylim=c(0,10), xlab="", ylab="", axes=FALSE)
text(1:25, 1.8, as.character(1:25), cex=0.7)
text(12.5, 2.5, "Default", cex=0.9)
points(1:25, rep(4,25), pch=1:25, col= "blue")
text(1:25, 4.8, as.character(1:25), cex=0.7, col="blue")
text(12.5, 5.5, "Blue", cex=0.9, col="blue")
points(1:25, rep(7,25), pch=1:25, col= "blue", bg="red")
text(1:25, 7.8, as.character(1:25), cex=0.7, col="blue")
text(10, 8.5, "Blue", cex=0.9, col="blue")
text(15, 8.5, "Red", cex=0.9, col="red")
box()
##########################################################################################################################


## R literacy: 2.6 Manipulating data

##########################################################################################################################
## 2.6.1 Modes, classes, attributes, length, and coercion

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
x<- 1:10
mode(x)
length(x)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
x<- 1:10
as.character(x)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
X<- matrix(1:30,nrow=3)
as.data.frame(X)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
X<- matrix(1:30, nrow=3)
X
nrow(X)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
ncol(X)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
dim(X)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
x<- 1:10
NROW(x)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
length(X)
length(x)
##########################################################################################################################


##########################################################################################################################
## 2.6.2 Indexing, sub-setting, sorting and locating data

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
v1<- c(5,1,3,8)
v1
v1[3]

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
v1[1:3]

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
v1[-4]

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
v1[v1<5]

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
v1 < 5
v1[c(FALSE,TRUE,TRUE,FALSE)]

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
length(v1)
v1[8]<- 10
length(v1)
v1

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
library(ithir)
data(USYD_soil1)
soil.data<- USYD_soil1
dim(soil.data)
str(soil.data)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
soil.data[1:5,1:2]

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
soil.data[1:2,]

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
soil.data[1:5, "Total_Carbon"]

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
soil.data[1:5, c("Total_Carbon", "CEC")]

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
na.omit(soil.data[soil.data$ESP>10,])

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
subset(soil.data, ESP>10)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
subset(soil.data, ESP>10 & Lower.Depth > 0.3 )

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=FALSE, background='white'-----
## subset(soil.data, Landclass=="Forest" | Landclass=="native pasture" )

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
head(subset(soil.data, Landclass %in% c("Forest", "native pasture")))

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
X<- matrix(1:30, nrow=3)
X
X[3,8]
X[,3]
Y<- array(1:90, dim=c(3,10,3))
Y[3,1,1]

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
list.1<- list(1:10, X, Y)
list.1[[1]]

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
list.1[[2]][3,2]

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=FALSE, background='white'-----
soil.data.split<- split(soil.data, soil.data$PROFILE)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
x<- rnorm(5)
x
y<- sort(x)
y

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
head(soil.data[order(soil.data$clay),])

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
order(soil.data$clay)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
match(c(25.85,11.45,9.23), soil.data$CEC)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
soil.data[c(41,59,18),]

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
match(max(soil.data$CEC, na.rm=TRUE), soil.data$CEC)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
soil.data$CEC[95]

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
which(soil.data$ESP>5)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
which(soil.data$ESP>5 & soil.data$clay > 30)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
which(is.na(soil.data$ESP))
soil.data$ESP[c(which(is.na(soil.data$ESP)))]
##########################################################################################################################


##########################################################################################################################
## 2.6.3 Factors

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
a<- c(rep(0,4),rep(1,4))
a
a<- factor(a)
a

## ----echo=TRUE, tidy= TRUE,cache=FALSE,eval=TRUE, background='white'-----
soil.drainage<- c("well drained", "imperfectly drained", 
                  "poorly drained", "poorly drained", 
                  "well drained", "poorly drained")

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
soil.drainage1<- factor(soil.drainage)
soil.drainage1
as.numeric(soil.drainage1)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
soil.drainage2<- factor(soil.drainage, levels= c("well drained", 
                                                 "imperfectly drained", "poorly drained"))
as.numeric(soil.drainage2)
##########################################################################################################################


##########################################################################################################################
## 2.6.4 Combining data

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
soil.info1<- data.frame(soil=c("Vertosol", "Hydrosol", "Sodosol"), response=1:3)
soil.info1
soil.info2<- data.frame(soil=c("Chromosol", "Dermosol", "Tenosol"), response=4:6)
soil.info2
soil.info<- rbind(soil.info1, soil.info2)
soil.info
a.column<- c(2.5,3.2,1.2,2.1,2,0.5)
soil.info3<- cbind(soil.info, SOC=a.column)
soil.info3
##########################################################################################################################



## R literacy: 2.7 Exploratory data analysis

##########################################################################################################################
## 2.7.1 Summary statistics

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
library(ithir)
data(USYD_soil1)
soil.data<- USYD_soil1
names(soil.data)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
mean(soil.data$clay, na.rm=TRUE)
median(soil.data$clay, na.rm=TRUE)
sd(soil.data$clay, na.rm=TRUE)
var(soil.data$clay, na.rm=TRUE)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
summary(soil.data[,1:6])
##########################################################################################################################


##########################################################################################################################
## 2.7.2 Histograms and boxplots

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=FALSE, background='white'-----
hist(soil.data$clay)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=FALSE, background='white'-----
boxplot(soil.data$clay)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=FALSE, background='white'-----
boxplot(Total_Carbon~Landclass, data=soil.data)
##########################################################################################################################


##########################################################################################################################
## 2.7.3 Normal quantile and cumulative probability plots

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=FALSE, background='white'-----
qqnorm(soil.data$Total_Carbon, plot.it=TRUE, pch=4, cex=0.7)
qqline(soil.data$Total_Carbon,col="red", lwd=2)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=FALSE, background='white'-----
qqnorm(log(soil.data$Total_Carbon), plot.it=TRUE, pch=4, cex=0.7)
qqline(log(soil.data$Total_Carbon),col="red", lwd=2)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
quantile(soil.data$Total_Carbon, na.rm=TRUE)
quantile(soil.data$Total_Carbon, na.rm=TRUE, probs=seq(0,1,0.05))
quantile(soil.data$Total_Carbon, na.rm=TRUE, probs=seq(0.9,1,0.01))
##########################################################################################################################


## R literacy: 2.8 Linear models-the basics

##########################################################################################################################
## 2.8.2 Linear Regression

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
library(ithir)
data(USYD_soil1)
soil.data<- USYD_soil1
summary(cbind(clay=soil.data$clay, CEC=soil.data$CEC))

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=FALSE, background='white'-----
plot(soil.data$clay, soil.data$CEC)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
mod.1<- lm(CEC~clay, data=soil.data, y=TRUE, x=TRUE)
mod.1

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
summary(mod.1)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
class(mod.1)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
mod.1$coefficients

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
coef(mod.1)
head(residuals(mod.1))

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
names(summary(mod.1))

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
summary(mod.1)[[4]]

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
summary(mod.1)[["r.squared"]]

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
head(predict(mod.1))

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=FALSE, background='white'-----
plot(mod.1$y,mod.1$fitted.values, xlim= c(0,25), ylim=c(0,25))
abline(a = 0, b = 1, col="red")
## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
head(predict(mod.1, int="conf"))

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
subs.soil.data<- soil.data[, c("clay", "CEC", "ExchNa", "ExchCa")]
summary(subs.soil.data)

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
cor(na.omit(subs.soil.data))

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=FALSE, background='white'-----
pairs(na.omit(subs.soil.data))

## ----echo=TRUE, tidy=TRUE,cache=FALSE,eval=TRUE, background='white'------
mod.2<- lm(CEC~ clay + ExchNa + ExchCa, data= subs.soil.data)
summary(mod.2)
##########################################################################################################################


## R literacy: 2.9 Logical thinking and algorithm development

##########################################################################################################################
## Principal toposequence algorithm

## The algorithm for principal toposequence can be written as:
## 1. Determine the highest point in an area.
## 2. Determine its 3 x 3 neighbor, and determine whether there are lower points?
## 3. If yes, set the lowest point as the next point in the toposequence, and then repeat step 2. If no, the toposequence has ended.


# function to find the lowest 3 x 3 neighbor
find_steepest<-function(dem,row_z,col_z){
  z1=dem[row_z,col_z] #elevation 
  
  # return the elevation of the neighboring values
  dir=c(-1,0,1) #neighborhood index
  nr=nrow(dem)
  nc=ncol(dem)
  pz=matrix(data=NA,nrow=3,ncol=3) #placeholder for the values
  for (i in 1:3){
    for (j in 1:3){
      if(i!=0 & j !=0) {
        ro<-row_z+dir[i]
        co<-col_z+dir[j]
        if(ro>0 & co>0 & ro<nr & co<nc) {
          pz[i,j]=dem[ro,co]}      
      }}}
  
  pz<-pz-z1# difference of neighbors from centre value
  #find lowest value
  min_pz<-which(pz==min(pz,na.rm=TRUE),arr.ind=TRUE)
  row_min<-row_z+dir[min_pz[1]]  
  col_min<-col_z+dir[min_pz[2]]
  retval<-c(row_min,col_min,min(pz,na.rm=TRUE))
  return(retval) #return the minimum
}

##RUNNING THE FUNCTION
##get data
library(ithir)
data(topo_dem)
str(topo_dem)

## Place to put transect infor
transect<-matrix(data=NA,nrow=20,ncol=3)

## Find maximum elevation (and position) within matrix
max_elev<-which(topo_dem==max(topo_dem),arr.ind=TRUE)
row_z=max_elev[1]# row of max_elev
col_z=max_elev[2]# col of max_elev
z1=topo_dem[row_z,col_z]# max elevation

#Put values into the first entry of the transect object
t<-1
transect[t,1]=row_z
transect[t,2]=col_z
transect[t,3]=z1
lowest=FALSE


# iterate down the hill until lowest point
while (lowest==FALSE) {
  result<-find_steepest(dem = topo_dem,row_z,col_z) # While condition is false find steepest neighbor
  t<-t+1
  row_z=result[1]
  col_z=result[2]
  z1=topo_dem[row_z,col_z]
  transect[t,1]=row_z
  transect[t,2]=col_z
  transect[t,3]=z1
  if(result[3]>=0) {lowest==TRUE; break}# Break if found lowest point
}

## Plot the Transect
dist=sqrt((transect[1,1]-transect[,1])^2+(transect[1,2]-transect[,2])^2)
plot(dist,transect[,3],type='l',xlab='Distance (m)', ylab='Elevation (m)')

## Algorithm for random toposequence

## 1. Select a random point from a DEM.
## 2. Travel uphill:
##  (a) Determine its 3 x 3 neighbor, and determine whether there are higher points?
##  (b) If yes, select randomly a higher point, add to the uphill sequence, and repeat step 2a. If this point is the highest, the uphill sequence ended.
## 3. Travel downhill:
##  (a) Determine its 3 x 3 neighbor, and determine whether there are lower points?
##  (b) If yes, select randomly a lower point, add to the downhill sequence, and repeat step 3a. If this point is the lowest or reached a stream, the downhill sequence ended.


#Specification of two functions


# function to simulate water moving down the slope
# input:  dem and its row & column
#         random: TRUE use random path, FALSE for steepest path
# return: row,col,z-z1 of lower neighbour
travel_down<-function(dem,row_z,col_z,random)
  
{
  z1=dem[row_z,col_z]
  # find its eight neighbour
  dir=c(-1,0,1)
  nr=nrow(dem)
  nc=ncol(dem)
  pz=matrix(data=NA,nrow=3,ncol=3)
  for (i in 1:3){
    for (j in 1:3){
      ro<-row_z+dir[i]
      co<-col_z+dir[j]
      if(ro>0 & co>0 & ro<nr & co<nc) {
        pz[i,j]=dem[ro,co]}      
    }}
  pz[2,2]=NA
  pz<-pz-z1# difference with centre value
  
  min_pz<-which(pz<0,arr.ind=TRUE)
  nlow<-nrow(min_pz)
  if(nlow==0) {
    min_pz<-which(pz==min(pz,na.rm=TRUE),arr.ind=TRUE)}
  else {  
    if(random){
      #find random lower value
      ir<-sample.int(nlow,size=1)
      min_pz<-min_pz[ir,]}
    else{
      #find lowest value
      min_pz<-which(pz==min(pz,na.rm=TRUE),arr.ind=TRUE)}
  }  
  row_min<-row_z+dir[min_pz[1]]  
  col_min<-col_z+dir[min_pz[2]]
  z_min<-dem[row_min,col_min]
  retval<-c(row_min,col_min,min(pz,na.rm=TRUE))
  return(retval)
}



# function to trace water coming from up hill
# input:  dem and its row & column
#         random: TRUE use random path, FALSE for steepest path
# return: row,col,z-zi  of higher neighbour
travel_up<-function(dem,row_z,col_z,random)
  
{
  z1=dem[row_z,col_z]
  # find its eight neighbour
  dir=c(-1,0,1)
  nr=nrow(dem)
  nc=ncol(dem)
  pz=matrix(data=NA,nrow=3,ncol=3)
  for (i in 1:3){
    for (j in 1:3){
      ro<-row_z+dir[i]
      co<-col_z+dir[j]
      if(ro>0 & co>0 & ro<nr & co<nc) {
        pz[i,j]=dem[ro,co]}      
    }}
  pz[2,2]=NA
  pz<-pz-z1# difference with centre value
  
  max_pz<-which(pz>0,arr.ind=TRUE)# find higher pixel
  nhi<-nrow(max_pz)
  if(nhi==0) {
    max_pz<-which(pz==max(pz,na.rm=TRUE),arr.ind=TRUE)}
  else {  
    if(random){
      #find random higher value
      ir<-sample.int(nhi,size=1)
      max_pz<-max_pz[ir,]}
    else{
      #find highest value
      max_pz<-which(pz==max(pz,na.rm=TRUE),arr.ind=TRUE)}
  }  
  row_max<-row_z+dir[max_pz[1]]  
  col_max<-col_z+dir[max_pz[2]]
  retval<-c(row_max,col_max,max(pz,na.rm=TRUE))
  return(retval)
}


###IMPLEMENTATION

## Select a random seed point

nr<-nrow(topo_dem)# no. rows in a DEM
nc<-ncol(topo_dem)# no. cols in a DEM

# start with a random pixel as seed point
row_z1<-sample.int(nr,1)
col_z1<-sample.int(nc,1)


# Travel uphill
# seed point as a starting point 
t<-1
transect_up<-matrix(data=NA,nrow=100,ncol=3)
row_z<-row_z1
col_z<-col_z1
z1=topo_dem[row_z,col_z]
transect_up[t,1]=row_z
transect_up[t,2]=col_z
transect_up[t,3]=z1

highest=FALSE
# iterate up the hill until highest point
while (highest==FALSE) {
  result<-travel_up(dem = topo_dem,row_z,col_z,random=TRUE)
  if(result[3]<=0) {highest==TRUE; break}# if found lowest point  
  t<-t+1
  row_z=result[1]
  col_z=result[2]
  z1=topo_dem[row_z,col_z]
  transect_up[t,1]=row_z
  transect_up[t,2]=col_z
  transect_up[t,3]=z1
}
transect_up<-na.omit(transect_up)


# travel downhill
# create a data matrix to store results
transect_down<-matrix(data=NA,nrow=100,ncol=3)
# starting point 
row_z<-row_z1
col_z<-col_z1
z1=topo_dem[row_z,col_z]# a random pixel
t<-1
transect_down[t,1]=row_z
transect_down[t,2]=col_z
transect_down[t,3]=z1
lowest=FALSE

# iterate down the hill until lowest point
while (lowest==FALSE) {
  result<-travel_down(dem = topo_dem,row_z,col_z,random=TRUE)
  if(result[3]>=0) {lowest==TRUE; break}# if found lowest point  
  t<-t+1
  row_z=result[1]
  col_z=result[2]
  z1=topo_dem[row_z,col_z]
  transect_down[t,1]=row_z
  transect_down[t,2]=col_z
  transect_down[t,3]=z1
}
transect_down<-na.omit(transect_down)

## Stitch both transect segments together
transect<- rbind(transect_up[order(transect_up[,3],
                                   decreasing = T),], transect_down[-1,])

# calculate distance from hilltop
dist=sqrt((transect[1,1]-transect[,1])^2+(transect[1,2]-transect[,2])^2)

## Make a plot
plot(dist,transect[,3],type='l',col='red',xlim=c(0,100),
     ylim=c(50,120),xlab='Distance (m)',
     ylab='Elevation (m)')

## Plot the seed point
points(dist[nrow(transect_up)],transect[nrow(transect_up),3])


##END