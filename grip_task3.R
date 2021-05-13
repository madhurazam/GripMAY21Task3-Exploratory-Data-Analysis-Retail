titanic=read.csv("F:\\data science\\R\\datasets\\titanic3.csv")
View(titanic)
dim(titanic)
names(titanic)

library(dplyr)
glimpse(titanic)

summary(titanic)

plot(mtcars)
count=table(mtcars$gear);count
barplot(count,main="Car distribution",xlab="number automatic cars",names.arg = c("manual","automatic"))


count=table(mtcars$vs,mtcars$gear);count
barplot(count,main="car distribution by gear and vs",beside = TRUE)

data("airquality")
temp=airquality$Temp
h=hist(temp)
h=hist(temp,ylim = c(0,40))
text(h$mids,h$counts,labels=h$counts,adj=c(0.5,-0.5))

age = c(36,25, 38, 46, 55, 68, 72, 55, 36, 38, 67, 45, 22, 48, 91, 46, 52, 61, 58, 55)

h<-hist(age, breaks=10, col="lightblue", xlab="age", main = "Histogram with Normal Curve")

xfit<-seq(min(age),max(age),length=40)

yfit<-dnorm(xfit,mean=mean(age),sd=sd(age))

yfit1 <- yfit*diff(h$mids[2:3])*length(age)

lines(xfit, yfit1, col="blue", lwd=2)

library(ggplot2)
library(tidyverse)
data(mtcars)
(ggplot(mtcars,aes(wt,mpg))+geom_point()+theme_bw())
(ggplot(mtcars,aes(wt,mpg,colour=factor(gear)))+geom_point()+theme_bw())
(ggplot(mtcars,aes(wt,mpg,colour=factor(gear),size=cyl))+geom_point()+theme_bw())
(ggplot(mtcars,aes(wt,mpg,colour=factor(gear)))+geom_point()+facet_wrap(~cyl)+theme_bw())

library(readxl)
df=read_excel("F:/data science/R/datasets/WildlifePopulation.xlsx")
df
library(tidyr)
long_data=gather(df,key = "animal",value = "values",Bears:Whales);long_data
(ggplot(long_data,aes(x=long_data$Year,y=long_data$values,group=animal,fill=animal)))+geom_area()


library(corrplot)
mydata=mtcars[,c(1,3,4,5,6,7)]
head(mydata)
mat=cor(mydata);mat
head(round(mat,2))
corrplot(mat)
corrplot(mat,type="upper")
corrplot(mat,type="upper",order="hclust")#clustering ....blu

library(fmsb)
data <- as.data.frame(matrix( sample( 2:20 , 10 , replace=T) , ncol=10))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding", "data-viz" , "french" , "physic", "statistic", "sport" )
data=rbind(rep(20,10),rep(0,10),data)
radarchart(data)
radarchart( data , axistype=1 ,
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 ,
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            
            #custom labels
            vlcex=0.8
)
##################################### HOMEWORK ####################################

################# A Dataset
library(readxl)
sales=read_excel(file.choose(),skip = 2)
View(sales)
library(dplyr)
library(tidyr)
library(ggplot2)

#1
l=gather(sales,key="Product",value="Sales",'Product A':'Product E');l
(ggplot(l,aes(x=l$Month,y=l$Sales,group=Product,colour=Product)))+geom_line()

#2
y=mutate(sales,all=(sales$`Product A`+sales$`Product B`+sales$`Product C`
                    +sales$`Product D`+sales$`Product E`))
y
(ggplot(y,aes(y$Month,y$all,group=1)))+geom_line()

#3
counts=sapply(sales[,2:6], sum);counts
barplot(counts,main="Annual Sales",horiz=TRUE,
     names.arg=colnames(counts),las=1)

#4
data=sales[11:12,2:6];

barplot(as.matrix(data), main="Last Two Months", legend=c("November","December"),
        col= c("darkblue","red"), beside=TRUE)


################# B Dataset

library(readxl)
data=read_excel("F:\\data science\\R\\datasets\\EEO Employment Report.xlsx",skip = 2)
View(data)
data1=na.omit(data)
View(data1)
library(dplyr)
library(tidyr)

emp_data=data[1:3,3:11]
emp_data
barplot(as.matrix(emp_data),main = "Alabama Employment",legends=c("")
,ylim=c(0,140000),col=c("blue","red","green"),beside = TRUE,las=2,cex.names = 0.7)


################# C Dataset

library(readxl)
china_trade=read_excel("F:\\data science\\R\\datasets\\China Trade Data.xlsx",skip = 2)
View(china_trade)
(ggplot(china_trade,aes(x=china_trade$Year,y=china_trade$`US Exports to China`,group=1)))+
  geom_point(colour="blue",size=3)+
  geom_line(colour="blue",size=1.1)+
  ggtitle("U.S.Exports to China($ billions)")+
  theme(axis.text.x = element_text(angle = 90))+
scale_x_continuous("Year",labels=as.character(china_trade$Year),breaks = china_trade$Year)+
scale_y_continuous(" ",breaks = seq(0,120,20))


################# D Dataset

edu_data=read_excel("F:\\data science\\R\\datasets\\Census Education Data.xlsx",skip = 1)
View(edu_data)
edu_data1=edu_data[17:22,1:2];edu_data1
data=edu_data1$`Not a High \r\nSchool Grad`
lbls<-edu_data1$...1
pct <-round(data/sum(data)*100)
lbls<-paste(lbls, pct) 
lbls<-paste(lbls,"%",sep="")
pie(data,labels= lbls, col=rainbow(length(lbls)), 
    main="Marital Status:Not a High School Grad")

spread()
barplot(as.matrix(edu_data1))
