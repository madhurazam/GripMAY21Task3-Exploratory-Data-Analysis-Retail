#Loading Packages

library(dplyr)
library(corrplot)
library(ggplot2)
library(packcircles)
library(maps)
library(tidyverse)
library(tidyr)

#Loading dataset
sales=read.csv("F:\\data science\\R\\datasets\\SampleSuperstore.csv")
View(sales)

#Checking NA's
sapply(sales, function(x) sum(is.na(x)))

num_var=sales[,10:13]
summary(num_var)   #max loss=6599.978   max profit=8399.976

correlation=round(cor(num_var),2);correlation
corrplot(correlation)

#profit is negatively correlated with discount 
#and positively correlated with sales and quantity
#To increase the profit we have to increase sales and quantity
#and decrease discount

############################Region-wise Analysis###########################

profit=tapply(sales$Profit,sales$Region,sum);
profit_in_per=round(profit/sum(profit)*100);profit_in_per
lbls=levels(sales$Region)
lbls<-paste(lbls, profit_in_per) 
lbls<-paste(lbls,"%",sep="")
pie(profit,labels= lbls, col=c("yellow","green","dark green","light yellow"), 
    main="Profit by Region")


##################################Ship mode###################################

ship_mode=select(sales,Ship.Mode,Profit) %>%
  group_by(Ship.Mode) %>%
  summarise(profit=sum(Profit));ship_mode
longdata=gather(ship_mode,key = "count",value = "values",profit);longdata
(ggplot(longdata,aes(x=longdata$Ship.Mode,y=longdata$values,group=count,
                     fill=count)))+
  geom_bar(stat="identity")+
    theme(axis.text = element_text(colour = "black"))+
  coord_flip()+labs(title = "Profit by Ship mode",
                    x="Shipping mode",y="Profit")
 
################################Segment####################################

segment=select(sales,Segment,Profit) %>%
  group_by(Segment) %>%
  summarise(sum_profit=sum(Profit));segment
packing=circleProgressiveLayout(segment$sum_profit, sizetype='area')
segment <- cbind(segment, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50);dat.gg
ggplot() + 
    geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
    geom_text(data = segment, aes(x, y, size=sum_profit, label = Segment)) +
  scale_size_continuous(range = c(5,5)) +
     theme_void() + 
  theme(legend.position="none") +
  coord_equal()

#############################state wise analysis##############################

state=select(sales,State,Profit) %>%
  group_by(State) %>%
  summarise(Profit=sum(Profit));state
state$region=tolower(state$State)
states_data=map_data("state")
profit_map=left_join(states_data, state, by = "region")
state_data=select(states_data,long,lat,region) %>%
  group_by(region) %>%
  summarise(Long=mean(long),Lat=mean(lat));state_data
ggplot(profit_map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = Profit), color = "white")+
  scale_fill_gradient(low="yellow",high="dark green")+
   geom_text(data = state_data, aes(Long, Lat, label = region,group=1), 
           size = 3)+theme_void()

#########################Category-wise analysis############################

categorical_data=tapply(sales$Profit,sales$Category,sum);categorical_data
barplot(categorical_data,main = "Category wise profit",xlab = "Category",
        ylab = "Profit")

subcategorical_data=select(sales,Category,Sub.Category,Profit) %>%
group_by(Category,Sub.Category) %>%
  summarise(total_profit=sum(Profit));subcategorical_data

(ggplot(subcategorical_data,aes(x=subcategorical_data$Sub.Category,
                             y=subcategorical_data$total_profit)))+
  geom_bar(stat="identity")+facet_grid(.~Category,scales = "free")+ theme(
    axis.text.x = element_text(angle=50, hjust=1)
  )+labs(title = "Sub-category wise plot",x="Sub categories",y="Profit")

#############################Central Region###################################
Central_states=select(sales,Region,State,Profit) %>% filter(Region=='Central') %>% group_by(State) %>% summarise(total_profit=sum(Profit))
Central_states$region=tolower(Central_states$State)
states_data=map_data("state")
sales_map=left_join(states_data,Central_states, by = "region")
state_data=select(states_data,long,lat,region) %>% group_by(region) %>% summarise(Long=mean(long),Lat=mean(lat))
ggplot(sales_map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = total_profit), color = "black")+
  scale_fill_gradient(low="yellow",high="red",na.value = "white")+
  geom_text(data = state_data, aes(Long, Lat, label = region,group=1), 
            size = 3)+theme_void()

central_category=select(sales,Region,Category,Profit) %>% 
  filter(Region=='Central') %>% 
  group_by(Category) %>% summarise(total_profit=sum(Profit));central_category

(ggplot(central_category,aes(x=central_category$Category,
        y=central_category$total_profit,group=1)))+geom_bar(stat="identity")+
  labs(title = "category wise plot in central region",
       x="categories",y="Profit")

central=select(sales,Region,Category,Sub.Category,Profit) %>%
  filter(Region=='Central') %>%
  group_by(Category,Sub.Category) %>%
  summarise(total_profit=sum(Profit));central

(ggplot(central,aes(x=central$Sub.Category,y=central$total_profit)))+
  geom_bar(stat="identity")+facet_grid(.~Category,scales = "free")+ theme(
    axis.text.x = element_text(angle=50, hjust=1)
  )+labs(title = "Sub-category wise plot in central region",
         x="Sub categories",y="Profit")


###########################South Region##############################
South_states=select(sales,Region,State,Profit) %>%
  filter(Region=='South') %>%
  group_by(State) %>%
  summarise(total_profit=sum(Profit))
South_states$region=tolower(South_states$State)
states_data=map_data("state")
sales_map=left_join(states_data,South_states, by = "region")
state_data=select(states_data,long,lat,region) %>%
  group_by(region) %>%
  summarise(Long=mean(long),Lat=mean(lat))
ggplot(sales_map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = total_profit), color = "black")+
  scale_fill_gradient(low="orange",high="brown",na.value = "white")+
  geom_text(data = state_data, aes(Long, Lat, label = region,group=1), 
            size = 3)+theme_void()

south_category=select(sales,Region,Category,Profit) %>%
  filter(Region=='South') %>%
  group_by(Category) %>%
  summarise(total_profit=sum(Profit));south_category

(ggplot(south_category,aes(x=south_category$Category,
y=south_category$total_profit,group=1)))+geom_bar(stat="identity")+
  labs(title = "category wise plot in south region",
       x="categories",y="Profit")

south=select(sales,Region,Category,Sub.Category,Profit) %>%
  filter(Region=='South') %>%
  group_by(Category,Sub.Category) %>%
  summarise(total_profit=sum(Profit));south

(ggplot(south,aes(x=south$Sub.Category,y=south$total_profit)))+
  geom_bar(stat="identity")+facet_grid(.~Category,scales = "free")+ theme(
    axis.text.x = element_text(angle=50, hjust=1)
  )+labs(title = "Sub-category wise plot in south region",
         x="Sub categories",y="Profit")

############################West Region###############################
West_states=select(sales,Region,State,Profit) %>%
  filter(Region=='West') %>%
  group_by(State) %>%
  summarise(total_profit=sum(Profit))
West_states$region=tolower(West_states$State)
states_data=map_data("state")
sales_map=left_join(states_data,West_states, by = "region")
state_data=select(states_data,long,lat,region) %>%
  group_by(region) %>%
  summarise(Long=mean(long),Lat=mean(lat))
ggplot(sales_map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = total_profit), color = "black")+
  scale_fill_gradient(low="light blue",high="purple",na.value = "white")+
  geom_text(data = state_data, aes(Long, Lat, label = region,group=1), 
            size = 3)+theme_void()

west_category=select(sales,Region,Category,Profit) %>%
  filter(Region=='West') %>%
  group_by(Category) %>%
  summarise(total_profit=sum(Profit));west_category

(ggplot(west_category,aes(x=west_category$Category,
y=west_category$total_profit,group=1)))+geom_bar(stat="identity")+
  labs(title = "category wise plot in west region",
       x="categories",y="Profit")

west=select(sales,Region,Category,Sub.Category,Profit) %>%
  filter(Region=='West') %>%
  group_by(Category,Sub.Category) %>%
  summarise(total_profit=sum(Profit));west

(ggplot(west,aes(x=west$Sub.Category,y=west$total_profit)))+
  geom_bar(stat="identity")+facet_grid(.~Category,scales = "free")+ theme(
    axis.text.x = element_text(angle=50, hjust=1)
  )+labs(title = "Sub-category wise plot in west region",
         x="Sub categories",y="Profit")

##########################East Region###############################
East_states=select(sales,Region,State,Profit) %>%
  filter(Region=='East') %>%
  group_by(State) %>%
  summarise(total_profit=sum(Profit))
East_states$region=tolower(East_states$State)
states_data=map_data("state")
sales_map=left_join(states_data,East_states, by = "region")
state_data=select(states_data,long,lat,region) %>%
  group_by(region) %>%
  summarise(Long=mean(long),Lat=mean(lat))
ggplot(sales_map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = total_profit), color = "black")+
  scale_fill_gradient(low="pink",high="red",na.value = "white")+
  geom_text(data = state_data, aes(Long, Lat, label = region,group=1), 
            size = 3)+theme_void()

east_category=select(sales,Region,Category,Profit) %>%
  filter(Region=='East') %>%
  group_by(Category) %>%
  summarise(total_profit=sum(Profit));east_category

(ggplot(east_category,aes(x=east_category$Category,
                          y=east_category$total_profit,group=1)))+geom_bar(stat="identity")+
  labs(title = "category wise plot in east region",
       x="categories",y="Profit")

east=select(sales,Region,Category,Sub.Category,Profit) %>%
  filter(Region=='East') %>%
  group_by(Category,Sub.Category) %>%
  summarise(total_profit=sum(Profit));east

(ggplot(east,aes(x=east$Sub.Category,y=east$total_profit)))+
  geom_bar(stat="identity")+facet_grid(.~Category,scales = "free")+ 
  theme(axis.text.x = element_text(angle=50, hjust=1))+
  labs(title = "Sub-category wise plot in east region",
         x="Sub categories",y="Profit")  
