
#Loading the Dataset and installing required packages

data(ChickWeight)

install.packages(dplyr)
library(dplyr)
library(ggplot2)

#a. Extract a subset of the data for all chicks with complete information and name the data set complete. 

Chicksnew<-ChickWeight %>% group_by(Chick) %>% mutate(count = n())

complete<-subset(Chicksnew, Chicksnew$count==12)
incomplete<-subset(Chicksnew, Chicksnew$count!=12)

#b. A new variable 'weightgain' that measures the current weight difference compared to day 0.

complete<-complete %>% group_by(Chick) %>% mutate(weightgain = weight - first(weight))

#c. Using the ggplot2 package create side-by-side boxplots of weightgain by Diet for day 21. Describe the relationship in 2-3 sentences. Change the order of the categories in the Diet variable such that the boxplots are ordered by median weightgain.

Day21<- data.frame(subset(complete,Time==21))

ggplot (Day21, aes(Diet,weightgain))+geom_boxplot() + xlab("Diet")+ ylab("Weight Gain on Day21")

ggplot (Day21, aes(reorder(Diet,weightgain),weightgain))+geom_boxplot() + xlab("Diet")+ ylab("Weight Gain on Day21")

#d. Using the ggplot2 package create a plot with Time along the x axis and weight in the y axis. Facet by Diet.

ggplot (complete,aes(Time,weight,color=Diet,group=Chick))+geom_point()+geom_line()+facet_grid(Chick~Diet)+facet_wrap(~Diet)+theme(legend.position="bottom")

#e. Select the Chick with the maximum weight at Time 21 for each of the diets. Redraw the previous plot with only these 4 chicks.Compute average daily weights under each Diet and redraw the plot.


tapply(Day21$weight,Day21$Diet, max)


MaxWeight<-complete %>% group_by(Diet) %>%top_n(1, weight)

filter(complete, Time==21) %>% group_by (Diet)%>%top_n(1, weight)

Max<-subset(complete, Chick=c(7,21,35,48))

MaxPlot<-subset(complete, Chick==21|Chick==7|Chick==35|Chick==48)

ggplot (MaxPlot,aes(Time,weight,color=Diet))+geom_point()+geom_line()+facet_grid(Chick~Diet)+facet_wrap(~Diet)+theme(legend.position="bottom")+labs(title="Maximum Weight")

Avg<-complete%>% group_by(Time,Diet)%>%summarise(AvgWt=mean(weight))

ggplot (Avg,aes(Time,AvgWt,color=Diet))+geom_point()+geom_line()+facet_grid(Chick~Diet)+facet_wrap(~Diet)+theme(legend.position="bottom")+labs(title="Average Weight")

#________________________________________________________________________________________________________________


