setwd("~/Desktop/BAX431")
movie = read.csv("movie.csv")
movie$Profit2 <- as.numeric(gsub(",","", movie$Profit))
movie$Adjusted_Gross2 <- as.numeric(gsub(",","", movie$Adjusted_Gross))
#Given genre sci-fi, find director
filter<-movie$Genre %in% c("sci-fi")
movie2<-movie[filter,]
mean <- aggregate(Adjusted_Gross2~Director, movie2,mean)
ggplot(data=mean, aes(x=Director, y=Adjusted_Gross2,fill=Director))+geom_bar(stat="identity")+ggtitle("Average Adjusted Gross Revenue by Director")
#Given genre sci-fi and budget, find studio
mean2 <- aggregate(Budget~Studio, movie2,mean)
ggplot(data=mean2, aes(x=Studio, y=Budget,fill=Studio))+geom_col()+ggtitle("Studio given Budget")+
  coord_flip()
#ROI by studio
movie2$Profit2 <- as.numeric(gsub(",","", movie2$Profit))
movie2$ROI <- as.numeric(movie2$Profit2/movie2$Budget)
mean3 <- aggregate(ROI~Studio, movie2,mean)
ggplot(data=mean3, aes(x=Studio, y=ROI,fill=Studio))+geom_col()+ggtitle("ROI by Studio")
