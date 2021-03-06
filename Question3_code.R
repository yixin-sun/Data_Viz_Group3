#431 Data Visualization Group HW Q3
#Is a movie does well in the U.S. also does well overseas?

movie<-read.csv('movie')
summary(movie)
# check if the class of Overseas and US revenue are numeric
class(movie$Overseas_rev)
class(movie$US_rev)
# find if these two variables are not, or moderate, or highly correlated
round(cor(movie$Overseas_rev, movie$US_rev, method='pearson'),digits=2)

#create scatter plot
install.packages("ggplot2")
p<-ggplot(data= movie, aes(x=movie$US_rev, y=movie$Overseas_rev))
#fix overplotting issue, draw curve instead of linear line to find the relationship between these two variables
p+geom_point(aes(x=movie$US_rev), alpha=1/5)+xlab("US Revenue")+ylab("Overseas Revenue")+
  geom_smooth(method="loess",fill='red')+
  labs(title = "Relationship Between US Revenue and Overseas Revenue", size=80)

#find good cutoff points
install.packages("data.table")

levels = c(0,100,Inf)
labels = c("negative","positive")

setDT(movie)[, grp := cut(US_rev, levels, labels)]
#cls %>%
#mutate(movie$grp=cut(movie$US_rev_num,levels,labels=labels))

q <- ggplot(data= movie, aes(x=movie$US_rev, y=movie$Overseas_rev,color=grp))
q + geom_point(aes(x=movie$US_rev), alpha=1/10) +
  xlab("US Revenue") + 
  ylab("Overseas Revenue")+
  geom_smooth(method="loess",fill=NA)+
  labs(title = "Relationship Between US Revenue and Overseas Revenue", size=80)

round(cor(movie[movie$grp=="negative"]$US_rev, movie[movie$grp=="negative"]$Overseas_rev),digits=3)
round(cor(movie[movie$grp=="positive"]$US_rev, movie[movie$grp=="positive"]$Overseas_rev),digits=3)

#scatter plot with the average US and Overseas revenue, color=genre
avg_rev_US_genre <- aggregate(movie$US_rev,by = list(Genre=movie$Genre), FUN = mean)
avg_rev_OV_genre <- aggregate(movie$Overseas_rev,by = list(Genre=movie$Genre), FUN = mean)
avg_budget_genre<-aggregate(movie$Overseas_rev,by = list(Genre=movie$Genre), FUN = mean)
avg_rev_genre1<-merge.data.frame(avg_rev_US_genre,avg_rev_OV_genre,by='Genre')
avg_rev_genre2<-merge.data.frame(avg_rev_genre1,avg_budget_genre,by='Genre')
avg_rev_genre2

ggplot(data=avg_rev_genre2, aes(x=x.x, y=x.y,colour=Genre,size=x))+
  geom_point()+theme(plot.title = element_text(hjust = 0.5))+
  xlab("Average US Revenue in $million") + ylab("Average Overseas Revenue in $million")+
  labs(title = "US Revenue vs Overseas Revenue by Genre", size=80)+
  geom_hline(yintercept=round(mean(avg_rev_OV_genre$x)),colour='blue')+
  geom_vline(xintercept=round(mean(avg_rev_US_genre$x)),colour='blue')

#scatter plot with the average US and Overseas revenue, color=studios
avg_rev_US_studio <- aggregate(movie$US_rev,by = list(Studios=movie$Studio), FUN = mean)
avg_rev_OV_studio <- aggregate(movie$Overseas_rev,by = list(Studios=movie$Studio), FUN = mean)
avg_budget_studio<-aggregate(movie$Overseas_rev,by = list(Studios=movie$Studio), FUN = mean)
avg_rev_studio1<-merge.data.frame(avg_rev_US_studio,avg_rev_OV_studio, by='Studios')
avg_rev_studio2<-merge.data.frame(avg_rev_studio1,avg_budget_studio,by='Studios')
avg_rev_studio2

ggplot(data=avg_rev_studio2, aes(x=x.x, y=x.y,colour=Studios,size=x))+
  geom_point()+theme(plot.title = element_text(hjust = 0.5))+
  xlab("Average US Revenue in $million") + ylab("Average Overseas Revenue in $million")+
  labs(title = "US Revenue vs Overseas Revenue by Studios", size=80)+
  geom_hline(yintercept=round(mean(avg_rev_OV_studio$x)),colour='blue')+
  geom_vline(xintercept=round(mean(avg_rev_US_studio$x)),colour='blue')

#explore the average IMDB ratings of the studios with high US revenue&low Overseas revenue
high_us_low_overseas<-subset(avg_rev_studio2,x.x>150 & x.y<200)
avg_rating_US_studio <- aggregate(movie$IMDb_Rating,by = list(Studios=movie$Studio), FUN = (mean))

#IFC, Orion
IMDB_rating1<-subset(avg_rating_US_studio,Studios %in% c('IFC','Orion'))
ggplot(data =IMDB_rating1,aes(x =Studios,y=x))+geom_bar(stat="identity",fill="lightblue")+
  theme(plot.title = element_text(hjust = 0.5))+ylab("IMDB Rating")+
  labs(title = "IMDB Ratings of High US&Low Oveseas Revenue by Studio")+
  geom_hline(yintercept=round(mean(IMDB_rating2$x)),colour='blue')

#explore the average IMDB ratings of the studios with low US revenue&high Overseas revenue
low_us_high_overseas<-subset(avg_rev_studio2,x.x<150 & x.y>mean(avg_rev_OV_studio$x))
low_us_high_overseas

#Gramercy Pictures,MGM,Path_Distribution,Screen Gems,Sony,Weinstein Company
IMDB_rating2<-subset(avg_rating_US_studio,Studios %in% c('Gramercy Pictures','MGM','Path_Distribution','Screen Gems','Sony','Weinstein Company'))
ggplot(data =IMDB_rating2,aes(x =Studios,y=x))+geom_bar(stat="identity",fill="lightcoral")+
  theme(plot.title = element_text(hjust = 0.5))+ylab("IMDB Rating")+
  labs(title = "IMDB Ratings of Low US&High Oveseas Revenue by Studio")+
  geom_hline(yintercept=round(mean(IMDB_rating2$x)),colour='blue')
