#Question 2
geren_num = sqldf("select Genre, count(*) as genre_size from movie group by Genre order by Genre")

genre_name = time_n_revenue[,1]
genre_size = geren_num[,1]
time_rating = time_n_rating[,2]
time_profit = time_n_profit[,2]
correlation_2 = cbind.data.frame(genre_name,genre_size , time_rating, time_profit)
correlation_2


library(ggplot2)
p = ggplot(data=correlation_2,aes(x=time_profit, y=time_rating, color=genre_name, size=genre_size))

p+geom_point() +
  xlab("Runtime & Profit Coefficient") + 
  ylab("Runtime & Rating Coefficient") 
#labs(title = "Relationship Between US Revenue and Overseas Revenue", size=60)

