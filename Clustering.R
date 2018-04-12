#Load dataset and transformation
head(movie)
dim(movie)
mov<- data.matrix(movie)
warnings()

#selecting sample of 500 values
smple<-mov[sample(nrow(mov),500),]
View(smple)
#omiting the NA's and selecting the gross and the budget columns
smple_short<- smple[,c(9,23)]
smple_short<- na.omit(smple_short)
View(smple_short)
smple_matrix<- data.matrix(smple_short)
View(smple_matrix)

#Elbow curve
wss<-(nrow(smple_matrix)-1)*sum(apply(smple_matrix,2,var))
for(i in 2:15) wss[i] <- sum(kmeans(smple_matrix,centers =i)$withinss)
plot(1:15,wss,type ="b", xlab = "Number of Clusters", ylab = "Within sum of squares") #Elbow plot

#K-means clustering
cl<- kmeans(smple_matrix,3,nstart = 25)
plot(smple_matrix,col=(cl$cluster +1), main = "K-means result with 3 clusters", pch =1, cex = 1, las = 1)
points(cl$clusters, col = 12, pch = 8, cex = 2)

cl$centers

#aggregate(data = smple, movie_facebook_likes ~ cluster, mean)

#aggregate(data = smple, director_facebook_likes ~ cluster, mean)
