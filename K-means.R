data(wine, package="rattle")
head(wine)
wssplot(df)                                                #2
library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]), 
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

set.seed(1234)
fit.km <- kmeans(df, 3, nstart=25)                           #3
fit.km$size

fit.km$centers
aggregate(wine[-1], by=list(cluster=fit.km$cluster), mean)
