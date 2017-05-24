#We first view  the data set to decide how to clean the data set
View(movies)
 
#as it is a text file delimited with commas, we use read.delim function to import it as a data frame
movie <- read.delim("data location/movies-1.txt", header = F, sep = "|")

#there are no colomn names for the data, hence we provide the names as per the data dictionary
View(movie)

#now lets analyse the data type of the data frame and also the summary
str(movie)
summary(movie)

#giving colomn names for the data set and storing it as a vector

header <- c("ID","Title","ReleaseDate","imdblink",
            "unknown","action","adventure","animation","children","crime","prime","documentory","drama","fantasy","filmnoir",
            "horror","musical","Mystery","Romance","scifi","thriller","war","western")

#data cleaning is done at this stage we remove unwanted columns from the data set which is v4 (NULL) and v1 (Serial number)
View(movie)
movie$V4 <- NULL
movie$V1 <- NULL
View(movie)

#giving colomn names
new <- setNames(cbind(rownames(movie), movie, row.names = NULL),header)
View(new)  
new <- unique(new)
colsum <- colSums(new[,5:23]) 
colsum
as.matrix(colsum)

#now we take all the genres and store it as a data frame and their sum also as a frquency
gener <- as.data.frame(as.matrix(colsum))
#naming the sum of genres as frequency
colnames(gener) <- "Frequency"

#euclidean method using dist to find the distance of the nodes and cluster them into complete linkage clustering
distances <- dist(new[5:23], method = "euclidean")
distances

#Hierarchical clustering using hclust and method war.D2
clustermovie <- hclust(distances, method = "ward.D2")

#creating a Dendogram
plot(clustermovie)

#Assign points to clusters (we only take clusters with in 10 to 25 in the x axis)
clustergroups <- cutree(clustermovie, k=10)
#now we view the cluster points 
head(clustergroups, n =20)

movies <- cbind(new, clustergroups)
 
 
#understandin the clusters

tapply(movies$action, movies$clustergroups, mean)
tapply(movies$western, movies$clustergroups, mean)
tapply(movies$war, movies$clustergroups, mean)

gener.title <- c("unknown","action","adventure","animation","children","crime","prime","documentory","drama","fantasy","filmnoir",
            "horror","musical","Mystery","Romance","scifi","thriller","war","western")

List  = list()
# we assign names to each cluster with the approriate genre name for speeding up the process I have used a for loop
for(i in gener.title)
{
  newcol <- tapply(movies[,i], movies$clustergroups, mean)
  List[[i]] = newcol
}
#list has the genre and their scores for each cluster
List
# making cluster into a data frame for further analysis
List <- as.data.frame(List)
#we get the movie id by providing the movie name
which(movies$Title == "Men in Black (1997)")
# movie id 257
which(movies$Title == "Star Wars (1977)")
# movie id 50
which(movies$Title == "Toy Story (1995)")
# movie id 1

#To find from which cluster the movie belong to we provide the movie id 
movies$clustergroups[257] 
movies$clustergroups[50] 
movies$clustergroups[1]


#conclusion 
#After getting the cluster number we come to know which movie is from which cluster.
#for example now there are several users who like some specific movie in horror and sci-fy we can recommand them with other movies in the same genre 
#it is possible because we know from which cluster he likes those movies