
x = read.csv("~/Downloads/cancer_data.csv") 
head(x)
dim(x)
str(x)
summary(x)

#Preprocessing 
	#Change the diagnosis column from "M" and "B" to 1 and 2
		x$diagnosis=ifelse(x$diagnosis == "M", 1, ifelse(x$diagnosis == "B", 2, x$diagnosis))
		x$diagnosis=as.numeric(x$diagnosis)
		head(x)
		xx= x$diagnosis
		
	
		s=sum(is.na(x$X))	
		s==nrow(x) #sum=number of observations so all the values are NA
		#Remove the column "X" because it consists of NAs only
		x=x[, !colnames(x) %in% c("X")]
		head(x)
			
	#Plot the data
		plot(x[,3:8],col=x$diagnosis+4,pch=19, main = "Plotting the first 6 observations between the two diagnostic groups") #the one with the least overlap 
		quartz()
		plot(x[,9:14],col=x$diagnosis+4,pch=19, main = "Plotting the 7th to the 13th observations between the two diagnostic groups") #too much overlap here 
		quartz()
		plot(x[,15:20],col=x$diagnosis+4,pch=19, main = "Plotting the 14th to the 19th observations between the two diagnostic groups") #too much overlap here 
		quartz()
		plot(x[,21:26],col=x$diagnosis+4,pch=19, main = "Plotting the 20th to the 25th observations between the two diagnostic groups") #too much overlap here 
		quartz()
		plot(x[,27:32],col=x$diagnosis+4,pch=19,main = "Plotting the 26th to the 31st observations between the two diagnostic groups") #too much overlap here 
		
	#Normalize the numeric variables
		z=cbind(x[,1:2],scale(x[,3:32]))
		z=data.frame(z)

		head(z)

#Take the first eight columns as new subset 
	z1=z[,1:8]
		
#Print the first six observations in the subset 
	head(z1)

#Print the number of rows and columns in the subset 
	dim(z1)

#Plot the new subset 
	plot(z1[,3:8],col=z1$diagnosis)

#Clustering Algorirthms 
	#Hierarchical Clustering
	#eucledian distance and ward
	d = dist(z1[,3:8], method = "euclidean") # method:"manhattan","canberra", # "minkowski“, etc. 
	hc = hclust(d, method="ward.D2") # method: "single", "complete", "average“, etc. 
	# display dendrogram 
		plot(hc,main="Cluster Dendogram with k=4") 
		clusters=cutree(hc, k=4) # cut tree into 3 clusters  
		rect.hclust(hc, k=4, border="red") # draw dendogram with red borders around them
	n=8
	for (i in 3:n){
		for (j in 3:n){
			centers=aggregate(z1[,c(i,j)], list(clusters), mean) 
			centers=centers[,-1]; 
			k=nrow(centers) 
			quartz()
			plot(z1[,c(i,j)],col=clusters , pch=19, cex=0.75) 
			points(centers, col = 1:k, pch = 8, cex=2) 
			points(centers, col = 1:k, pch = 19, cex=1) 
			clusters
			table(clusters) 
			centers	
			}
			quartz()
		}
		#goodness of fit function
		R2=function(x,clusters,k=3){ 
			n=nrow(x)
			tss=var(x)
			tss=(n-1)*sum(diag(tss))
			wss=0 
			for(j in 1:k){ 
				cj=x[clusters==j,]
				nj=nrow(cj)
				vj=var(cj)
				wssj=0 
				if(is.matrix(cj)) wssj=(nj-1)*sum(diag(vj))
				wss=wss+wssj 
				}
				r2=1-wss/tss
				cat("R2 = ",r2,"\n") 
				return(r2) 
				} 
				#goodness of fit
				r2=R2(z1[,3:8],clusters,3)
				
		#eucledian distance and average linkage
			d = dist(z1[,3:n], method = "euclidean") # method:"manhattan","canberra", # "minkowski“, etc. 
			hc = hclust(d, method="average") # method: "single", "complete", "average“, etc. 
			# display dendrogram 
				plot(hc) 
			clusters=cutree(hc, k=3) # cut tree into 3 clusters  
			rect.hclust(hc, k=3, border="red") # draw dendogram with red borders around them
			n=8
			for (i in 3:n){
				for (j in 3:n){
					centers=aggregate(z1[,c(i,j)], list(clusters), mean) 
					centers=centers[,-1]; 
					k=nrow(centers) 
					quartz()
					plot(z1[,c(i,j)],col=clusters , pch=19, cex=0.75) 
					points(centers, col = 1:k, pch = 8, cex=2) 
					points(centers, col = 1:k, pch = 19, cex=1) 
					clusters
					table(clusters) 
					centers	
					}
					quartz()
				}
				#goodness of fit
				r2=R2(z1[,3:8],clusters,3)
				
		#manhattan distance and ward		
			d = dist(z1[,3:n], method = "manhattan") # method:"manhattan","canberra", # "minkowski“, etc. 
			hc = hclust(d, method="ward.D2") # method: "single", "complete", "average“, etc. 
			# display dendrogram 
				plot(hc) 
			clusters=cutree(hc, k=3) # cut tree into 3 clusters  
			rect.hclust(hc, k=3, border="red") # draw dendogram with red borders around them
			n=8
			for (i in 3:n){
				for (j in 3:n){
					centers=aggregate(z1[,c(i,j)], list(clusters), mean) 
					centers=centers[,-1]; 
					k=nrow(centers) 
					quartz()
					plot(z1[,c(i,j)],col=clusters , pch=19, cex=0.75) 
					points(centers, col = 1:k, pch = 8, cex=2) 
					points(centers, col = 1:k, pch = 19, cex=1) 
					clusters
					table(clusters) 
					centers	
					}
					quartz()
				}
				#goodness of fit
				r2=R2(z1[,3:8],clusters,3)
				
		#manhattan distance and average linkage
			d = dist(z1[,3:n], method = "manhattan") # method:"manhattan","canberra", # "minkowski“, etc. 
			hc = hclust(d, method="average") # method: "single", "complete", "average“, etc. 
			# display dendrogram 
				plot(hc) 
			clusters=cutree(hc, k=3) # cut tree into 3 clusters  
			rect.hclust(hc, k=3, border="red") # draw dendogram with red borders around them
			n=8
			for (i in 3:n){
				for (j in 3:n){
					centers=aggregate(z1[,c(i,j)], list(clusters), mean) 
					centers=centers[,-1]; 
					k=nrow(centers) 
					quartz()
					plot(z1[,c(i,j)],col=clusters , pch=19, cex=0.75) 
					points(centers, col = 1:k, pch = 8, cex=2) 
					points(centers, col = 1:k, pch = 19, cex=1) 
					clusters
					table(clusters) 
					centers	
					}
					quartz()
				}
				#goodness of fit
				r2=R2(z1[,3:8],clusters,3)
				
#K-MEANS--------------------------------------------------------------------------
head(x)
xs= x[3:8]
head(xs)

# Determining the number of clusters 
x=scale(xs, center=TRUE, scale=TRUE) # scale x 
head(x)
dim(x)
wss = (nrow(x)-1)*sum(apply(x,2,var))
for (i in 2:10) {
  wss[i] = sum(kmeans(x,centers=i)$withinss)} 
plot(wss, type="b", pch=19, xlab="k",ylab="WSS", main="The L-Curve")

#can't decide number of clusters from it, need to calculate the error rate for each
  #Estimate a range between 2 and 4 for the value of K.

#K-Means Clustering using EUCLIDEAN
  #K=3
head(x)
k=3
kmc = kmeans(x, k)
kmc
clusters=kmc$cluster
pairs(x, pch=19, col = kmc$cluster, main= "K-means Clustering with Euclidean Distance (K=3)") 
points(kmc$centers, col = 1:k, pch = 8, cex=2) 
points(kmc$centers, col = 1:k, pch = 19, cex=1) 
clusters
table(clusters)
kmc$centers
r3=R2(x, clusters, k)

  #K=4
k=4
kmc = kmeans(x, k)
kmc
clusters=kmc$cluster
pairs(x, pch=19, col = kmc$cluster, main= "K-means Clustering with Euclidean Distance (K=4)") 
points(kmc$centers, col = 1:k, pch = 8, cex=2) 
points(kmc$centers, col = 1:k, pch = 19, cex=1) 
clusters
table(clusters)
kmc$centers
r4=R2(x, clusters, k)

  #K=2
k=2
kmc = kmeans(x, k)
kmc
clusters=kmc$cluster
pairs(x, pch=19, col = clusters, main= "K-means Clustering with Euclidean Distance (K=2)") 
points(kmc$centers, col = 1:k, pch = 8, cex=2) 
points(kmc$centers, col = 1:k, pch = 19, cex=1) 
clusters
table(clusters)
kmc$centers
r2=R2(x, clusters, k)

#ERROR RATE
actual_diagnosis = xx
# Define a function to calculate the error rate
calculate_error_rate <- function(clusters, actual_diagnosis) {
  contingency_table <- table(clusters, actual_diagnosis)
    max_values <- apply(contingency_table, 1, max)
    correct_assignments <- sum(max_values)
    total_observations <- sum(contingency_table)
    error_rate <- 1 - correct_assignments / total_observations
  return(error_rate)
}

# For K=3
k3_clusters <- kmeans(x, centers = 3)$cluster
error_rate_k3 <- calculate_error_rate(k3_clusters, actual_diagnosis)
print(paste("Error Rate for K=3 using Euclidean dist:", error_rate_k3))

# For K=2
k2_clusters <- kmeans(x, centers = 2)$cluster
error_rate_k2 <- calculate_error_rate(k2_clusters, actual_diagnosis)
print(paste("Error Rate for K=2 using Euclidean dist:", error_rate_k2))

# For K=4
k4_clusters <- kmeans(x, centers = 4)$cluster
error_rate_k4 <- calculate_error_rate(k4_clusters, actual_diagnosis)
print(paste("Error Rate for K=4 using Euclidean dist:", error_rate_k4))

#K-mean Clustering using MANHATTAN

install.packages("amap")
library(amap)
df=as.matrix(x)
head(df)

#K=2
k=2
kmc = Kmeans(df, k, method = "manhattan", iter.max = 100)
kmc
clusters=kmc$cluster
pairs(x, pch=19, col = kmc$cluster,main = "K-means Clustering with Manhattan Distance (K=2)") 
points(kmc$centers, col = 1:k, pch = 8, cex=2) 
points(kmc$centers, col = 1:k, pch = 19, cex=1) 
clusters
table(clusters)
kmc$centers
error_rate_k2 <- calculate_error_rate(clusters, actual_diagnosis)
print(paste("Error Rate for K=2 using Manhattan dist:", error_rate_k2))
r2=R2(x, clusters, k)


#K=3
k=3
kmc = Kmeans(df, k, method = "manhattan", iter.max = 100)
kmc
clusters=kmc$cluster
pairs(x, pch=19, col = kmc$cluster,main = "K-means Clustering with Manhattan Distance (K=3)") 
points(kmc$centers, col = 1:k, pch = 8, cex=2) 
points(kmc$centers, col = 1:k, pch = 19, cex=1) 
clusters
table(clusters)
kmc$centers
error_rate_k3 <- calculate_error_rate(clusters, actual_diagnosis)
print(paste("Error Rate for K=3 using Manhattan dist:", error_rate_k3))
r3=R2(x, clusters, k)

#K=4
k=4
kmc = Kmeans(df, k, method = "manhattan", iter.max = 100)
kmc
clusters=kmc$cluster
pairs(x, pch=19, col = kmc$cluster,main = "K-means Clustering with Manhattan Distance (K=4)") 
points(kmc$centers, col = 1:k, pch = 8, cex=2) 
points(kmc$centers, col = 1:k, pch = 19, cex=1) 
clusters
table(clusters)
kmc$centers
error_rate_k4 <- calculate_error_rate(clusters, actual_diagnosis)
print(paste("Error Rate for K=4 using Manhattan dist:", error_rate_k4))
r4=R2(x, clusters, k)
