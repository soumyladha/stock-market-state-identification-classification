sector_data = read.csv(file.choose(),stringsAsFactors = FALSE)

date_d = sector_data$Date
sector_data$Date = NULL
sector_data$Teck = NULL

return_data = diff(as.matrix(log(sector_data)))
n_row = nrow(return_data)
cov_mat = list()
n_col = ncol(sector_data)
for(i in 21:n_row)
{
  cov_mat[[i-20]] = cor(return_data[(i-20):(i),1:n_col])
}

abs_cov_mat = matrix(data = 0,length(cov_mat),length(cov_mat))

for(i in 1:length(cov_mat))
{
  for(j in 1:length(cov_mat))
  {
    abs_cov_mat[i,j] = sum(abs(cov_mat[[i]]-cov_mat[[j]]))
  }
}
nrow1 = length(cov_mat)
knn_matrix = matrix(data = NA,nrow = nrow1,ncol = n_col^2)

for(i in 1:length(cov_mat))
{
  knn_matrix[i,1:n_col^2] = array(cov_mat[[i]])
}

squares = numeric(length = 40)
for(i in 1:40)
{
  clusters = kmeans(knn_matrix,i)
  squares[i] = clusters$tot.withinss
  
}

png(filename="Elbow Clustering.png")
plot(squares,xlab = "cluster size",ylab = "square error",type = "l")
dev.off()

# Choosing 8 clusters
n_cluster = 10
final_cluster = kmeans(knn_matrix,n_cluster)
centers = final_cluster$centers

centers_list = list()
for(i in 1:n_cluster)
{
  centers_list[[i]] = matrix(centers[i,1:n_col^2],n_col,n_col)
}
names_sector = colnames(sector_data)
for(i in 1:n_cluster)
{
  rownames(centers_list[[i]]) = names_sector
  colnames(centers_list[[i]]) = names_sector
  ggplot(data = melt(centers_list[[i]]), aes(x=Var1, y=Var2, fill=value)) + geom_tile() + 
    scale_fill_gradient2(low="white", high="navyblue", guide="colorbar")+
    theme(axis.text.x = element_text(angle = 90))
  ggsave(paste("center",toString(i),".jpeg",sep = ""),dpi = 600,height = 15.4,width = 20.6)
}

mmmodel = markovchainFit(data = final_cluster$cluster)
mmmodel$estimate
