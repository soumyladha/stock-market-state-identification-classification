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
    abs_cov_mat[i,j] = (norm(cov_mat[[i]]-cov_mat[[j]],type = 'F'))/(n_col^2)
  }
}

ggplot(data = melt(abs_cov_mat), aes(x=Var1, y=Var2, fill=value)) + geom_tile() + 
  scale_fill_gradient2(low="white", high="navyblue", guide="colorbar") + 
  scale_x_continuous(breaks=seq(0, nrow(abs_cov_mat), 1000/4.1), labels = seq(2002, 2018, 1)) +
  scale_y_continuous(breaks=seq(0, nrow(abs_cov_mat), 1000/4.1), labels = seq(2002, 2018, 1)) +
  labs(title = "State Transitions",x = "Year",y = "Year") +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90))
ggsave("state heat.jpg",dpi = 1200,height = 7.7,width = 10.3)
dev.off()

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
temp1 = as.data.frame(cbind(1:40,squares))
ggplot(data = temp1,aes(x = V1,y = squares))+geom_line() + 
  labs(title = "Squared Error vs No. of Clusters",y = "Squared Error",x = "No. of Clusters")
ggsave("elbow.jpeg",dpi = 1200,height = 7.7,width = 10.3)
rm(temp1)
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
    theme(axis.text.x = element_text(angle = 90),axis.title.x=element_blank(),
          axis.title.y = element_blank(),plot.title = element_text(hjust = 0.5))+
    labs(title = paste("Correlation Matrix","of State:",i))
  ggsave(paste("Center",toString(i),".jpeg",sep = ""),dpi = 1200,height = 7.7,width = 10.3)
}

mmmodel = markovchainFit(data = final_cluster$cluster)
transition_mat=mmmodel$estimate@transitionMatrix
steadyStates(mmmodel$estimate)
write.csv(transition_mat,file = "tran.csv",row.names = TRUE,col.names = TRUE)

t_mat = mmmodel$estimate@transitionMatrix

ggplot(data = melt(t_mat), aes(x=Var1, y=Var2, fill=value)) + geom_tile() + 
  scale_fill_gradient2(low="white", high="navyblue", guide="colorbar",limits=c(0,1))+
  theme(axis.text.x = element_text(angle = 90))+ labs(x="State", y="State")+
  scale_x_continuous(breaks=seq(0, 10, 1))+ scale_y_continuous(breaks=seq(0, 10, 1))
ggsave(paste("Transition matrix",".jpeg",sep = ""),dpi = 600,height = 7.7,width = 10.3)

qplot(x = date_d[22:3985],y = final_cluster$cluster,xlab = "Date",ylab = "State")
ggsave(paste("State_transition_zoom",".jpeg",sep = ""),dpi = 600,height = 7.7,width = 10.3)