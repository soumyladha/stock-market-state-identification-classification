names = c("State",colnames(return_data))
i = 1
while (i<= ncol(centers)) {
  
  plot_data = as.data.frame(cbind(1:10,centers[,i:(i+10)]))
  colnames(plot_data) = names
  plot_data1 <- melt(plot_data, id="State")
  ggplot(data = plot_data1,aes(x = State,y = value,colour = variable))+geom_line() + 
    labs(title = paste("Correlation of",names[i%/%11+2],"vs Other Indices")
         , y = "Correlation") + theme(plot.title = element_text(hjust = 0.5))+
    scale_x_continuous(breaks=seq(0, 10, 1))
  ggsave(paste(names[i%/%11+2],".jpeg",sep = ""),dpi = 1200,height = 7.7,width = 10.3)
  i = i + 11
}
