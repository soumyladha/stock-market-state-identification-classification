ggplot(data=as.data.frame(error), aes(as.data.frame(error))) + 
  geom_histogram(breaks=seq(-1000, 1000, by = 100), 
                 col="red", 
                 fill="blue", 
                 alpha = .2)+labs(title="Histogram of Error") + labs(x="Error", y="Count")
ggsave(paste("Histogram",".jpeg",sep = ""),dpi = 600,height = 7.7,width = 10.3)


dummy = cbind(as.data.frame(error),1 )
ggplot(as.data.frame(error), aes(x = "", y = error)) +
  geom_boxplot()+labs(title="Boxplot of Error") + labs(x="", y="Error")
ggsave(paste("BoxPlot",".jpeg",sep = ""),dpi = 600,height = 7.7,width = 10.3)

t_mat = mcFit$estimate@transitionMatrix

ggplot(data = melt(t_mat), aes(x=Var1, y=Var2, fill=value)) + geom_tile() + 
  scale_fill_gradient2(low="white", high="navyblue", guide="colorbar",limits=c(0,1))+
  theme(axis.text.x = element_text(angle = 90))+ labs(x="State", y="State")
ggsave(paste("State Matrix",".jpeg",sep = ""),dpi = 600,height = 7.7,width = 10.3)

png("MChain Plot.png",res = 600,width=480, height=240)
plot(mcFit$estimate)
dev.off()
