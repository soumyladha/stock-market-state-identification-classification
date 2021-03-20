

data = read.csv(file.choose(),stringsAsFactors = FALSE)
close = data[2]
date_data = data[1]
date_data = date_data[-(1:50),1]

close = ts(close)

n_row = NROW(close)

error = numeric(length = n_row-50)

for(i in 50:(n_row-1))
{
  model_window=auto.arima(close[(i-49):i])
  frcast = forecast(model_window,h = 1)
  error[i-49] = frcast$mean-close[i+1]
  
}
error_sorted = sort(error)

n_data = length(error_sorted)

# Applying Stationary test on Error to build markov chain

adf.test(error)

min_error = error_sorted[round(1*n_data/100)]
max_error = error_sorted[round(99*n_data/100)]
mean_error = (min_error+max_error)/2
mid_min = (min_error+mean_error)/8
mid_max = (max_error+mean_error)/8

error_state = vector(mode = "character",length = length(error))

for(i in 1:length(error))
{
  if (error[i]<mid_min)
  {
    error_state[i] = "Very Low"
  }
  else if (error[i]<mean_error)
  {
    error_state[i] = "Low"
  }
  else if (error[i]<mid_max)
  {
    error_state[i] = "High"
  }
  else
  {
    error_state[i] = "Very High"
  }
}

mcFit <- markovchainFit(data=error_state)
mcFit$estimate
steadyStates(mcFit$estimate)
plot(mcFit$estimate)

date_ = as.data.frame(date_data,stringsAsFactors = FALSE)
N_date = nrow(date_)
date_ = as.Date(date_[1:N_date,1],"%d-%m-%Y")


qplot(x = date_[3000:3500],y = error_state[3000:3500],xlab = "Date",ylab = "State")
ggsave(paste("State_transition_zoom",".jpeg",sep = ""),dpi = 600,height = 7.7,width = 10.3)
