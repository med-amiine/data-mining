#Exercise 2.1

florist <- read.table("C:/Users/mohammed.idmoussi/Documents/R_projects/lab2.0/florist.txt", header = T, sep = ",")
florist_by_day <- aggregate(florist$price, by=list(florist$date), FUN = length)
#sum
names(florist_by_day) <- c('date', 'no_of_bouquets')
roses <- subset(florist, florist$bouquet=="roses")

roses_by_day <- aggregate(roses$price, by = list(roses$date), FUN = sum)
plot(roses_by_day)
outval = boxplot(roses_by_day, plot=FALSE)
boxplot(roses)
summary(roses)


binning <- function(x,bin_size, FUN = mean) {
  
  len <- length(x)
  y <- vector()
  for (i in 1:len){
    k = i%%bin_size
    if (k==0) {
      k1 <- k0 + bin_size - 1
      }
   else {
     k0 <- i-k+1
     k1 <- k0 +bin_size -1
   }
   if (k1>len) 
   {
     k1<-len
   }
    
  }
  y[i] = FUN(x[k0,k1])
  return(y)
}


roses_by_week <-roses_by_week[order(roses_by_week$year, roses_by_week$week)]
roses_by_day <- aggregate(roses$prices, by=list(roses$year, roses$week), FUN = length)
plot(roses_by_week$total,type='1')


