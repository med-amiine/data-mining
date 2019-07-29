
binning <- function(x, bin_size, FUN = mean) {
  len <- length(x)
  y <- vector()
  for(i in 1:len) {
    k <- i %% bin_size
    if(k == 0) { k0 <- i - bin_size + 1
    } else { k0 <- i - k + 1 }
    k1 <- k0 + bin_size - 1
    if(k1 > len) { k1 <- len }
    y[i] <- FUN(x[k0:k1])
  }
  return(y)
} 


bouquets_by_province <- aggregate(florist$bouquet,by=list(florist$year, florist$sender_province), FUN = sum)






min_max_nom <- function(x,new_max=1,new_min=0,na.rm=TRUE){
  min_x = min(x,na.rm)
  max_x = max(x,na.rm)
  new_x <- (x-min_x)/(max_x-min_x)*(new_max-new_min)+new_min
  return(new_x)
}

x = bouquets_by_province$total

province_norm_1 = min_max_nom(x,1,0,TRUE)

#z-score normalization

x_new = (x-mean(x))/sd(x)



#https://docs.google.com/document/d/1R3ymqNpNdOiQ5NLVMlrg52pTNX8bHQjIO4ahJqGwC50/edit?fbclid=IwAR3aRoyB9fhru9fdE5CUV1Na9wGwxv5FWvgUV4t6QDdfTmdk5ftM1fwDMtI#

