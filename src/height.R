heightfoot <- read.table("../data/height_foot.txt", header=T)
attach(heightfoot)
plot(height, foot)
model.1 <- lm(foot ~ height)
summary(model.1)

which(height>80) # 28
model.2 <- lm(foot ~ height, subset=(1:33)[-28]) # exclude obs #28
summary(model.2)

cook <- cooks.distance(model.1)
cook

dot_plot<-function( this_list, ... )
{ 
  ## the first thing to do is to just sort the list into a local copy
  
  lcl_list <- sort( this_list )
  
  ## then we want a second list that is just as long as was the
  ## original list, because, in that second copy we will place the
  ## vertical position of the associated value in the sorted copy
  
  lcl_count <- lcl_list
  
  ## then, to start, we begin at the first item in the sorted list 
  ## It will have  avertical position of 1
  cur_val <- lcl_list[1]
  m <- 1
  lcl_count[1]<-1
  
  ## now we just move through the rest of the sorted
  ## list and if we are at the same value then we go up one 
  ## vertical level, but if we are at a new value we reset
  ## the vertical position to 1
  
  for (i in 2:length(lcl_list))
  { 
    x <- lcl_list[i]
    if ( x==cur_val )
    { m <- m+1
      lcl_count[ i ] <- m
    }
    else
    {
      cur_val <- x
      m <- 1
      lcl_count[i] <- m
    }
  }
  
  ## once we are done with that, we can just do a scatter plot on
  ## the two vectors that we have created.

  
  plot(lcl_list,lcl_count, xlab="COOK DISTANCE",ylab='freq', ...)
}
dot_plot(cooks.distance(model.1))
detach(heightfoot)