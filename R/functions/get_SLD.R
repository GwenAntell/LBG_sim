get_SLD <- function(x, y){
  diff <- x - y
  diff <- abs(diff)
  diff <- sum(diff, na.rm = TRUE)
  return(diff)
}