get_proportional <- function(x){
  x <- x/max(x, na.rm = TRUE)
  return(x)
}
