occ_freq <- function(data, breaks){
  tmp <- data %>% group_by(id) %>% summarise(count=n())
  max <- max(tmp$count, na.rm = TRUE)
  brk <- seq(from = 0, to = (max+breaks), by = breaks)
  p <- hist(tmp$count, main = "Occurrence frequency distribution", breaks = brk, col = "grey80", freq = TRUE, ylab = expression(bold(Frequency)), xlab = expression(bold(Occurrences~(italic("n")))))
  counts <- p$counts
  int <- seq(breaks, (breaks*length(counts))+1, breaks)
  tmp <- cbind.data.frame(counts, int)
  colnames(tmp) <- c("counts", "bin")
  return(tmp)
}
