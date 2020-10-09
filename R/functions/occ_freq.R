occ_freq <- function(data, breaks){
  tmp <- data %>% dplyr::group_by(id) %>% dplyr::summarise(count=n())
  max <- max(tmp$count, na.rm = TRUE)
  brk <- seq(from = 0, to = (max+breaks), by = breaks)
  p <- hist(tmp$count, main = "", breaks = brk, col = "#3182bd", freq = TRUE, ylab = expression(bold(Frequency)), xlab = expression(bold(Occurrences~(italic("n")))))
  counts <- p$counts
  int <- seq(breaks, (breaks*length(counts))+1, breaks)
  tmp <- cbind.data.frame(counts, int)
  colnames(tmp) <- c("counts", "bin")
  return(tmp)
}
