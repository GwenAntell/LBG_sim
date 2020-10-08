equation = function(x) {
  lm_coef <- list(pval = round(summary(x)$coefficients[8], digits = 3),
                  r2 = round(summary(x)$r.squared, digits = 3));
  if(lm_coef$pval <= 0.000){
    lm_coef$pval <- c("< 0.001")
  }
  lm_eq <- substitute(~~italic(R)^2~"="~r2~","~~italic(P)~"="~pval, lm_coef)
  lm_eq <- as.character(as.expression(lm_eq)) 
}

equation2 = function(x) {
  lm_coef <- list(pval = round(x$p.value, digits = 3),
                  r2 = round(as.numeric(x$estimate), digits = 3));
  if(is.na(lm_coef$pval)){
    lm_coef$pval <- NaN
    lm_coef$r2 <- NaN
  }
  else if(lm_coef$pval <= 0.000){
    lm_coef$pval <- c("< 0.001")
  }
  lm_eq <- substitute(~~italic(PPMCC)~"="~r2~","~~italic(P)~"="~pval, lm_coef)
  lm_eq <- as.character(as.expression(lm_eq)) 
}