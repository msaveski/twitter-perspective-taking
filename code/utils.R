
#
# TexReg method for generating tables from BRMS models
#
extract.brmsfit <- function(model) {
  s <- summary(model)
  
  names <- rownames(s$fixed)
  co <- s$fixed[, 1]
  ci.low <- s$fixed[, 3]
  ci.up <- s$fixed[, 4]
  
  gof <- c(s$spec_pars[, "Rhat"])
  gof.names <- c("Rhat")
  
  tr <- createTexreg(
    coef.names = names,
    coef = co,
    ci.low = ci.low,
    ci.up = ci.up,
    gof.names = gof.names,
    gof = gof
  )
  return(tr)
}

setMethod("extract",
          signature = className("brmsfit", "brms"),
          definition = extract.brmsfit)

