## Helper functions
##   (defined inside nomogrammer, so remain local only & wont clutter user env)
odds         <- function(p){
  # Function converts probability into odds
  o <- p/(1-p)
  return(o)
}

logodds      <- function(p){
  # Function returns logodds for a probability
  lo <- log10(p/(1-p))
  return(lo)
}

logodds_to_p <- function(lo){
  # Function goes from logodds back to a probability
  o <- 10^lo
  p <- o/(1+o)
  return(p)
}

p2percent <- function(p){
  # Function turns numeric probability into string percentage
  # e.g. 0.6346111 -> 63.5% 
  scales::percent(signif(p, digits = 3))}

fa_to_png_to_datauri <- function(name, ...) {
  
  tmpfl <- tempfile(fileext = ".png")
  
  fontawesome::fa_png(name, file = tmpfl, ...)
  
  knitr::image_uri(tmpfl)
  
}
