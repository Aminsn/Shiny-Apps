# a function to change default y-axis label to x \times 10^y
scientific_10 <- function(x) {
  parse(text = gsub("e", "%*%10^", scales::scientific_format()(x)))
}

# split a vector by every nth element
n_by_n = function(x, n) { return(data.frame(split(x, 1+(seq_along(x)-1) %% n))) }