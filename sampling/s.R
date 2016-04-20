
library('devtools')
load_all('/Users/tom/Projects/R-optim-functions')

sample <- function(fname, method, N, d) {
  sample.func(fname, N, d, match.fun(method))
}

range <- function(fname) {
  get_info(fname)$domain
}

getf <- function(fname) {
  get_info(fname)$func
}

