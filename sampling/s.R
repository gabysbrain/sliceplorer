
library('devtools')
load_all('/Users/tom/Projects/R-optim-functions')

sample <- function(fname, method, N, d) {
  sample.func(fname, N, d, match.fun(method))
}

