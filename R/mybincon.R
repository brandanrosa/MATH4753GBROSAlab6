#' mybincon
#'
#' Produces a simulation for the binomial distribution with a bar-plot and comparison to the true proportion values
#'
#' @param iter number of iterations
#' @param n number in each sample
#' @param p probability of success
#' @param ... pass additional options to the argument
#'
#' @return a summary
#' @export
#'
#' @examples \dontrun{mybincon(1000, 10 , 0.6)}
mybincon = function(iter, n, p, ...) {
  sam_mat <- matrix(NA,
                    nrow = n,
                    ncol = iter,
                    byrow = TRUE)

  succ <- vector(mode = "numeric",
                 length = iter)

  for (i in 1:iter) {
    sam_mat[, i] <- sample(
      x = c(1, 0),
      size = n,
      replace = TRUE,
      prob = c(p, 1 - p)
    )

    succ[i] <- sum(sam_mat[, i])
  }

  succ.tab <- table(factor(succ, levels = 0:n))

  st <- succ.tab / iter

  mdpts <- barplot(
    height = st,
    col = rgb(red = 0.15, green = st, blue = 0, alpha = .75),
    main = "Binomial Simulation - Brandan Rosa",
    xlab = "Number of Successes",
    ...
  )

  tp <- dbinom(0:n, size = n, prob = p)

  esd <- dist(rbind(st, tp))

  l <- list(estp = st, tp = tp, esd = esd, mdpts = mdpts, sim = sam_mat)
  #invisible(l)
  l
}
