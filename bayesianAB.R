# See http://www.evanmiller.org/bayesian-ab-testing.html
#
# Formula for computing the Bayesian probability that B is better than A
#
# α_A - one plus the number of successes for A
# β_A - one plus the number of failures for A
# α_B - one plus the number of successes for B
# β_B - one plus the number of failures of B

a_better_than_b <- function(α_A, β_A, α_B, β_B) {
  val <- 0

  for (i in 0:α_B-1) {
    val <- val + exp(lbeta(α_A + i, β_B + β_A) - log(β_B + i) - lbeta(1+i, β_B) - lbeta(α_A, β_A))
  }

  return (val)
}
