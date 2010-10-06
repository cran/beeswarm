\name{breast}
\alias{breast}
\docType{data}
\title{
Lymph-node-negative primary breast tumors
}
\description{
Tumor molecular measurements and outcome from breast cancer patients.
}
\usage{data(breast)}
\format{
  A data frame with 286 observations on the following 5 variables.
  \describe{
    \item{\code{ER}}{Estrogen receptor status, a factor with levels \code{neg} \code{pos}}
    \item{\code{ESR1}}{Expression of the ESR1 gene, a numeric vector}
    \item{\code{ERBB2}}{Expression of the ERBB2 gene, a numeric vector}
    \item{\code{time_survival}}{Time in months, a numeric vector}
    \item{\code{event_survival}}{Coded event, a numeric vector, 0 = censored, 1 = metastasis}
  }
}
\details{
ER, ESR1, and ERBB2 were measured on a tumor specimen surgery (time = 0).

ESR1 and ERBB2 expression values were determined by probe sets 205225\_at and 216836\_s\_at using RMA-normalized data.
}
\source{
Wang Y, Klijn JG, Zhang Y, Sieuwerts AM, Look MP, Yang F, Talantov D, Timmermans M, Meijer-van Gelder ME, Yu J, Jatkoe T, Berns EM, Atkins D, Foekens JA.
Gene-expression profiles to predict distant metastasis of lymph-node-negative primary breast cancer.
Lancet. 2005 Feb 19-25;365(9460):671-9.
}
\references{
}
\examples{
  data(breast)

  with(breast, 
    plot(ESR1, ERBB2, col = as.numeric(ER))
  )
}
\keyword{datasets}
