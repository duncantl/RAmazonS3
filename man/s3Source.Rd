\name{s3Source}
\Rdversion{1.1}
\alias{s3Source}
\title{Source R code from S3}
\description{
  This function sources R code from S3. This can be compared to the \code{\link[base]{source}} function.
}
\usage{
s3Source(bucket, name, envir = parent.frame(), auth = getOption("AmazonS3"),
           curl = getCurlHandle(), virtual = (tolower(bucket) == bucket))
}
\arguments{
 \item{bucket}{the name of the AWS bucket. One can also specify the
    bucket and object/file name as "bucket/name".}
  \item{name}{the name of the object within the bucket to remove}
  \item{auth}{a named character vector giving the AWS key and secret as a \code{c(key = secret)}.}  
  \item{curl}{a curl handle that the caller can specif to reuse an
      existing object that is resused across different libcurl calls.}
  \item{virtual}{a logical value indicating whether to use the virtual
    host mechanism or \code{s3.amazonaws.com/bucket/obj} format.
    The default argument is most likely the appropriate value.
  }  
}
\value{

}
\references{
\url{http://docs.amazonwebservices.com/AmazonS3/latest/gsg/}  
}
\author{
Markus Schmidberger <schmidbe@amazon.de>  
}

\examples{
}
\keyword{IO}
