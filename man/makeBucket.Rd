\name{makeBucket}
\Rdversion{1.1}
\alias{makeBucket}
\title{Create a top-level S3 bucket}
\description{
  This function creates a top-level bucket for containing objects.
}
\usage{
makeBucket(name, location = NA, auth = getOption("AmazonS3"), curl = getCurlHandle(), virtual = TRUE)
}
\arguments{
  \item{name}{the name of the new S3 bucket to create.}
  \item{location}{a string that specifies where the bucket should be
    located, i.e. US or Europe.}
  \item{auth}{a named character vector giving the AWS key and secret as
    a \code{c(key = secret)}.}
  \item{curl}{a curl handle that the caller can specif to reuse an
      existing object that is resused across different libcurl calls.}
  \item{virtual}{a logical value indicating whether to use the virtual
    host mechanism or \code{s3.amazonaws.com/bucket/obj} format.
    The default argument is most likely the appropriate value.
  }
}
\value{
A logical value indicating whether the request was successful.  
}
\references{
 \url{http://docs.amazonwebservices.com/AmazonS3/latest/gsg/}  
}
\author{
Duncan Temple Lang <duncan@wald.ucdavis.edu>    
}

\seealso{
  \code{\link{listBucket}}
  \code{\link{listBuckets}}
  \code{\link{getFile}}
  \code{\link{addFile}}    
}
\examples{

}
\keyword{ }
