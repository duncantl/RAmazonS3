\name{removeFile}
\Rdversion{1.1}
\alias{removeFile}
\alias{removeBucket}
\title{Remove an S3 bucket or object}
\description{
  These functions allow the R user to delete/remove
  an S3 bucket or object within a bucket.
}
\usage{
removeFile(bucket, name, auth = getOption("AmazonS3"),
            curl = getCurlHandle(), virtual = (tolower(bucket) == bucket))
removeBucket(bucket, auth = getOption("AmazonS3"), 
              curl = getCurlHandle(),
                virtual = (tolower(bucket) == bucket))
}
%- maybe also 'usage' for other objects documented here.
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
  A logical value indicating whether the operation was successful
  (\code{TRUE}) or not (\code{FALSE}).
}
\references{
\url{http://docs.amazonwebservices.com/AmazonS3/latest/gsg/}  
}
\author{
Duncan Temple Lang <duncan@wald.ucdavis.edu>
}

\seealso{
  \code{\link{addFile}}
  \code{\link{getFile}}
  \code{\link{copyFile}}  
}
\examples{

}
\keyword{IO}
