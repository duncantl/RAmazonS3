\name{about}
\Rdversion{1.1}
\alias{about}
\alias{getInfo}
\title{Get meta-information about S3 file or bucket}
\description{
  This function retrieves meta-information for an
  S3 file/object or bucket.
  This information includes the date last modified
  and explicitly added meta data in
  'x-amz-meta-*' headers. These can be specified
  when storing an object (via the \code{meta}
  parameter of \code{\link{addFile}}).
}
\usage{
about(bucket, name = character(), auth = getOption("AmazonS3"),
       curl = getCurlHandle(), virtual = (tolower(bucket) == bucket),
        asHeader = FALSE)
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
  \item{asHeader}{a logical indicating that the parsed HTTP header
    should be returned directly and not filtered.}
}
\value{
 a named character vector giving the meta-information 
}
\references{
\url{http://docs.amazonwebservices.com/AmazonS3/latest/gsg/}  
}
\author{
Duncan Temple Lang <duncan@wald.ucdavis.edu>  
}
\seealso{
  \code{\link{listBuckets}}  
  \code{\link{listBucket}}
  \code{\link{getFile}}  
}
\examples{
}
\keyword{IO}

