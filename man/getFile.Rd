\name{getFile}
\Rdversion{1.1}
\alias{getFile}
\alias{s3Exists}
\title{Retrieve the contents of an object stored on the S3 server}
\description{
  \code{getFile} function returns the contents of the specified
  object that is stored on the S3 object.
  It returns the contents that were uploaded and
  specifies the content type for the file's contents.

  \code{s3Exists} tests whether the specified bucket/object
  exists and can be read.
}
\usage{
getFile(bucket, name, auth = getOption("AmazonS3"), curl = getCurlHandle(),
           binary = NA, virtual = (tolower(bucket) == bucket), verbose = FALSE)
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
  \item{binary}{a logical value (\code{TRUE}, \code{FALSE} or \code{NA})
     indicating whether the content of the file is known to be binary or
   not, or unknown.  The retrieval mechanism attempts to understand the
   Content-Type to determine this, erring on the side of treating the
   content as binary. See \code{rawToChar} to convert the resulting raw
   vector to a character string.}
 \item{verbose}{a logical value allowing the caller to see details of
 the HTTP request and response.}
}
\value{

  \code{getFile} returns the contents of the object.
  The Content-Type will be available on the result as an attribute.
  If the content type is binary, the result is a raw vector.
  Otherwise, the result is a character vector of length 1.

  \code{s3Exists} returns \code{TRUE} or \code{FALSE}
  if the object can is on the S3 server.
  If permission to check the object is not available,
  the result is \code{NA}.
}
\references{
\url{http://docs.amazonwebservices.com/AmazonS3/latest/gsg/}  
}
\author{
Duncan Temple Lang <duncan@wald.ucdavis.edu>  
}

\seealso{
  \code{\link{listBucket}}
  \code{\link{removeFile}}
  \code{\link{about}}
  \code{\link{setS3Access}}  
}
\examples{
}
\keyword{IO }
