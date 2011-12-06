\name{listBucket}
\Rdversion{1.1}
\alias{listBucket}
\alias{listBuckets}
\alias{getBucketLocation}
\title{Get bucket names and information}
\description{
  These functions allow an R user to list
  all of the buckets for a paticular S3 login/user
  and to list the contents (the names of the objects)
  within a bucket.  We can also find out meta-information
  about a bucket, currently just where it is located (e.g. US or Europe).
}
\usage{
listBuckets(withDates = TRUE, auth = getOption("AmazonS3"), host = "http://s3.amazonaws.com")
listBucket(name, auth = getOption("AmazonS3"), host = "s3.amazonaws.com",
            curl = getCurlHandle(), asXML = FALSE, virtual = (tolower(name) == name))
getBucketLocation(bucket, auth = getOption("AmazonS3"), curl = getCurlHandle(),
                    virtual = (tolower(bucket) == bucket))
}
\arguments{
  \item{withDates}{a logical value that controls whether the creation
    dates of the buckets being listed are returned or just the names}

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
  \item{host}{a string identifying the basic host.}
  \item{asXML}{a logical value that allows the caller to indicate that
    they want the parsed XML document containing the response and not
    to bother converting it to a data frame. This allows the caller to
    extract the information in the way she wants it.}
}
\details{
}
\value{
  \code{listBuckets:}
  If \code{withDates} is \code{TRUE}, a data frame with as many rows
  as there are buckets for that S3 user and with 2 columns: the names of
  the buckets and the creationDate
}
\references{
\url{http://docs.amazonwebservices.com/AmazonS3/latest/gsg/}
}
\author{
Duncan Temple Lang <duncan@wald.ucdavis.edu>
}
\seealso{
  \code{\link{listBuckets}}
  \code{\link{addFile}}
  \code{\link{removeFile}}
  \code{\link{about}}    
}
\examples{
}
\keyword{IO}
