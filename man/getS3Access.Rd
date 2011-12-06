\name{getS3Access}
\Rdversion{1.1}
\alias{getS3Access}
\alias{setS3Access}
\alias{setACL}
\title{Query and set the access to a bucket or an object}
\description{
  These functions provide facilities to query or set
  who can access an S3 object or bucket.
  One can find out who is entitled to read/write the object
  or specify either general access properties (e.g.
  only the owner, everyone can read, everyone can read and write
  or authenticated reading).
  Alternatively, one can specify more specific access
  by detailing the users or groups that can access the object in what manner.
}
\usage{
getS3Access(bucket, name, auth = getOption("AmazonS3"), curl = getCurlHandle(),
            virtual = (tolower(bucket) == bucket), asXML = FALSE)
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
  \item{asXML}{a logical value that allows the caller to indicate that
    they want the parsed XML document containing the response and not
    to bother converting it to a data frame. This allows the caller to
    extract the information in the way she wants it.}
}
\value{
  A data frame with ...
}
\references{
\url{http://docs.amazonwebservices.com/AmazonS3/latest/gsg/}  
}
\author{
Duncan Temple Lang <duncan@wald.ucdavis.edu>  
}
\note{
}

\seealso{
  \code{\link{setS3Access}}
  The \code{setACL} function is not currently working correctly
  but will allow one to specify access control at the level
  of individual groups.
}
\examples{
}
\keyword{IO}
