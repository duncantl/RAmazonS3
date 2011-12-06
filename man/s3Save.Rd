\name{s3Save}
\Rdversion{1.1}
\alias{s3Save}
\alias{s3Load}
\title{Store and retrieve R objects on S3}
\description{
  These functions serialize and de-serialize R objects, storing the
  results on the S3 server.
  They are equivalent to the \code{\link[base]{save}}
  and \code{\link[base]{load}} functions,
  differing only in that the resulting content is stored
  and retrieved from the S3 
}
\usage{
s3Save(..., list = character(), file = stop("'file' must be specified"),
        ascii = FALSE, version = NULL, envir = parent.frame(),
         compress = !ascii, eval.promises = TRUE, precheck = TRUE,
          auth = getOption("AmazonS3"), access = NA, useFiles = FALSE)
}
\arguments{
  \item{\dots}{the R objects to serialize}
  \item{list}{a character vector listing the names of the R objects to serialize}
  \item{file}{the "bucket/object" identifier giving the location on S3
    to store the object. }
  \item{ascii,version,envir,compress,eval.promises,precheck}{arguments
    passed directly to \code{\link[base]{save}}.}
  \item{access}{a character vector. This is passed to
    \code{\link{addFile}} and controls who can access the file.
  }
  \item{auth}{a named character vector giving the AWS key and secret as a \code{c(key = secret)}.}
  \item{useFiles}{a logical value indicating whether to explicitly save
    the R objects to a file and then upload that file.
    If this is \code{FALSE}, the objects are serialized in memory and
    the resulting raw vector stored.
    The results are the same regardless of whether we use files or not.
    There may be benefits to writing to a file if the objects are very,
    very large as serializing them in memory will essentially give us
    two copies in memory at the same time. Uploading from a file
    can avoid this.
  }
}
\value{
  \code{s3Save} returns a logical value indicating whether the
  operation was successful.
  \code{s3Load} invisibly returns the names of the objects
  that were de-serialized.

  In the future, we might return the name of the object
  from \code{s3Save} so that it can be used by the caller.
  If the operation was unsuccessful, we'll reaise an error.
}
\references{
\url{http://docs.amazonwebservices.com/AmazonS3/latest/gsg/}  
}
\author{
Duncan Temple Lang <duncan@wald.ucdavis.edu>  
}

\seealso{
  \code{\link{addFile}}
}
\examples{
}
\keyword{IO}
