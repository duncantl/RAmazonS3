\name{addFile}
\Rdversion{1.1}
\alias{addFile}
\alias{copyFile}
\alias{renameFile}
\title{Create objects in S3 storage server}
\description{
 These functions
}
\usage{
addFile(contents, bucket, name = basename(contents), access = NA,
        type = if (is.raw(contents)) "binary/octet-stream" else "text/plain",
        meta = character(), encoding = NA,
        isContents = is.raw(contents) || is(contents, "AsIs"),
        auth = getOption("AmazonS3"), curl = getCurlHandle(),
        virtual = (tolower(bucket) == bucket))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{contents}{the name of a file or the actual contents.
    To store text directly from and R character string,
    wrap it as an AsIs object, e.g. \code{I("some text")}.
   One can also specify that this is the content, and not the name of a
   file, using \code{isContents = TRUE}.}
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
  \item{access}{a character string specifying the access for this new
    object. This should be one of
    \code{"private"}, \code{"public-read"}, \code{"public-read-write"},
    \code{"authenticated-read"}.
  }
  \item{type}{the content type of the object being stored. One should
    use common, recognized types such as image/png, image/jpg,
     text/html, application/json, ...}
  \item{meta}{a character vector of name - value pairs which specify
  meta-information about the object being stored. }
  \item{encoding}{if specified, the character encoding of the file.}
  \item{isContents}{a logical value indicating whether the value of
    \code{contents} should be treated as the actual contents to store
    or the name of a file which contains the contents to be stored.}
}
\value{
}
\references{
\url{http://docs.amazonwebservices.com/AmazonS3/latest/gsg/}  
}
\author{
Duncan Temple Lang <duncan@wald.ucdavis.edu>  
}

\seealso{
  \code{\link{getFile}}
  \code{\link{listBucket}}  
}
\examples{
}
\keyword{IO}
