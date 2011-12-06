\name{Bucket-class}
\Rdversion{1.1}
\docType{class}
\alias{Bucket-class}
\alias{[[,Bucket,character,missing-method}
\alias{[[<-,Bucket,character,missing-method}
%\alias{$,Bucket,character-method}
\alias{$,Bucket-method}
\alias{$<-,Bucket,character-method}
\alias{length,Bucket-method}
\alias{names,Bucket-method}
\alias{Bucket}

\title{Abstract representation of an S3 bucket}
\description{
  This class provides a way to represent an S3 bucket
  and methods to operate on the bucket.
  This allows an R user to query the objects it contains,
  add and remove objects. 
}
\section{Objects from the Class}{
  One creates objects of this class via the
  \code{Bucket} function. This encapsulates
  the name of the bucket along with authorization information which
  is used to access the contents of the bucket.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"} the name of
      the bucket }
    \item{\code{auth}:}{Object of class \code{"character"} the login/key
    and AWS key used to access the bucket.}
    \item{\code{curl}:}{Object of class \code{"CURLHandle"} an optional
      curl handle that is used as the default handle to use to make the
      HTTP requests.
    }
  }
}
\section{Methods}{
  \describe{
    \item{[[}{\code{signature(x = "Bucket", i = "character", j =
	"missing")}: retrieve the contents of an object within the bucket}
    \item{[[<-}{\code{signature(x = "Bucket", i = "character", j =
	"missing")}: 
          store content in the bucket using the specified name}
    \item{$}{\code{signature(x = "Bucket", name = "character")}:
      retrieve the contents of an object within the bucket. }
    \item{$<-}{\code{signature(x = "Bucket", name = "character")}:
        store content in the bucket using the specified name}
    \item{length}{\code{signature(x = "Bucket")}: the number of objects
      in the bucket. }
    \item{names}{\code{signature(x = "Bucket")}: the names of the
      entries in the bucket}
	 }
}
\references{\url{http://docs.amazonwebservices.com/AmazonS3/latest/gsg/}  }
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}

\seealso{
}
\examples{
showClass("Bucket")
}
\keyword{classes}
