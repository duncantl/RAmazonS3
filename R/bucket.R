
setClass("Bucket",
            representation(name = "character",
                           auth = "character",
                           curl = "CURLHandle"))

Bucket =
function(name, auth = getOption("AmazonS3"), curl = NULL, obj = new("Bucket"))
{
  if(!is.null(curl))
    obj@curl = curl

  obj@name = as(name, "character")
  obj@auth = as(auth, "character")

  obj
}

if(FALSE) {
setGeneric("listFiles",
            function(what, ...) {
              standardGeneric("listFiles")
            })

setMethod("listFiles", "Bucket",
            function(x, what, ...) {
               listFiles(what@name, what, auth = x@auth...)
            })
}

setMethod("names", "Bucket",
            function(x) {
               # as.character(listBucket(x@name, auth = x@auth)[,"Key"])
               doc = listBucket(x@name, auth = x@auth, asXML = TRUE)
               xpathSApply(doc, "//x:Contents", function(x) xmlValue(x[["Key"]]), namespaces = "x")               
            })

setMethod("length", "Bucket",
            function(x) {
               doc = listBucket(x@name, auth = x@auth, asXML = TRUE)
               length(getNodeSet(doc, "//x:Contents", "x"))
            })

setMethod("[[", c("Bucket", "character", "missing"),
            function(x, i, j, binary = NA, ...) {
               getFile(x@name, i, auth = x@auth, binary = binary, ...)
            })

setMethod("$", c("Bucket"), #"character"),
            function(x, name) {
               getFile(x@name, name, auth = x@auth)
            })


setMethod("[[<-", c("Bucket", "character", "missing"),
            function(x, i, j, type = NA, encoding = NA, ..., value) {
               addFile(value, x@name, i, auth = x@auth, ...)
               x
            })

setMethod("$<-", c("Bucket", "character"),
            function(x, name, value) {
               addFile(value, x@name, name, auth = x@auth)
               x
            })
