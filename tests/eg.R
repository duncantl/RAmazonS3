# Set the option(AmazonS3 = c(id = secret))
library(RAmazonS3)

if(FALSE) {
o = listBucket("RRupload")
a = getFile("ops.R", "RRupload")


addFile("bar", "RRupload", I("This is raw text"))


addFile("compressed", "RRupload", compress("This is raw text"))
getFile("compressed", "RRupload")
}
