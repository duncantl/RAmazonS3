#source("R/ops.R"); source("R/sign.R"); source("R/utils.R"); source("R/bucket.R")
library(RAmazonS3)
if(!is.null(getOption("AmazonS3"))) {
  doc = listBucket("dtl-test", asXML = TRUE) # , auth = NA)

  library(XML)
  k = getNodeSet(doc, "//x:Contents", "x")
  xmlToDataFrame(doc, nodes = k)
}
