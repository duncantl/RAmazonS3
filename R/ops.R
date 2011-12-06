# Bucket[["foo"]]
#  is http://bucketName.s3.amazonaws.com/foo
# Content-Type and other meta data
#

# Add
#  Expect: 100-continue
# to the header to use 100- continuations in HTTP and avoid cost of sending
# things twice. 

###########################################################################



listS3Files = listBucket = 
function(name, auth = getOption("AmazonS3"), host = "s3.amazonaws.com",
           curl = getCurlHandle(), asXML = FALSE, virtual = (tolower(name) == name))
{
  u = if(virtual)
         sprintf("http://%s.s3.amazonaws.com", name)
      else
         sprintf("http://s3.amazonaws.com/%s", name)  

  h = c(Date = AWSDate())
  if(virtual)
    h["Host"] = vhost(name)
  header = S3AuthString(auth, "GET", name, "", h, "")
  

  curlSetOpt(.opts = list(httpheader = header, verbose = FALSE), curl = curl, .forceHeaderNames = TRUE)
  ans = getURLContent(u, curl = curl)

  doc = xmlParse(ans, asText = TRUE)
  if(asXML)
     return(doc)

    #XXX Need to deal with the meta info.
    #  Name, Prefix, Marker, MaxKeys, IsTruncated
  parseContentsInfo(doc)
}

addFile =
  #
  # allow determining the type
  # base64 encoding
  # Use readfunction to read from a file.
  #
  # Add another function for uploading R objects by saving them.
  #
function(contents, bucket, name = basename(contents), access = NA,
         type = if(is.raw(contents)) "binary/octet-stream" else "text/plain",
         meta = character(),
         encoding = NA, isContents = is.raw(contents) || is(contents, "AsIs"),
         auth = getOption("AmazonS3"), curl = getCurlHandle(), virtual = (tolower(bucket) == bucket))
{

  if(missing(name)) {
    if(length(grep("/", bucket)) > 0 && length(grep("/$", bucket)) == 0) {
       tmp = strsplit(bucket, "/")[[1]]
       name = paste(tmp[-1], collapse = "/")
       bucket = tmp[1]
    }
  }        


  u = if(virtual)
         sprintf("http://%s.s3.amazonaws.com/%s", bucket, name)
      else
         sprintf("http://s3.amazonaws.com/%s/%s", bucket, name)  

  if(!isContents) {
     # determine the type if not specified.
     if(missing(type)) 
       type = "application/binary"

     if(FALSE)
      contents = if(substring(type, 1, 5) == "text/")
                   readLines(contents)
                else
                   readBinary(contents)
   } else if(is.character(contents))
       contents = paste(contents, collapse = "\n")


  m = missing(curl)

  fun = basicTextGatherer()
  h = c(Date = AWSDate(), "User-Agent" = userAgent())

  if(length(meta))
    h = c(h, structure(meta, names = paste('x-amz-meta', names(meta), sep = "-")))

  
  if(!is.na(type))
     h["Content-Type"] = type
  if(!is.na(encoding))
     h["Content-Encoding"] = encoding
  if(virtual)
     h["Host"] = sprintf("%s.s3.amazonaws.com", bucket)

  if(!isContents) {
     h["Transfer-Encoding"] = ""
     h["Content-Length"] = file.info(contents)[1, "size"]
  }
  
  if(!is.na(access))
     h['x-amz-acl'] =  match.arg(access, ACL)


  header = S3AuthString(auth, "PUT", bucket, name, h, md5 = md5(contents, isFile = !isContents))

   # We would like to be able to arrange for the file to be read as it is uploaded
   # by libcurl rather than having us load it into memory and then pass it in one
   # big blob.  But we have to compute the md5sum of the contents for the purposes
   # of the signature. We do this with md5
  if(!isContents)
    .opts = list(upload = TRUE, readfunction = RCurl:::uploadFunctionHandler(contents, isTextType(type)))
  else
    .opts = list()
  
  curlPerform(url = u,
              customrequest = "PUT",
              httpheader = header,
              postfields = contents,
              httppost = TRUE, # HTTPPOST
              verbose = FALSE,
              headerfunction = fun$update, .opts = .opts)

    # Parse this header
  reply = parseHTTPHeader( fun$value() )
  as.integer(reply[["status"]]) %/% 100 == 2
}

isTextType =
function(type)
  substring(type, 1, 5) == "text/"
 
about = getInfo = 
function(bucket, name = character(), auth = getOption("AmazonS3"),
         curl = getCurlHandle(), virtual = (tolower(bucket) == bucket),
         asHeader = FALSE)
{
  if(missing(name)) {
    tmp = strsplit(bucket, "/")[[1]]
    name = paste(tmp[-1], collapse = "/")
    bucket = tmp[1]
  }

  u = if(virtual)
         sprintf("http://%s.s3.amazonaws.com/%s", bucket, name)
      else
         sprintf("http://s3.amazonaws.com/%s/%s", bucket, name)

  h = c(Date = AWSDate(), "User-Agent" = userAgent())
  if(virtual)
     h["Host"] = vhost(bucket)  

  h = S3AuthString(auth, "HEAD", bucket, name, h)

  header = basicTextGatherer()
  ans = curlPerform(url = u, customrequest = "HEAD", httpheader = h, verbose = FALSE,
                    nobody = TRUE,
                    headerfunction = header$update,
                    curl = curl)


  hh = parseHTTPHeader(header$value())
  if(asHeader)
    return(hh)
  
  i = match(c("x-amz-id-2", "x-amz-request-id", "Date", "status", "statusMessage"), names(hh))
  hh = hh[ - i]
  names(hh) = gsub("^x-amz-meta-", "", names(hh))
  hh
}


getFile =
function(bucket, name, auth = getOption("AmazonS3"), 
         curl = getCurlHandle(), binary = NA, virtual = (tolower(bucket) == bucket),
         verbose = FALSE)
{

  if(missing(name)) {
    tmp = strsplit(bucket, "/")[[1]]
    name = paste(tmp[-1], collapse = "/")
    bucket = tmp[1]
  }      
  
 u = if(virtual)
         sprintf("http://%s.s3.amazonaws.com/%s", bucket, name)
     else
         sprintf("http://s3.amazonaws.com/%s/%s", bucket, name)
 h = c(Date = AWSDate(), "User-Agent" = userAgent())
 if(virtual)
    h["Host"] = vhost(bucket)
 
 h = S3AuthString(auth, "GET", bucket, name, h)
 getURLContent(u, .opts = list(httpheader = h, verbose = verbose), curl = curl, binary = binary)
}


copyFile =
  #
  #  copyFile('www.penguin/temp1', 'reproducibleresearch/temp9')   
  #  copyFile('www.penguin/temp1', 'temp9') 
  #
  #
function(from, to, auth = getOption("AmazonS3"), 
           curl = getCurlHandle(), virtual = TRUE) #XXX lower case issues.
{
  a = strsplit(from, "/")[[1]]
  tmp = strsplit(to, "/")[[1]]
  bucket = a[1]

 if(length(tmp) == 1) {
   if(length(grep("/$", to))) {
      tmp = c(gsub("/$", "", to),  a[2])
      to = gsub("//+", "/", paste(tmp, collapse = "/"))
   } else {
      tmp =  c(bucket, to)  # sprintf("/%s/%s", bucket, to)
      to = sprintf("%s/%s", bucket, to)
   }
 }

 u = if(virtual)
        sprintf("http://%s.s3.amazonaws.com/%s", tmp[1], tmp[2])
     else
        sprintf("http://s3.amazonaws.com/%s", paste(to, collapse = "/"))

 xfrom = sprintf("/%s", from)
 h = c(Date = AWSDate(), "User-Agent" = userAgent(), 'x-amz-copy-source' = xfrom)
 if(virtual)
    h["Host"] = sprintf("%s.s3.amazonaws.com", tmp[1])
 h = S3AuthString(auth, "PUT", bucket, to, h, resource = sprintf("/%s", to))


 reader = basicTextGatherer()
 body = basicTextGatherer()

 curlPerform(url = u, httpheader = h, customrequest = "PUT", curl = curl, verbose = FALSE,
              writefunction = body$update, headerfunction = reader$update)

 ans = parseHTTPHeader(reader$value())
 if(as.integer(ans[["status"]]) %/% 100 == 2)
     return(TRUE)
 
 top = xmlRoot(xmlParse(body$value(), asText = TRUE))
 stop(xmlValue(top[["Code"]]), ": ", xmlValue(top[["Message"]]), ",\n\n header string signed: ", xmlValue(top[["StringToSign"]]))
}


getBucketLocation =
function(bucket, auth = getOption("AmazonS3"), 
           curl = getCurlHandle(), virtual = (tolower(bucket) == bucket))
{
   u = if(virtual)
          sprintf("http://%s.s3.amazonaws.com/?location", bucket)
       else
          sprintf("http://s3.amazonaws.com/%s?location", bucket)
   h = c(Date = AWSDate(), "User-Agent" = userAgent())
   if(virtual)
      h["Host"] = vhost(bucket) 
   h = S3AuthString(auth, "GET", bucket, "?location", h)
   ans = getURL(u, .opts = list(httpheader = h))

   doc = xmlParse(ans, asText = TRUE)
   top = xmlRoot(doc)
   if(xmlSize(top)) xmlValue(top) else as.character(NA)
}

ACL = c("private", "public-read", "public-read-write", "authenticated-read")

setS3Access = 
function(bucket, name, to, auth = getOption("AmazonS3"), 
           curl = getCurlHandle(), virtual = (tolower(bucket) == bucket))
{
  if(missing(name)) {
    tmp = strsplit(bucket, "/")[[1]]
    name = paste(tmp[-1], collapse = "/")
    bucket = tmp[1]
  }    

   u = if(virtual)
          sprintf("http://%s.s3.amazonaws.com/%s", bucket, name)
       else
          sprintf("http://s3.amazonaws.com/%s/%s", bucket, name)  

   to = match.arg(to, ACL)
   h = c(Date = AWSDate(), 'x-amz-acl' = to, 'User-Agent' = userAgent(), Except = "", 'Content-Length' = 0, 'Transfer-Encoding' = "")
   if(virtual)
      h["Host"] = sprintf("%s.s3.amazonaws.com", bucket)
   h = S3AuthString(auth, "PUT", bucket, name, h)
   reader = basicTextGatherer()   
   ans = curlPerform(url = u, httpheader = h, verbose = FALSE,
                     put = TRUE,
                     header = reader$update,
                     curl = curl)
   parseHTTPHeader(ans)
}

getS3Access =
function(bucket, name, auth = getOption("AmazonS3"), 
         curl = getCurlHandle(), virtual = (tolower(bucket) == bucket),
         asXML = FALSE)
{
  if(missing(name)) {
    tmp = strsplit(bucket, "/")[[1]]
    name = paste(tmp[-1], collapse = "/")
    bucket = tmp[1]
  }    
  
   u = if(virtual)
         sprintf("http://%s.s3.amazonaws.com/%s?acl", bucket, name)  
       else
         sprintf("http://s3.amazonaws.com/%s/%s?acl", bucket, name)

   h = c(Date = AWSDate(), "User-Agent" = userAgent())
   if(virtual)
     h["Host"] =  sprintf("%s.s3.amazonaws.com", bucket)
   h = S3AuthString(auth, "GET", bucket, sprintf("%s?acl", name), h)
   ans = getURL(u, .opts = list(httpheader = h, verbose = FALSE), curl = curl)
   doc = xmlParse(ans, asText = TRUE)
   if(xmlName(xmlRoot(doc)) == "Error")
     stop(paste(sapply(xmlRoot(doc)[1:3], xmlValue), collapse = " "))

   if(asXML)
     doc
   else
     processAccessList(doc)
}

processAccessList =
function(doc)
{
       # Assumes we are looking at an XML document
       # of the form
  grants = getNodeSet(doc, "//x:AccessControlList/x:Grant", "x")
  ans = xmlToDataFrame(nodes = lapply(grants, function(x) x[[1]]))
  ans$permission = factor(sapply(grants, function(x) xmlValue(x[["Permission"]]))) #XXX put the levels here since we now them.
  ans
}

removeBucket =
function(bucket, auth = getOption("AmazonS3"), 
           curl = getCurlHandle(), virtual = (tolower(bucket) == bucket))
{
  m = missing(curl)

if(virtual) {  
  host = sprintf("%s.s3.amazonaws.com", bucket)
} else {
  host = sprintf("s3.amazonaws.com", bucket)
}
  
  h = c(Date = AWSDate(), Host = host, "User-Agent" = userAgent())
  header = S3AuthString(auth, "DELETE", bucket, "", h, "")


  fun = basicTextGatherer()
  status = curlPerform(customrequest = "DELETE",
                       url = paste(host, "/", sep = ""),
                       httpheader = header,
                       verbose = FALSE,
                       headerfunction = fun$update,
                       curl = curl)
  reply = parseHTTPHeader( fun$value() )
  as.integer(reply[["status"]]) %/% 100 == 2
}

removeFile =
function(bucket, name, auth = getOption("AmazonS3"), 
           curl = getCurlHandle(), virtual = (tolower(bucket) == bucket))
{

    if(missing(name)) {
      tmp = strsplit(bucket, "/")[[1]]
      name = paste(tmp[-1], collapse = "/")
      bucket = tmp[1]
    }      

    if(virtual) {  
      host = sprintf("%s.s3.amazonaws.com", bucket)
      u = sprintf("http://%s.s3.amazonaws.com/%s", bucket, name)
    } else {
      host = sprintf("s3.amazonaws.com", bucket)
      u = sprintf("http://s3.amazonaws.com/%s/%s", bucket, name)
    }

    m = missing(curl)

    h = c(Date = AWSDate(), Host = host, "User-Agent" = userAgent())
    header = S3AuthString(auth, "DELETE", bucket, name, h, "")

   reader = basicTextGatherer()
    status = curlPerform(customrequest = "DELETE",
                         url = u,
                         verbose = FALSE,
                         httpheader = header,
                         headerfunction = reader$update,
                         curl = curl)
  #XX Reset the request to PUT.
  if(!m) {}

    # parse the HTTP response header.
  hdr = parseHTTPHeader(reader$value())
  as.integer(hdr["status"]) %/% 100 == 2
}


makeBucket =
function(name, location = NA, auth = getOption("AmazonS3"), 
           curl = getCurlHandle(),
          virtual = TRUE)  # has a http whereas for listBucket, it doesn't.
{
     if(virtual) {
        u = sprintf("http://%s.s3.amazonaws.com", name)
        vhost = sprintf("%s.s3.amazonaws.com", name)
     } else {
        u = sprintf("http://s3.amazonaws.com/%s", name)
        vhost = "s3.amazonaws.com"
     }

     h = c("User-Agent" = userAgent(), Date = AWSDate(), 'Content-Length' = 0L, Expect = "", 'Transfer-Encoding' = '')
     h = S3AuthString(auth, "PUT", name, "", h)
     reader = basicTextGatherer()

     ans = curlPerform(.opts = list(put = TRUE,
                                    url = u,
#                                   postfieldsize = 0L, # important! Otherwise has Expect: 100...
                                    postfields = "bob",
                                    httpheader = h,
                                    headerfunction = reader$update, 
                                    verbose = FALSE), curl = curl)
    ans = parseHTTPHeader(reader$value())
    as.integer(ans[["status"]]) %/% 100 == 2
}


listBuckets =
  # Okay.
function(withDates = TRUE, auth = getOption("AmazonS3"), host = "http://s3.amazonaws.com")
{
  h = c(Date = AWSDate())
  header = S3AuthString(auth, "GET", character(), "", h, resource = "/")

  ans = getURL(host, .opts = list(httpheader = header, followlocation = TRUE, verbose = FALSE))
  doc = xmlParse(ans, asText = TRUE)
  top = xmlRoot(doc)
  ids = xmlSApply(top[["Buckets"]], function(x) xmlValue(x[[1]]))
  ans = if(withDates) {
    dates = xmlSApply(top[["Buckets"]], function(x) xmlValue(x[[2]]))
    data.frame(bucket = ids, creationDate =  as.POSIXct(strptime(dates, "%Y-%m-%dT%H:%M:%S.000Z")))
  } else
    ids

    # Tag the name of the user to the result for clarity/prvenance, just in
    # case the the caller has access to several accounts and wants to find which buckets are
    # in which.
  attr(ans, "user") = xmlValue(top[["Owner"]][["DisplayName"]])
  ans
}

parseContentsInfo =
  #
  # Take the Contents from a bucket listing and
  # turn them into a data frame.
  # Like file.info.

  #
  # XXX I have seen one row come back that didn't have an ID
  #  and so the do.call("rbind", ...) fails.
  #
  #
function(doc, contents = getNodeSet(doc, "//x:Contents", "x"))
{
  if(length(contents) == 0)
    return(NULL) # or a data frame
  
  info = do.call("rbind", lapply(contents, function(x) as.data.frame(xmlToList(x))))

  info$LastModified = as.POSIXct(strptime(info$LastModified, "%Y-%m-%dT%H:%M:%S.000Z"))
  info$ETag = gsub('"', '', (as.character(info$ETag)))
  info$Size = as.integer(as.character(info$Size))
  info
}


s3Exists =
function(bucket, name, auth = getOption("AmazonS3"),
          curl = getCurlHandle(),
          virtual = (tolower(bucket) == bucket))
{
     if(missing(name)) {
        tmp = strsplit(bucket, "/")[[1]]
        name = paste(tmp[-1], collapse = "/")
        bucket = tmp[1]
     }        

     ans = getInfo(bucket, name, auth, curl, virtual, asHeader = TRUE)
     st = as.integer(ans[["status"]])
     if(st == 404)
       FALSE
     else if(st %/% 100 == 4)
       NA
     else if(st %/% 100 == 2)
       TRUE
     else
       FALSE
}



renameFile = s3Rename =
function(from, to, auth = getOption("AmazonS3"), curl = getCurlHandle()) #virtual?
{
   if(copyFile(from, to, auth = auth, curl = curl)) {
      removeFile(from, auth = auth, curl = curl)
      TRUE
    } else
      FALSE  # should have an error from copyFile().
}

#
if(FALSE) { 
  fun = basicTextGatherer()
  fun1 = basicTextGatherer()
  u = "http://s3.amazonaws.com/RRupload"
  curlPerform(url = u, httpheader = c("Date" = as.character(header["Date"]),
                                      "Authorization" = as.character(header["Authorization"])),
                   verbose = FALSE, headerfunction = fun$update)
  return(1)
x = getURL(u, .opts = list(httpheader = header, verbose = TRUE),
                       curl = curl)
return(1)  
}  
