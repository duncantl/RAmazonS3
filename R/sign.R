S3AuthString =
  #
  # path is not needed if the caller specifies the resource.
  #
  #
function(id, op, bucket, path, header = character(), content = character(), secret,
         resource = sprintf("/%s%s%s", if(length(bucket)) bucket else "", if(length(path) && nchar(path)) "/" else "/", path),
         forceDate = FALSE,
         md5 = digest(content, "md5", serialize = FALSE))
{
  cur.date = Sys.time()

  if(length(header) == 0)
     header["Date"] = as.integer(as.POSIXct(Sys.time()))
 
  type = if(is.na(i <- match("Content-Type", names(header)))) "" else header[i]
  if(is.na(header["Date"]))
     header["Date"] = as.integer(as.POSIXct(Sys.time()))

  date =  header["Date"]

  if(forceDate)
     date = as.integer(cur.date)
 
 i = grep("^x-amz-", names(header))
 if(length(i)) {
    header[i] = gsub("/", "%2F", header[i])
      # need to sort these to be valid in the signature.
    o = order(names(header[i]))
    tmp = header[i][o]
    amz = paste(names(tmp), tmp, sep = ":")
  } else
    amz = character()


 content = if(length(content) && nchar(content) > 0)
               md5
           else
               ""

 str = c(op, content, type, date, amz, resource)
 
 str = paste(str, collapse = "\n")

 if(length(id) && !(is.na(id))) {
      # if the secret and id are a single named character vector
      # pull them apart.
   if(missing(secret)) {
      secret = id
      id = names(id)
   }

   
    # Sign the string
   val = signString(str, secret)
 
   auth = sprintf("AWS %s:%s", id, val)

   header["Authorization"] = auth
 }

 if(forceDate)
    header["Date"] = AWSDate(cur.date)
 
 header
}

signString =
function(str, key, base64 = TRUE, perl = TRUE)
{
  perl.exe = getOption("perl")
  if(is.null(perl.exe))
     perl.exe = "perl"
  
  if(is.character(perl)) {
    perl.exe = perl
    perl = TRUE
  }
    
  if(perl) {
    sign.pl = system.file("sign.pl", package = "RAmazonS3")
    cmd = sprintf("%s %s '%s' '%s'", perl.exe, sign.pl, key, str)
    ans = system(cmd, intern = TRUE)
    return(ans)
  }

  
  if(base64)
    base64 = " | openssl enc -base64"
  else
    base64 = ""
  cmd = sprintf("echo '%s' | openssl dgst -sha1 -hmac '%s' -binary %s",  # -binary/-hex
                 str, key, base64)

  system(cmd, intern = TRUE)
}

#XXXX
if(FALSE)
S3AuthString =
function(id, op, bucket, path, header, content, secret)  
{
  header["Authorization"] = sprintf("AWS %s:%s", names(id), id)
  header
}
