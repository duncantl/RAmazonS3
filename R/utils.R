AWSDate =
function(when = Sys.time())
{
  format(when, "%a, %d %b %Y %H:%M:%S %Z")
}

vhost =
function(bucket)
 sprintf("%s.s3.amazonaws.com", bucket)


userAgent =
function(version = TRUE)
{
   if(is.logical(version))
     version = if(version)
                  R.version.string
               else
                  ""

   sprintf("RAmazonS3, %s", version)
}


readBinary =                                                                                                                            
function(filename, len = NA)                                                                                                            
{                                                                                                                                       
  if(is.na(len))                                                                                                                        
     len = file.info(filename)[1, "size"]                                                                                               
                                                                                                                                        
  readBin(filename, "raw", len)                                                                                                         
}                                                                                                                                       


md5 =
function(what, isFile = !inherits(what, "AsIs"))
{
  if(isFile) {
     if(md5sum.exe == "")
       stop("No program available at installation time to compute md5 sum of a file")
     
     ans = system(paste(md5sum.exe, path.expand(what)), intern = TRUE)
     strsplit(ans, " ")[[1]][1]
  } else
     digest(what, "md5", serialize = TRUE)
}
