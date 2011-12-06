s3Save =
function(..., list = character(), file = stop("'file' must be specified"),
         ascii = FALSE, version = NULL, envir = parent.frame(),  compress = !ascii,
          eval.promises = TRUE, precheck = TRUE, auth = getOption("AmazonS3"),
          access = NA, useFiles = FALSE)
{
   # Change this to a rawConnection()
   if(useFiles)
     ff = con = tempfile()
   else {
     con = rawConnection(raw(0), "wb")
     on.exit(close(con))
   }
   
   save(..., list = list, file = con, ascii = ascii, version = version, envir = envir,
         compress = compress, eval.promises = eval.promises, precheck = precheck)

   if(!useFiles) 
      ff = rawConnectionValue(con)

   addFile(ff, file, isContents = !useFiles, access = access, type = if(ascii) "text/plain" else "application/x-rda",
            auth = auth)

   ff
}

# We can then retrieve this and load() it.
# Need to sort out writing to disk and reloading it
# or loading it directly from a raw vector.
# When writing to disk, discard the attributes.
# But if we have a binary

s3Load =
function(bucket, name, envir = parent.frame(), auth = getOption("AmazonS3"),
          curl = getCurlHandle(), virtual = (tolower(bucket) == bucket))
{
     if(missing(name)) {
        tmp = strsplit(bucket, "/")[[1]]
        name = paste(tmp[-1], collapse = "/")
        bucket = tmp[1]
     }

     data = getFile(bucket, name, auth = auth, virtual = virtual, curl = curl)
      # Want to be able to pass this to load() directly
      #  load(rawConnection(data, "rb"))
      # But it doesn't work just like that. Later....

     attributes(data) = NULL # kill the attributes so writeBin() won't complain.
     f = tempfile()
     writeBin(data, f)
     load(f, envir)
}
