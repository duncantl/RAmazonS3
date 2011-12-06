setACL =
  #
  #
 
  # Create the XML AccessControlPolicy
  # granting access to the id, name, permission triples.
  #
  # setACL("dtl-share/hello", ,"4e10a30dc41e8b3b6c7bcebe32720f27b4a79454e99155590730897b3a3f0033", "duncan", "FULL_CONTROL")
  #
  #
  
function(bucket, name, id, user, permission, append = TRUE,
          auth = getOption("AmazonS3"), 
          virtual = (tolower(bucket) == bucket),
          curl = getCurlHandle())
{
   if(missing(name)) {
      tmp = strsplit(bucket, "/")[[1]]
      name = paste(tmp[-1], collapse = "/")
      bucket = tmp[1]
   }

   doc = getS3Access(bucket, asXML = TRUE, curl = curl)

   if(missing(user))
      user = names(id)

   doc = makeACLGrants(doc, id, user, permission, append)

   txt = saveXML(doc)

   h = c("User-Agent" = userAgent(), Date = AWSDate(), 'Content-Type' = "application/xml", Accept = "")   
   if(virtual) {
      h["Host"] = sprintf("%s.s3.amazonaws.com", bucket)
      u = sprintf("http://%s.s3.amazonaws.com/%s?acl", bucket, name)
   } else {
      u = sprintf("http://s3.amazonaws.com/%s/%s?acl", bucket, name)          
   }

   h = S3AuthString(auth, "PUT", bucket, , h, txt, resource = paste("/", bucket, "/", name, "?acl",sep = ""))
   reader = basicTextGatherer()
browser()
   curlPerform(#curl = curl,  # if we reuse the curl handle which we used in the getS3Access() above, we get crashes.
                              # is it the httpheader?
               url = u,
               postfields = txt,
               httpheader = h,
               customrequest = "PUT",
               httppost = TRUE,
               verbose = TRUE,
               headerfunction = reader$update)

   ans = parseHTTPHeader(reader$value())
   ans
}

makeACLGrants =
function(doc, id, name, permission, append = TRUE)
{
#   doc = newXMLNode("AccessControlPolicy", namespaceDefintions = c("" = 'http://s3.amazonaws.com/doc/2006-03-01/'))
#   who(owner, names(owner), parent = doc)
#   al = newXMLNode("AccessControlList", parent = doc)
   al = getNodeSet(doc, "//x:AccessControlList", "x")[[1]]
   if(!append)
     removeChildren(al, kids = xmlChildren(al))
   mapply(function(id, name, permission)
            grant(id, name, permission, al),
          id, name, permission)
   doc
}

who =
function(id, name, nodeName = "Owner", parent = NULL)
{  
  n = newXMLNode(nodeName,
                   newXMLNode("ID", id),
                   newXMLNode("DisplyName", name),
                 parent = parent)
 # if(nodeName == "Grantee")
 #     xmlAttrs(n) = c('xsi:type' = "CanonicalUser")
  n
}

grant =
function(id, name, permission, parent = NULL)
{
  g = newXMLNode("Grant", parent = parent)
  who(id, name, "Grantee", parent = g)
  newXMLNode("Permission", permission, parent = g)
  g
}



