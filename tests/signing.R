# http://docs.amazonwebservices.com/AmazonS3/2006-03-01/index.html?RESTAPI.html

if(FALSE) {
key = c('0PN5J17HBGZHT7JJ3X82' = 'uV3F3YluFJax1cknvbcGwgjvx4QpvB+leU8dUj2o')
  # md5 sum of ""
txt = 'GET\nd41d8cd98f00b204e9800998ecf8427e\n\nTue, 27 Mar 2007 19:36:42 +0000\n/johnsmith/photos/puppy.jpg'
txt = 'GET\n\n\nTue, 27 Mar 2007 19:36:42 +0000\n/johnsmith/photos/puppy.jpg'

signString(txt, key)

# Should get
#   xXjDGYUmKxnwqr5KXNPGldn5LbA=

txt = 'GET\n\n\nWed, 28 Mar 2007 01:49:49 +0000\n/dictionary/fran%C3%A7ais/pr%c3%a9f%c3%a8re'
signString(txt, key)
}
