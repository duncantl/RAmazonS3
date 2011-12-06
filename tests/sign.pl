use Digest::SHA::PurePerl qw(hmac_sha1);
use MIME::Base64;

$key = 'uV3F3YluFJax1cknvbcGwgjvx4QpvB+leU8dUj2o';
$data = 'GET\n\n\nTue, 27 Mar 2007 19:36:42 +0000\n/johnsmith/photos/puppy.jpg';

$digest = hmac_sha1($data, $key);

print encode_base64($digest);


