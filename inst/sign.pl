#!/usr/bin/perl
use Digest::HMAC_SHA1;
use MIME::Base64 qw(encode_base64);

$key = $ARGV[0];
$data = $ARGV[1];

$hmac = Digest::HMAC_SHA1->new($key);
$hmac->add($data);
my $b64 = encode_base64($hmac->digest, '');

print $b64, "\n";
