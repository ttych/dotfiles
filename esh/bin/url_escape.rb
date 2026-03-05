#!/usr/bin/env ruby

require 'erb'

ARGV.each do |arg|
  puts ERB::Util.url_encode(arg)
end
