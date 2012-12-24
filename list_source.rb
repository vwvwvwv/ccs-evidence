#!/usr/bin/env ruby

MAXDIGITS = 5
LEFTCOLUMN = 3
i = 0
ARGF.each do |line|
  puts i.to_s.rjust(MAXDIGITS) + " " * LEFTCOLUMN + line
  i += 1
end
