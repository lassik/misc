#! /usr/bin/env ruby

Bytes_per_row = 8

n = 0
STDIN.each_byte do |byte|
  printf("%02x", byte)
  n += 1
  n %= Bytes_per_row
  printf(if n == 0 then "\n" else " " end)
end
