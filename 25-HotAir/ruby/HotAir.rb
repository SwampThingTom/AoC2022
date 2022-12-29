#!/usr/bin/env ruby

# Full of Hot Air
# https://adventofcode.com/2022/day/25

def snafu_to_dec(snafu)
  from_snafu = Hash["=" => -2, "-" => -1, "0" => 0, "1" => 1, "2" => 2]
  snafu.chars.map { |ch| from_snafu[ch] }.reduce(0) { |result, digit| 5 * result + digit }
end

def dec_to_snafu(dec)
  to_snafu = Hash[-2 => "=", -1 => "-", 0 => "0", 1 => "1", 2 => "2"]
  snafu = ""
  while dec != 0
    digit = ((dec + 2) % 5) - 2
    snafu = to_snafu[digit] + snafu
    dec = (dec - digit) / 5
  end
  snafu
end

snafus = File.read("../input.txt").split
dec_sum = snafus.map { |snafu| snafu_to_dec(snafu) }.reduce(0) { |sum, value| sum + value }
snafu_sum = dec_to_snafu(dec_sum)
puts "Part 1: #{snafu_sum}"
