#! /usr/bin/env ruby

# Converts the full list of IETF RFC documents into a CSV file.

require "csv"
require "open-uri"

HTML_URL  = "https://tools.ietf.org/rfc/index"
HTML_FILE = "rfc.html"
CSV_FILE  = "rfc.csv"

if not File.exist?(HTML_FILE)
  open(HTML_URL) do |html|
    IO.copy_stream(html, HTML_FILE)
  end
end

open(HTML_FILE) do |html|
  CSV.open(CSV_FILE, "wb") do |csv|
    html.each_line do |line|
      if line =~ /<a href="http:..tools.ietf.org.html.\d+">(RFC\d+)<.a>\s+(.*)/
        rfc_number = $1
        rfc_title = $2.gsub(/<.*?>/, "").sub(/\..*/, "")
        csv << [rfc_number, rfc_title]
      end
    end
  end
end
