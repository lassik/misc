#! /usr/bin/env ruby

require 'csv'
require 'date'
require 'open3'

require 'mustache'

rows = CSV.new(open('table.csv')).read

view = Mustache.new
view.template_file = 'template.mustache'
view[:rows] = rows
html = view.render

_, err_msg, status = Open3.capture3('wkhtmltopdf', '-', 'output.pdf',
                                    stdin_data: html)
$stderr.puts(err_msg)
$stderr.puts("Exit status #{status.exitstatus}")
