#! /usr/bin/env ruby

# See http://prawn.majesticseacreature.com/manual.pdf

require 'csv'
require 'date'

require 'prawn'
require 'prawn/table'

rows = CSV.new(open('table.csv')).read
Prawn::Document.new do
  font 'Helvetica'
  define_grid(columns: 5, rows: 8, gutter: 10)
  grid([0, 0], [1, 1]).bounding_box do
    text 'INVOICE', size: 18
    text 'Invoice No: 0001', align: :left
    text "Date: #{Date.today}", align: :left
    move_down 10
    text 'Attn: To whom it may concern '
    text 'Company Name'
    text 'Tel No: 1'
    text 'Fax No: 0`  1'
  end
  stroke_horizontal_rule
  move_down 10
  Prawn::Table.new(rows, self).draw
  render_file 'output.pdf'
end
