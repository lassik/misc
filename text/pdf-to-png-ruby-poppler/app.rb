require 'open3'

pdf_file = ARGV[0]
png_file = File.basename(pdf_file, '.*') + '.png'
png_data, err_msg, status = Open3.capture3(
  'pdftoppm', '-png', pdf_file
)
$stderr.puts(err_msg)
$stderr.puts("Exit status #{status.exitstatus}")
open(png_file, 'w').write(png_data)
