#! /bin/path ruby

# Easily create a proper, dated ZIP archive of a directory.
#
# References: rubyzip-0.9.1 source code, pkzip appnote.txt

require("optparse")
require("zlib")

class Array
  def has?(&block)
    self.each do |x| return true if block.call(x) end; false
  end
end

$datep = nil
$timep = nil
$stem = nil
$stuffix = nil
$excludes = []
$arch = nil
$localheaderoffset = {}
$crctab = {}
$out = nil
$cdo = nil
$cdn = 0
$cnt = 0

def exclude?(path)
  $excludes.has? do |pat| File.fnmatch?(pat, path) end
end

def dostime(t)
  (t.sec / 2) + (t.min << 5) + (t.hour << 11)
end

# xxx: overflow?
def dosdate(t)
  t.day + (t.month << 5) + ((t.year - 1980) << 9)
end

def arch
  unless $arch
    now = Time.now.utc
    date = if $datep then now.strftime("%Y%m%d") else nil end
    time = if $timep then now.strftime("%H%M")   else nil end
    stem = if $stem then $stem else File.basename(File.expand_path(".")) end
    suffix = if $suffix then $suffix else "" end
    $arch = stem + if date and time then "-#{date}T#{time}Z" elsif date then "-#{date}" elsif time then "-#{time}Z" else "" end + suffix
  end
  $arch
end

def virtpath(p)
  "#{arch}/#{p}"
end

def me
  File.basename(__FILE__).sub(/\..*?$/, "")
end

def warn(s)
  STDERR.puts("#{me}: #{s}")
end

def bounds(i, a, b)
  raise unless (i >= a) and (i <= b)
end

def str(s)
  $out.write(s)
end

def u08(i)
  bounds(i, 0, 0xff)
  $out.putc(i)
end

def u16(i)
  bounds(i, 0, 0xffff)
  u08(i & 0xff); i >>= 8
  u08(i & 0xff)
end

def u32(i)
  bounds(i, 0, 0xffffffff)
  u08(i & 0xff); i >>= 8
  u08(i & 0xff); i >>= 8
  u08(i & 0xff); i >>= 8
  u08(i & 0xff)
end

def selfdot(d)
  if d == "" then "." else d end
end

def selfnull(d, n)
  if d == "" then n else "#{d}/#{n}" end
end

def dirpaths(d, &b)
  (Dir.entries(selfdot(d)) - [".", ".."]).sort.map do |n| selfnull(d, n) end
end

def dirh(p)
end

def filedata(p, &b)
  chunkmax = 65536
  File.open(p, "rb") do |f| 
    loop do
      chunk = f.read(chunkmax)
      break if not chunk
      b.call(chunk)
      break if chunk.length < chunkmax
    end
  end
end

def crc(p)
  c = 0; filedata(p) do |s| c = Zlib.crc32(s, c) end; c
end

def cpy(p)
  filedata(p) do |s| $out.write(s) end
end

def file(p)
  $localheaderoffset[p] = $out.pos
  $crctab[p] = crc(p)
  u32(0x04034b50)
  u16(0)  #version needed to extract
  u16(0)  #flags
  u16(0)  #compression method
  u16(dostime(File.mtime(p)))
  u16(dosdate(File.mtime(p)))
  u32($crctab[p])
  u32(File.size(p))
  u32(File.size(p))
  u16(virtpath(p).length)
  u16(0)
  str(virtpath(p))
  cpy(p)
end

def centraldirectory(p)
  off = $out.pos
  u32(0x02014b50)
  u16(0)  # version made by
  u16(0)  # version to extract
  u16(0)  # flags
  u16(0)  # compression method
  u16(dostime(File.mtime(p)))
  u16(dosdate(File.mtime(p)))
  u32($crctab[p])
  u32(File.size(p))
  u32(File.size(p))
  u16(virtpath(p).length)
  u16(0)
  u16(0)
  u16(0)
  u16(0)
  u32(0)
  u32($localheaderoffset[p])
  str(virtpath(p))
  $cnt += 1
  $cdn += $out.pos - off
end

def endofcentraldirectory
  u32(0x06054b50)
  u16(0)
  u16(0)
  u16($cnt)
  u16($cnt)
  u32($cdn)
  u32($cdo)
  u16(0)
end

def walk(d, &b)
  dirpaths(d).each do |p|
    if exclude?(p) or exclude?(File.basename(p))
      warn("excluding #{p}")
    elsif File.directory?(p)
      walk(p, &b)
    elsif File.file?(p)
      b.call(p)
    else
      warn("skipping weird file #{p}")
    end
  end
end

OptionParser.new do |o|
  o.on("-d", "--date", "Append date to archive name") do |x| $datep = x end
  o.on("-t", "--time", "Append time to archive name") do |x| $timep = x end
  o.on("-s", "--stem STEM", "Set the archive name stem to STEM") do |x| $stem = x end
  o.on("-u", "--suffix SUFFIX" "Set the archive name suffix to SUFFIX") do |x| $suffix = x end
  o.on("-x", "--exclude PATTERN", "Exclude files matching PATTERN") do |x| $excludes.push(x) end
end.parse!
raise "Such an archive file already exists" if File.exist?("../#{arch}.zip")
$out = File.open("../#{arch}.zip", "wb")
walk("") do |p| file(p) end
$cdo = $out.pos
walk("") do |p| centraldirectory(p) end
endofcentraldirectory
