#! /usr/bin/env ruby

Outw = 11
Outh = 8
Bytesperline = (Outw / 8.0).ceil

@table = [nil] * 128

def reset
  @codepoint = nil
  @inbitmap = false
  @rows = []
  @w = nil
  @h = nil
  @xoff = nil
  @yoff = nil
end

def emittext(bitmap)
  0.upto(Outh) do |y|
    0.upto(Outw) do |x|
      STDOUT.write(if bitmap[Outw * y + x] == 1
        "#"
      else
        "."
      end)
    end
    STDOUT.puts
  end
  STDOUT.puts
end

@timesemitted = 0

def emit24bitmask(bitmap)
  return if @timesemitted > 0
  @timesemitted += 1
  STDOUT.binmode
  Outh.times do |y|
    Outw.times do |x|
      byte = if bitmap[Outw * y + x] == 0
               0
             else
               0xff
             end
      STDOUT.putc(byte)
      STDOUT.putc(byte)
      STDOUT.putc(byte)
    end
  end
end

def emitbitmask(bitmap)
  binary = ""
  Outh.times do |y|
    Bytesperline.times do |byteidx|
      byte = 0
      8.times do |bitidx|
        if byteidx * 8 + bitidx < Outw
          byte |= (bitmap[Outw * y + byteidx * 8 + bitidx] << bitidx)
        end
      end
      binary += byte.chr
    end
  end
  @table[@codepoint] = binary if @codepoint and (@codepoint < @table.length)
end

def emit
  bitmap = [0] * Outw * Outh
  y = 0 #@yoff
  @rows.each do |row|
    x = 0
    while row != 0
      if (row & 1) != 0
        bitmap[Outw * y + (@w - x)] = 1  # + @xoff
      end
      x += 1
      row >>= 1
    end
    y += 1
  end
  emitbitmask(bitmap)
  reset
end

reset
@input = File.open("font.bdf")
@input.each_line do |line|
  case line
  when /^STARTCHAR\s+(.*?)\s*$/
    #STDOUT.puts($1)
  when /^ENCODING\s+(\d+)\s*$/
    @codepoint = $1.to_i
  when /^BBX\s+(\d+)\s+(\d+)\s+(-?\d+)\s+(-?\d+)\s*$/
    @w = $1.to_i
    @h = $2.to_i
    @xoff = $3.to_i
    @yoff = $4.to_i
  when /^BITMAP\s*$/
    @inbitmap = true
  when /^ENDCHAR\s*$/
    emit
  else
    if @inbitmap
      raise "Bad bitmap line: #{line}" unless line =~ /^([0-9A-Fa-f]+)$/
      @rows.push($1.to_i(16) >> ($1.length * 4 - @w))
    end
  end
end

STDOUT.binmode
@table.each do |bytes|
  if bytes
    STDOUT.write(bytes)
  else
    STDOUT.write(0.chr * (Outh * Bytesperline))
  end
end
