-- Quantize 24-bit 0xRRGGBB colors to fit the (default) XTerm
-- 256-color palette. The palette consists of a 6x6x6 RGB color cube
-- plus 24 shades of gray. Each color axis scales linearly with no
-- tweaks to exploit human color sensitivity.
--
-- In addition to the XTerm-originated 256-color encoding there is
-- also an ISO standard 24-bit RGB encoding for terminals but as of
-- this writing only a couple of terminal emulators support it.

function xterm_from_rgb6(rgb6)
  local b = rgb6 % 256; rgb6 = (rgb6 - b) / 256
  local g = rgb6 % 256; rgb6 = (rgb6 - g) / 256
  local r = rgb6 % 256; rgb6 = (rgb6 - r) / 256
  if r == g and r == b then
    -- 232..255 shades of gray
    return 232 + math.floor((r - 8) / 10)
  end
  r = math.floor((r - 55) / 40)
  g = math.floor((g - 55) / 40)
  b = math.floor((b - 55) / 40)
  -- 16..230 RGB color cube 6x6x6
  return 16 + 6*6*r + 6*g + b
end

function foreground(rgb6)
  io.write("\x1b[38;5;"..tostring(xterm_from_rgb6(rgb6)).."m")
end

function background(rgb6)
  io.write("\x1b[48;5;"..tostring(xterm_from_rgb6(rgb6)).."m")
end

function reset()
  io.write("\x1b[0m")
end

function output_background_palette()
  for r = 55,255,40 do
    for g = 55,255,40 do
      for b = 55,255,40 do
        background(256*256*r + 256*g + b)
        io.write("aa ")
      end
      reset(); print()
    end
    reset(); print()
  end
  for i = 8,255,10 do
    background(256*256*i + 256*i + i)
    io.write("aa ")
  end
  reset(); print()
end

output_background_palette()
