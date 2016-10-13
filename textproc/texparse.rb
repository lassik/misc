class String
  def map_chr(&block)
    r = ""
    self.each_byte do |byte|
      rc = block.call(byte.chr)
      r += rc if rc
    end
    r
  end
end

class To_html
  def before
    "<html>"
    "<head>"
    "<title></title>"
    "</head>"
    "<body>"
  end
  def after
    "</body>"
    "</html>"
  end
  def quote(str)
    str.map_chr do |chr|
      case chr
      when "\"" then "&quot;"
      when "&"  then "&amp;"
      when "<"  then "&lt;"
      when ">"  then "&gt;"
      else chr
      end
    end
  end
  def h(heading)
    puts "<h1>#{quote heading}</h1>"
  end
  def hh(heading)
    puts "<h2>#{quote heading}</h2>"
  end
  def hhh(heading)
    puts "<h3>#{quote heading}</h3>"
  end
  def e(text)
    puts "<em>#{quote text}</em>"
  end
  def l(text, url)
    puts "<a href=\"#{quote url}\">#{quote text}</a>"
  end
end

$thischr = nil

def mygetc
  r = STDIN.getc
  if r then r.chr else :eof end
end

def peekchr
  $thischr = mygetc if not $thischr
  $thischr
end

def readchr
  chr = ($thischr or mygetc)
  raise if chr == :eof
  $thischr = nil
  chr
end

def isnamechr(chr)
  chr =~ /^[A-Za-z_]$/
end

def directive
  cmd = ""
  cmd += readchr while isnamechr(peekchr)
  args = [cmd]
  while peekchr == "{"
    readchr
    arg = ""
    loop do
      case peekchr
      when "{"
        raise
      when "}"
        readchr
        break
      when "\\"
        readchr
        arg += readchr
      else
        arg += readchr
      end
    end
    args.push(arg)
  end
  p args
end

def toplevel
  loop do
    chr = peekchr
    case chr
    when :eof
      break
    when "\\"
      readchr
      directive
    else
      tok = ""
      loop do
        case peekchr
        when :eof
          break
        when "\\"
          break
        else
          tok += readchr
        end
      end
      paragraphs = tok.strip.split /^\s*$/
      p tok
    end
  end
end

toplevel
