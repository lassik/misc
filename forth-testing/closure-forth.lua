-- Compile Forth code into nested closures

local stack = {}
local variables = {}

-- function   -- executed word
-- {function} -- compiled word
-- "string"   -- sentinel word
local dictionary = {}

function defexecuted(name, fn)
  dictionary[name] = fn
end

function defbinaryop(name, fn)
  defexecuted(
    name,
    function()
      local b, a = pop(), pop()
      push(fn(a, b))
    end
  )
end

function defcompiled(name, fn)
  dictionary[name] = {fn}
end

function defsentinel(name)
  dictionary[name] = name
end

function definition(name)
  local ans = dictionary[name]
  if not ans then error("Undefined: "..tostring(name)) end
  return ans
end

function issentinel(word, which)
  return dictionary[word] == which
end

function iter(tbl)
  local i = 1
  return function()
    local ans = nil
    if i <= #tbl then
      ans = tbl[i]
      i = i + 1
    end
    return ans
  end
end

function printstack()
  print(unpack(stack))
end

function push(x)
  table.insert(stack, x)
end

function pop()
  assert(#stack > 0)
  return table.remove(stack)
end

defexecuted(".s", printstack)
defexecuted("drop", pop)
defbinaryop("2dup", function(a, b) push(a); push(b); push(a); return b end)
defbinaryop("+", function(a, b) return a + b end)
defbinaryop("-", function(a, b) return a - b end)
defbinaryop("<", function(a, b) return a < b end)

function compile(word, worditer)
  if type(word) == "number" then
    return function() push(word) end
  end
  local def = definition(word)
  if type(def) == "function" then -- executed
    return def
  elseif type(def) == "table" then -- compiled
    assert((#def == 1) and (type(def[1]) == "function"))
    return def[1](worditer)
  end
  error("Cannot compile "..tostring(word))
end

function execute(xt)
  assert(type(xt) == "function")
  -- TODO: If we stored the name/description of xt somewhere, we could
  -- implement a trace feature that printed here the name/description
  -- of the token that's about to be executed.
  xt()
end

function execlist(xts)
  for _, xt in ipairs(xts) do
    execute(xt)
  end
end

defcompiled(
  "if",
  function(worditer)
    local thens, elses, inelse = {}, {}, false
    for word in worditer do
      if issentinel(word, "then") then
        break
      elseif issentinel(word, "else") then
        assert(not inelse)
        inelse = true
      elseif not inelse then
        table.insert(thens, compile(word, worditer))
      else
        table.insert(elses, compile(word, worditer))
      end
    end
    return function()
      if pop() then
        execlist(thens)
      else
        execlist(elses)
      end
    end
  end
)

defsentinel("then")
defsentinel("else")

function compilelist(words)
  local compiled = {}
  local worditer = iter(words)
  for word in worditer do
    table.insert(compiled, compile(word, worditer))
  end
  return function()
    execlist(compiled)
  end
end

function demo(anum, bnum)
  local program = {
    anum, bnum, "2dup", "<", "if", "+", "else", "-", "then", ".s", "drop"
  }
  execute(compilelist(program))
end

demo(5, 2)
demo(2, 5)
