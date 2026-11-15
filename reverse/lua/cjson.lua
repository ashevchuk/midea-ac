-- Minimal pure-Lua stand-in for the cjson C module, implementing only
-- encode/decode. Needed because the downloaded device protocol scripts
-- do `require "cjson"` and no C extension is installed; this file is
-- picked up by `require` because it sits next to the device script.

local M = {}

local function is_array(t)
  local n = 0
  for _ in pairs(t) do n = n + 1 end
  for i = 1, n do
    if t[i] == nil then return false end
  end
  return true, n
end

local escapes = {
  ['"'] = '\\"', ['\\'] = '\\\\', ['\n'] = '\\n', ['\r'] = '\\r', ['\t'] = '\\t',
}

local function encode_string(s)
  return '"' .. s:gsub('[%c"\\]', function(c) return escapes[c] or string.format('\\u%04x', c:byte()) end) .. '"'
end

local encode_value

local function encode_table(t)
  local ok, n = is_array(t)
  if ok then
    local parts = {}
    for i = 1, n do parts[i] = encode_value(t[i]) end
    return '[' .. table.concat(parts, ',') .. ']'
  end
  local parts = {}
  for k, v in pairs(t) do
    parts[#parts + 1] = encode_string(tostring(k)) .. ':' .. encode_value(v)
  end
  return '{' .. table.concat(parts, ',') .. '}'
end

encode_value = function(v)
  local ty = type(v)
  if ty == 'string' then return encode_string(v)
  elseif ty == 'number' then return tostring(v)
  elseif ty == 'boolean' then return v and 'true' or 'false'
  elseif ty == 'table' then return encode_table(v)
  elseif ty == 'nil' then return 'null'
  else error('cjson shim: cannot encode type ' .. ty) end
end

function M.encode(v)
  return encode_value(v)
end

-- Decoder: small recursive-descent parser, enough for this project's needs.
local function skip_ws(s, i)
  local _, j = s:find('^%s*', i)
  return j + 1
end

local parse_value

local function parse_string(s, i)
  assert(s:sub(i, i) == '"', 'expected string')
  local j = i + 1
  local out = {}
  while true do
    local c = s:sub(j, j)
    if c == '' then error('unterminated string') end
    if c == '"' then return table.concat(out), j + 1 end
    if c == '\\' then
      local e = s:sub(j + 1, j + 1)
      local map = { n = '\n', r = '\r', t = '\t', ['"'] = '"', ['\\'] = '\\', ['/'] = '/' }
      if map[e] then
        out[#out + 1] = map[e]; j = j + 2
      elseif e == 'u' then
        local hex = s:sub(j + 2, j + 5)
        out[#out + 1] = string.char(tonumber(hex, 16) % 256)
        j = j + 6
      else
        out[#out + 1] = e; j = j + 2
      end
    else
      out[#out + 1] = c; j = j + 1
    end
  end
end

local function parse_number(s, i)
  local num = s:match('^-?%d+%.?%d*[eE]?[+-]?%d*', i)
  return tonumber(num), i + #num
end

local function parse_array(s, i)
  local out = {}
  i = skip_ws(s, i + 1)
  if s:sub(i, i) == ']' then return out, i + 1 end
  while true do
    local v, j = parse_value(s, i)
    out[#out + 1] = v
    i = skip_ws(s, j)
    local c = s:sub(i, i)
    if c == ',' then i = skip_ws(s, i + 1)
    elseif c == ']' then return out, i + 1
    else error('expected , or ] at ' .. i) end
  end
end

local function parse_object(s, i)
  local out = {}
  i = skip_ws(s, i + 1)
  if s:sub(i, i) == '}' then return out, i + 1 end
  while true do
    local k, j = parse_string(s, i)
    i = skip_ws(s, j)
    assert(s:sub(i, i) == ':', 'expected : at ' .. i)
    local v, j2 = parse_value(s, skip_ws(s, i + 1))
    out[k] = v
    i = skip_ws(s, j2)
    local c = s:sub(i, i)
    if c == ',' then i = skip_ws(s, i + 1)
    elseif c == '}' then return out, i + 1
    else error('expected , or } at ' .. i) end
  end
end

parse_value = function(s, i)
  i = skip_ws(s, i)
  local c = s:sub(i, i)
  if c == '"' then return parse_string(s, i)
  elseif c == '{' then return parse_object(s, i)
  elseif c == '[' then return parse_array(s, i)
  elseif s:sub(i, i + 3) == 'true' then return true, i + 4
  elseif s:sub(i, i + 4) == 'false' then return false, i + 5
  elseif s:sub(i, i + 3) == 'null' then return nil, i + 4
  else return parse_number(s, i) end
end

function M.decode(s)
  local v = parse_value(s, 1)
  return v
end

return M
