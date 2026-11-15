-- Phase 1 generator harness (dev-time only, run via luajit, never at
-- ac-ng.pl runtime). Treats the Midea-cloud-issued device lua script as a
-- black-box oracle: calls its real jsonToData/dataToJson entry points with
-- controlled inputs and diffs the outputs to derive byte/bit-level offsets,
-- instead of reading the lua source by eye.
--
-- Usage: luajit harness.lua <device.lua>
-- Prints one JSON object to stdout; writes nothing else there.

local JSON = require "cjson"

local device_path = arg[1]
if not device_path then
  io.stderr:write("usage: harness.lua <device.lua>\n")
  os.exit(1)
end

local src_fh = io.open(device_path, "r")
local src = src_fh:read("*a")
src_fh:close()

-- keyT["KEY_X"] = "field_name" — the script's own authoritative property
-- name list, read as plain text (keyT is a file-local, not reachable after
-- dofile, and that's fine: this is a static string-literal list anyway).
local keyT_fields = {}
local seen = {}
for key, name in src:gmatch('keyT%["([%u_%d]+)"%]%s*=%s*"([%a_%d]+)"') do
  if not seen[name] then
    seen[name] = true
    keyT_fields[#keyT_fields + 1] = name
  end
end

dofile(device_path)
assert(type(jsonToData) == "function", "device script has no global jsonToData")
assert(type(dataToJson) == "function", "device script has no global dataToJson")

local DEVICE_INFO = { deviceSubType = 2, deviceSN = "00000000000000Q1B0" }

-- Bootstrap baseline: the minimal set of control fields this model's
-- jsonToModel needs populated on every call to avoid a nil arithmetic
-- error (found by trial: fields the script only conditionally sets from
-- "if streams[x] ~= nil", with no nil-safe default, are read unconditionally
-- later while building bodyBytes). This is model-specific and was derived
-- by hand for 00000Q1B; a different device lua may need different/fewer
-- base fields — if the probe below reports every field as "skipped" with
-- the same "bad argument ... nil" error, that's the signal to extend this.
local BASE_CONTROL = {
  power = "on", mode = "cool", temperature = 24, small_temperature = 0,
  wind_speed = 50, wind_swing_ud = "off", wind_swing_lr = "off",
  strong_wind = "off", comfort_sleep = "off", purifier = "off",
  eco = "off", dry = "off", prevent_cold = "off", power_saving = "off",
  ptc = "off", sleep_status = 0,
}

local function merge(base, extra)
  local t = {}
  for k, v in pairs(base) do t[k] = v end
  for k, v in pairs(extra) do t[k] = v end
  return t
end

-- jsonToModel's "control" path does not call the script's own init_keyP(),
-- so keyP state leaks across calls within one Lua VM. dataToJson calls
-- init_keyP() itself and is self-resetting, but jsonToData is not — reload
-- the whole script before every control probe so each one starts clean.
local function encode_control(ctrl)
  dofile(device_path)
  local env = { deviceinfo = DEVICE_INFO, control = ctrl }
  return pcall(jsonToData, JSON.encode(env))
end

local function decode_status(hexframe)
  local env = { deviceinfo = DEVICE_INFO, msg = { data = hexframe } }
  local ok, out = pcall(dataToJson, JSON.encode(env))
  if not ok then return false, out end
  local decoded = JSON.decode(out)
  return true, decoded and decoded.status or {}
end

-- Byte-diff between two same-length hex frames -> list of {offset, a, b}.
local function diff_hex(h1, h2)
  local out = {}
  local n = math.min(#h1, #h2) / 2
  for i = 0, n - 1 do
    local b1 = tonumber(h1:sub(i * 2 + 1, i * 2 + 2), 16)
    local b2 = tonumber(h2:sub(i * 2 + 1, i * 2 + 2), 16)
    if b1 ~= b2 then out[#out + 1] = { offset = i, a = b1, b = b2 } end
  end
  return out
end

-- ===== SET-direction probing =====

local ok, base_frame = encode_control(BASE_CONTROL)
if not ok then
  io.stderr:write("FATAL: baseline control probe failed: " .. tostring(base_frame) .. "\n")
  os.exit(1)
end

-- The device script picks ONE of two wire formats per call: a fixed-layout
-- "regular" body (type 0x40) when keyP.propertyNumber stays 0, or a
-- variable-length B0 property-list body when a field pushes it above 0.
-- A raw byte diff only means "fixed offset" for two regular-type frames;
-- for two B0 frames it means "the same property tag changed value", which
-- needs TLV parsing, not an offset.
local function frame_body_type(hex)
  return hex:sub(21, 22)
end

-- B0 body: [0xb0][count][tag_lo][tag_hi][pad][value]... repeated.
local function parse_b0_tags(hex)
  local bytes = {}
  for i = 0, (#hex / 2) - 1 do
    bytes[i] = tonumber(hex:sub(i * 2 + 1, i * 2 + 2), 16)
  end
  local count = bytes[11] -- body[1], body[0] is at byte index 10
  local pos = 12 -- body[2]
  local tags = {}
  for _ = 1, (count or 0) do
    if pos + 3 > #hex / 2 then break end
    local tag = bytes[pos] + bytes[pos + 1] * 256
    local value = bytes[pos + 3]
    tags[tag] = value
    pos = pos + 4
  end
  return tags
end

-- Regular-body diff, excluding the trailing checksum/CRC bytes (the last
-- two bytes of the frame always change when anything earlier does, and
-- are not a property location).
local function regular_diff(h1, h2)
  local total = math.min(#h1, #h2) / 2
  local d = diff_hex(h1:sub(1, (total - 2) * 2), h2:sub(1, (total - 2) * 2))
  return d
end

-- "on"/"off" cover most booleans; the enum tokens cover mode/fan-speed-like
-- fields cheaply (an unrecognized token is just ignored or pcall-caught).
local PROBE_VALUES = {
  "on", "off", "cool", "heat", "auto", "dry", "fan", 16, 30, 70,
}
local NUMERIC_FALLBACK = { 0, 1, 2, 3, 5, 8, 10, 16, 30, 50, 64, 100, 127 }

local set_results = {}

for _, field in ipairs(keyT_fields) do
  if BASE_CONTROL[field] == nil then
    local result = { field = field, state = "not_found" }
    local frames = {} -- probe value -> hex frame
    local any_ok, last_err = false, nil

    local function try_value(v)
      local ok2, frame = encode_control(merge(BASE_CONTROL, { [field] = v }))
      if not ok2 then last_err = frame; return end
      any_ok = true
      frames[v] = frame
    end

    -- A B0 frame alone isn't a positive result — the field's own tag must
    -- actually vary across probes, not just ride along as a constant
    -- companion. Check that before deciding whether to widen the sweep.
    local function varying_b0_tags()
      local tag_values_by_probe = {}
      for v, frame in pairs(frames) do
        if frame_body_type(frame) == "b0" then
          tag_values_by_probe[v] = parse_b0_tags(frame)
        end
      end
      local candidate_tags = {}
      for _, tags in pairs(tag_values_by_probe) do
        for tag in pairs(tags) do candidate_tags[tag] = true end
      end
      local varying = {}
      for tag in pairs(candidate_tags) do
        local distinct = {}
        for v, tags in pairs(tag_values_by_probe) do distinct[tags[tag] or "\0nil"] = true end
        local nd = 0
        for _ in pairs(distinct) do nd = nd + 1 end
        if nd > 1 then
          local vals = {}
          for v, tags in pairs(tag_values_by_probe) do vals[v] = tags[tag] end
          varying[#varying + 1] = { tag = tag, values = vals }
        end
      end
      return varying
    end

    local function have_positive_regular()
      for v, frame in pairs(frames) do
        if frame_body_type(frame) == "40" and #regular_diff(base_frame, frame) > 0 then
          return true
        end
      end
      return false
    end

    for _, v in ipairs(PROBE_VALUES) do try_value(v) end
    local varying = varying_b0_tags()
    local have_b0, have_regular_diff = (#varying > 0), have_positive_regular()
    if not have_regular_diff and not have_b0 and any_ok then
      for _, v in ipairs(NUMERIC_FALLBACK) do try_value(v) end
      varying = varying_b0_tags()
      have_b0 = (#varying > 0)
      have_regular_diff = have_positive_regular()
    end

    if have_b0 then
      result.state = "derived"
      result.kind = "extended_property"
      result.tags = varying
    elseif have_regular_diff then
      result.state = "derived"
      result.kind = "regular"
      local observed = {}
      for v, frame in pairs(frames) do
        if frame_body_type(frame) == "40" then
          local d = regular_diff(base_frame, frame)
          if #d > 0 then observed[#observed + 1] = { probe = v, diff = d } end
        end
      end
      result.observations = observed
    elseif any_ok then
      result.state = "not_found"
    else
      result.state = "skipped"
      result.error = last_err
    end
    set_results[#set_results + 1] = result
  end
end

-- ===== GET-direction probing (bit-flip sweep over the body) =====

local ok2, status_frame = true, base_frame:sub(1, 20) .. "c0" .. base_frame:sub(23)
local base_decode_ok, base_status = decode_status(status_frame)
if not base_decode_ok then
  io.stderr:write("FATAL: baseline status decode failed: " .. tostring(base_status) .. "\n")
  os.exit(1)
end

-- header (10 bytes) is left alone; sweep the body only.
local body_len = (#status_frame / 2) - 10 - 2 -- minus header, minus trailing checksum/crc
local get_results = {}

-- byte 0 is the frame-type selector (0xC0/0xA0/...), not a property; flipping
-- it makes binToModel reject the whole frame, which would otherwise look
-- like every property changing at once.
for byte_off = 1, body_len - 1 do
  for bit_idx = 0, 7 do
    local pos = 10 + byte_off
    local cur = tonumber(status_frame:sub(pos * 2 + 1, pos * 2 + 2), 16)
    local mask = bit.lshift(1, bit_idx)
    local flipped = bit.bxor(cur, mask)
    local variant = status_frame:sub(1, pos * 2) ..
        string.format("%02x", flipped) .. status_frame:sub(pos * 2 + 3)
    local ok3, out = decode_status(variant)
    if ok3 then
      local changes = {}
      for k, v in pairs(out) do
        if base_status[k] ~= v then
          changes[#changes + 1] = { property = k, base_value = base_status[k], flipped_value = v }
        end
      end
      for k, v in pairs(base_status) do
        if out[k] == nil and v ~= nil then
          changes[#changes + 1] = { property = k, base_value = v, flipped_value = nil }
        end
      end
      -- A single real property bit changes at most a couple of fields
      -- (e.g. temperature + its half-degree companion). A flip that
      -- disturbs many fields at once broke the frame structurally (e.g.
      -- shifted a length/type byte the decoder keys off), not a property —
      -- record it once as structural instead of polluting every property.
      if #changes > 3 then
        get_results[#get_results + 1] = {
          byte = byte_off, bit = bit_idx, structural = true, affected_count = #changes,
        }
      else
        for _, c in ipairs(changes) do
          c.byte = byte_off; c.bit = bit_idx
          get_results[#get_results + 1] = c
        end
      end
    end
  end
end

print(JSON.encode({
  source_file = device_path,
  keyT_fields = keyT_fields,
  base_control = BASE_CONTROL,
  base_frame = base_frame,
  base_status = base_status,
  set_results = set_results,
  get_results = get_results,
}))
