# Protocol notes extracted from T_0000_AC_00000Q1B_2024013001.lua

Source: Midea cloud-issued device-specific protocol script (model `00000Q1B`,
dated 2024-01-30), downloaded via `ac-check.pl --lua`. Functions referenced:
`jsonToModel`/`jsonToData` (JSON → wire bytes, "set" direction) and
`binToModel` (wire bytes → JSON, "status" direction).

Indexing: `messageBytes[0]` / `bodyBytes[0]` is the byte right after the
10-byte `AA` frame header — i.e. the same body-type byte `ac.pl` calls
`body[0]`. Status responses are typed `0xC0`/`0xA0`/`0xA1`/`0xB0`/`0xB1` at
that index; a plain set command is typed `0x40` (`keyB.BYTE_CONTROL_CMD`),
an extended-property set command is typed `0xB0` (`keyB.BYTE_CONTROL_PROPERTY_CMD`).
Indices line up 1:1 with `ac.pl`'s `body[]` — no fixed offset shift.

## Fixed main-body fields (bit flags in the plain `0x40`/`0xC0` frame)

| Property | Direction | Byte[bit/mask] | Notes |
|---|---|---|---|
| power | encode | `body[1] & 0x01` | `powerValue` OR'd into body[1] with client-mode/timer/buzzer bits |
| power | decode | `body[1] & 0x01` | |
| buzzer | encode | `body[1] & 0x40` | `BYTE_BUZZER_ON = 0x40` |
| mode | encode | `body[2] & 0xE0` | top 3 bits |
| mode | decode | `body[2] & 0xE0` | |
| temperature (int part) | encode | `body[2] & 0x0F` = `temp - 0x10` | |
| temperature (half-degree) | encode | `body[2] bit4 (0x10)` | |
| temperature (half-degree, alt layout) | decode | `body[1] bit1 (0x02)` or `body[1] bit6 (0x40)` depending on branch | two different bit layouts coexist in decode, selected by other flags |
| fan speed | encode/decode | `body[3] & 0x7F` | |
| swing L/R (on/off) | encode/decode | `body[7] & 0x03` | |
| swing U/D (on/off) | encode/decode | `body[7] & 0x0C` | |
| strong wind ("强劲", separate from turbo) | encode/decode | `body[8] & 0x20` | `BYTE_STRONG_WIND_ON = 0x20` |
| sleep level | encode/decode | `body[8] & 0x03` | `BYTE_SLEEP_ON = 0x03` (2-bit value, not a single flag bit) |
| power saving | encode/decode | `body[8] & 0x08` | |
| **eco** | **encode** | **`body[9] bit7 (0x80)`** | `BYTE_ECO_ON = 0x80` |
| **eco** | **decode** | **`body[9] bit4 (0x10)`**, left-shifted ×8 internally to compare against `0x80` | wire read-bit ≠ wire write-bit, see cross-check below |
| natural wind | decode | `body[9] bit1 (0x02)` | not seen set in this model's encode path |
| aux heat (PTC) | encode/decode | `body[9] & 0x18` | 2-bit field |
| purifier | encode/decode | `body[9] & 0x20` | |
| dry | encode/decode | `body[9] & 0x04` | |
| sleep-curve switch | encode/decode | `body[9] bit6 (0x40)` | `comfortableSleepSwitch` |
| **turbo** ("Tubro", AE/AB variant) | **encode** | **`body[10] bit1 (0x02)`** | `keyP.tubroValue = 0x02` |
| turbo | decode | `body[10] & 0x02` | |
| sleep status flag | decode | `body[10] bit0 (0x01)` | |
| prevent-cold ("防着凉") | decode | `body[10] bit5 (0x20)` | |
| temperature unit | decode | `body[9] bit7 (0x80)` or `body[10] bit2 (0x04)`, model-dependent branch | |
| indoor temperature | decode | `body[11]` (raw, `(byte-50)/2`) | |
| outdoor temperature | decode | `body[12]` (raw, `(byte-50)/2`) | |
| error code | decode | `body[16]` | |
| screen display (current) | decode | `body[14] & 0x70` | `screenDisplayNowValue` |
| comfort power save | decode | `body[14] bit0 (0x01)` (A0 frame) or `body[22] bit0` (other frame) | frame-dependent |
| independent PTC | decode | `body[11] bit3 (0x08)` or `body[8] bit6 (0x40)` | frame-dependent |

## Extended properties (B0 set / B1 status — variable-length property list)

Format: `[propId_lo][propId_hi][len]...[value bytes]`, walked with a cursor,
same scheme `ac.pl`'s memory describes for B0/B1 angle/breezeless parsing.
Selected property IDs (id_lo/id_hi):

| Property | ID | Encode fields (from cursor) | Decode fields (from cursor) |
|---|---|---|---|
| no-wind-sense ("无风感", breezeless) | `0x18/0x00` | — (not located in encode scan range read) | level at `cursor+5` when `cursor+3==0x02`, else value at `cursor+4` |
| wind_swing_ud_angle | `0x09/0x00` | value at `cursor+3` | value at `cursor+4` (one path) or `band(cursor+21, 0x0F)` (B5-style path) |
| wind_swing_lr_angle | `0x0A/0x00` | value at `cursor+3` | value at `cursor+4` |
| buzzer (extended form) | `0x1A/0x00` | `cursor+3 = 1 if buzzerValue==0x40 else 0` | — |
| prevent_super_cool ("防过冷") | `0x49/0x00` | — | value at `cursor+4` |
| prevent_straight_wind ("防直吹") | `0x42/0x00` | — | value at `cursor+4` |
| auto_prevent_straight_wind | `0x26/0x02` | — | value at `cursor+4` |
| self_clean ("自清洁") | `0x39/0x00` | — | value at `cursor+4` |
| wind_avoid ("风避人") | `0x33/0x00` | — | value at `cursor+4` |
| intelligent_wind ("智慧风") | `0x34/0x00` | — | value at `cursor+4` |
| child_prevent_cold_wind | `0x3A/0x00` | — | value at `cursor+4` |
| little_angel ("小天使") | `0x1B/0x02` | — | value at `cursor+4` |
| cool_hot_sense ("冷热感") | `0x21/0x00` | — | value at `cursor+4` |
| security ("安防") | `0x29/0x00` | — | value at `cursor+4` |
| even_wind ("均匀风") | `0x4E/0x00` | — | value at `cursor+4` |
| single_tuyere ("单风口") | `0x4F/0x00` | — | value at `cursor+4` |
| extreme_wind ("超远风") | `0x4C/0x00` | — | value + level at `cursor+4`/`cursor+5` |
| voice_control(_new) | `0x20/0x00` | `cursor+3` value, rest `0xFF` | value at `cursor+4` |
| water_washing | `0x4A/0x00` | 3 sub-fields at `cursor+3..5` | value at `cursor+5`, time/stage at `+6/+7` |
| fresh_air | `0x4B/0x00` | 2 sub-fields | value/fan/temp at `cursor+4..6` |
| parent_control | `0x51/0x00` | 3 sub-fields | value/temp-up/temp-down at `cursor+4..6` |
| nobody_energy_save | `0x30/0x00` | 1 field + padding | value at `cursor+4` |
| prevent_straight_wind_lr | `0x58/0x00` | 1 field | value at `cursor+4` |
| water_pump | `0x50/0x00` | 1 field | value at `cursor+4` |
| intelligent_control | `0x31/0x00` | 1 field | value at `cursor+4` |
| volume_control | `0x24/0x00` | 1 field | value at `cursor+5` |
| filter level/value | `0x09/0x04` | — | level `cursor+5`, value `cursor+14` |
| pm25 | `0x0B/0x02` | — | 16-bit at `cursor+5/6` |
| indoor_humidity | varies by frame | — | `cursor+4` (B1), `cursor+40` (one variant), `cursor+3` (B5-style `b5_humidity`) |

Humidity has no fixed main-body offset in any frame read — confirms `ac.pl`
README's "get only, from B1" is the right model, not a simplification.

## Cross-check vs `ac.pl`'s documented facts

1. **`set_cmd`: `body[0x08] = 0x03` and `body[1] = 0x42` as base value**
   → **MISMATCH.** In this script, the plain set command's `body[0]` is
   `keyB.BYTE_CONTROL_CMD = 0x40` (not `0x03`, and not at index 8 — index 8
   is where sleep/strong-wind/power-saving bits live, not a fixed command
   byte). `body[1]`'s base (power off, buzzer off) comes out to
   `BYTE_CLIENT_MODE_MOBILE (0x02) | BYTE_TIMER_METHOD_REL (0x00) = 0x02`,
   not `0x42`. `0x03` does appear in this file, but as `BYTE_SLEEP_ON`
   (a per-feature value OR'd into `body[8]`), not as a fixed command byte.
   Either `ac.pl`'s note describes a different frame/offset convention, or
   this model's firmware uses different mode/timer constants than whatever
   model the original APK reverse-engineering targeted — the two are not
   directly comparable from this evidence alone.

2. **`eco` set: only sets bit 7 of `body[9]`**
   → **MATCH (encode direction).** `BYTE_ECO_ON = 0x80`, OR'd directly into
   `body[9]` with no other bits touched. Note a real asymmetry exposed by
   this file: the **decode** direction reads eco from `body[9]` **bit 4**
   (`0x10`), not bit 7 — the script left-shifts that bit by 3 purely to
   normalize it to `0x80` for internal comparison against `BYTE_ECO_ON`.
   If `ac.pl`'s status-parsing code checks bit 7 of `body[9]` to *read*
   eco state back from a status response, that would be wrong for this
   model; bit 4 is the one to check on read.

3. **`turbo` set: `body[10]` bit 1 is set, in addition to an existing bit
   elsewhere**
   → **MATCH.** `keyP.tubroValue = 0x02` (bit 1) is OR'd into `body[10]`.
   The "existing bit elsewhere" most likely refers to `BYTE_STRONG_WIND_ON
   = 0x20` in `body[8]` — a separate "强劲"/Strong-Wind flag that many
   Midea app UIs toggle together with Turbo. The two are independent
   switches in this script, consistent with `ac.pl` setting both as part
   of one "turbo" command.

4. **A `sleep` mode parameter exists and is supported**
   → **CONFIRMED (exists), offset not previously specified so no
   match/mismatch to report.** Sleep is a 2-bit field at `body[8] & 0x03`
   (`BYTE_SLEEP_ON = 0x03`), alongside a separate sleep-curve switch bit
   (`body[9] bit6`) and sleep-curve data table (`comfortByte`,
   `numstring2table` of a comma-separated curve string) not previously
   documented for `ac.pl` at all.
