# midea-ac

Command-line tools for **Midea M-Smart** air conditioners:

- **`ac.pl`** — control directly over LAN (no cloud); V2 default-key modules
- **`ac-check.pl`** — cloud iCheck / diagnostics (MSmartLife overseas)
- **`ac-cloud.pl`** — cloud login + LAN V3 (`0x8370`) token handshake / status
  (**work in progress** — handshake only, `--get` doesn't decode fields yet;
  see the "Cloud + LAN V3" section below)
- **`ac-ng.pl`** — control over LAN by running *your own unit's* cloud-issued
  protocol script through a built-in pure-Perl Lua interpreter, instead of a
  generic hand-ported byte table; see [Build a script for your own AC](#build-a-script-for-your-own-ac)

LAN mode is potentially compatible with other brands sharing the same M-Smart V3
protocol (Midea, Idea, Bek, Electrolux, Carrier, Toshiba, Neoclima, etc.). You can identify such devices
using the "NetHome Plus" app.

## Running these scripts

Every example below assumes you can just type e.g. `ac.pl --ip ...`. If
that gives you `Permission denied` or `command not found`, the file
isn't marked executable (a fresh `git clone` usually preserves this,
but a ZIP download or copying the files elsewhere won't). Two ways to
fix it — pick either one, you only need to do this once per script:

```sh
# Option 1: mark it executable, then run it directly
chmod +x ac.pl
./ac.pl --ip 192.168.1.2 --get

# Option 2: skip chmod entirely, always run it through perl
perl ac.pl --ip 192.168.1.2 --get
```

Everywhere this README writes `ac.pl --ip ...`, read that as `./ac.pl
--ip ...` or `perl ac.pl --ip ...` depending on which option you picked
— same for `ac-ng.pl`, `ac-check.pl`, `ac-cloud.pl`, and the scripts
under `lua/`.

## Requirements

**`ac.pl` (LAN):**
- Perl 5.10+
- Standard core modules only: `IO::Socket`, `Socket`, `POSIX`, `List::Util`

**`ac-check.pl` / `ac-cloud.pl` (cloud):**
- Perl 5.10+, `openssl`, `curl`
- `MIME::Base64`, `File::Temp`, `Getopt::Long` (core)
- `ac-cloud.pl` also needs `IO::Socket` / `Socket` for LAN V3
## Usage

```
ac.pl --ip <address> [--get | --set | --caps] [options]
```

### Get current state

```sh
# All parameters
ac.pl --ip 192.168.1.2 --get

# Specific parameters
ac.pl --ip 192.168.1.2 --get --temp --mode --power

# Value only
ac.pl --ip 192.168.1.2 --get --power --value
```

### Set parameters

```sh
# Turn on and configure
ac.pl --ip 192.168.1.2 --set --power on --mode cool --temp 20

# Single parameter
ac.pl --ip 192.168.1.2 --set --temp 17
```

### Device capabilities

Query supported modes, temperature range, swing directions, turbo, eco and other
device capabilities via B5 (including a second frame when needed), plus B1
properties and group_data power/runtime:

```sh
ac.pl --ip 192.168.1.2 --caps
```

Output includes current state fields alongside `cap_*` capability flags, for example:

```
cap_modes:cool+heat+dry+auto
cap_swing:up_down+left_right
cap_turbo:heat
cap_eco:yes
cap_cool_min:16
cap_cool_max:30
...
```

### Device discovery

```sh
# UDP broadcast (fast)
ac.pl --ip 255.255.255.255 --discover

# Network scan
ac.pl --ip 192.168.1.0/24 --discover

# IP range
ac.pl --ip 192.168.1.1-192.168.1.8 --discover
```

### Exit code for scripting

Exit `0` if the parameter is ON, `1` otherwise:

```sh
ac.pl --ip 192.168.1.2 --exit power && echo "AC is on"
ac.pl --ip 192.168.1.2 --exit power || echo "AC is off"
```

## Parameters

| Option | Values | Description |
|--------|--------|-------------|
| `--power` | `on` / `off` | Power state |
| `--temp` | `16..30` (step 0.5) | Target temperature, °C |
| `--mode` | `auto` `cool` `dry` `heat` `fan` | Operation mode |
| `--fan` | `auto` `high` `medium` `low` `silent` (`mute` = alias) | Fan speed; output always `silent` |
| `--swing` | `off` `vertical` `horizontal` `both` | Louver swing mode (C0) |
| `--swing_h` | `off` `left` `left_mid` `middle` `right_mid` `right` | Fixed horizontal angle (B0/B1); on this model `off` is ignored |
| `--swing_v` | `off` `up` `up_mid` `middle` `down_mid` `down` | Fixed vertical angle (B0/B1); on this model `off` is ignored |
| `--breezeless` | `on` / `off` | Breezeless / no-wind-feel (B0/B1) |
| `--display` | `on` / `off` | Panel screen display (B0/B1) |
| `--humidity` | (get only) | Indoor humidity % from B1 |
| `--turbo` | `on` / `off` | Turbo mode |
| `--eco` | `on` / `off` | Eco mode |
| `--sleep` | `on` / `off` | Sleep mode |
| `--buzzer` | `on` / `off` | Audible feedback (prompt tone) |
| `--led` | `on` / `off` | C0 display LED |
| `--unit` | `C` / `F` | Temperature unit |
| `--power_saving` | `on` / `off` | Power saving |
| `--smart_eye` | `on` / `off` | Smart eye |
| `--dry_clean` | `on` / `off` | Dry clean |
| `--aux_heat` | `on` / `off` | Auxiliary PTC heat |
| `--anion` | `on` / `off` | Anion / ionizer |
| `--natural_wind` | `on` / `off` | Natural wind |
| `--frost_protect` | `on` / `off` | Frost protect |
| `--comfort` | `on` / `off` | Comfort mode |

`--exit` accepts any boolean field above (plus `error`).

## Output formatting

Output format is fully configurable for integration with home automation systems:

```sh
# Formatted JSON
ac.pl --ip 192.168.1.2 --get \
  --begin '{\n\t' --end '\n}\n' --delimiter ',\n\t' --quote '"' --unquote_num

# Compact JSON
ac.pl --ip 192.168.1.2 --get \
  --begin '{' --end '}' --delimiter ',' --quote '"' --unquote_num

# JSON array of values
ac.pl --ip 192.168.1.2 --get --value \
  --begin '[' --end ']' --delimiter ',' --quote '"' --unquote_num
```

| Option | Default | Description |
|--------|---------|-------------|
| `--value` | — | Print values only, without field names |
| `--begin` | — | String prepended to output |
| `--end` | `\n` | String appended to output |
| `--separator` | `:` | Between field name and value |
| `--delimiter` | `\n` | Between fields |
| `--quote` | — | Wrap names and values in this string |
| `--unquote_num` | — | Skip quoting for numeric values |

## Protocol

Communicates directly with the device over **TCP 6444** (discovery on **UDP 6445**) using the Midea M-Smart V3 binary protocol with AES-128-ECB encryption. The AES implementation is self-contained pure Perl — no compiled modules required.

## Debug

```sh
ac.pl --ip 192.168.1.2 --get --debug
```

Prints raw protocol packets (hex) and decoded field values to stderr.

## Cloud iCheck (`ac-check.pl`)

Cloud diagnostics for MSmartLife / SmartHome overseas (appId `1010`). Needs
**Perl + openssl + curl**. Credentials via env or flags (never hardcode in the file).

iCheck is **not** a direct MAS alias: the app wraps calls as
`/v1/app2base/data/transmit` with `param.serviceUrl=/orac-check/icheck/...`
and AES-128-CBC session keys derived after login.

```sh
# Credentials via flags
ac-check.pl --user EMAIL --pass PASS --list
ac-check.pl --user EMAIL --pass PASS --device-id <APPLIANCE_ID> --start --more
ac-check.pl --user EMAIL --pass PASS --device-id <APPLIANCE_ID> --history
ac-check.pl --user EMAIL --pass PASS --device-id <APPLIANCE_ID> \
  --history-id <HISTORY_ID> --details
ac-check.pl --user EMAIL --pass PASS --device-id <APPLIANCE_ID> --lua

# Or via environment (same commands without --user/--pass)
export MIDEA_EMAIL='you@example.com'
export MIDEA_PASSWORD='...'
ac-check.pl --list
ac-check.pl --device-id <APPLIANCE_ID> --start --more
```

| Option | Description |
|--------|-------------|
| `--user EMAIL` | MSmartLife / SmartHome account email |
| `--pass PASS` | Account password |
| `--list` | List appliances |
| `--device-id ID` | Cloud appliance id (from `--list`) |
| `--lua` | Download this appliance's device-specific protocol lua script to `./` |
| `--start` | Start iCheck, wait for `historyId`, then `--details` |
| `--details` | Fault details (`history/detail` when `--history-id` is set) |
| `--more` | Full checkMore catalog (needs `historyId` from start or flag) |
| `--history` | History list |
| `--history-id ID` | Specific past run |
| `--region R` | `us` / `eu` / … (optional; omit to auto-probe) |
| `--poll SEC` | Wait budget while `historyId` is still `1` |
| `--debug` | Redacted request/response on stderr |

`--region` can be omitted: the script probes MAS hosts (US is tried first).

## Cloud + LAN V3 (`ac-cloud.pl`) — work in progress, not confirmed working

For Wi‑Fi modules that speak **LAN V3** (`0x8370`): the local session is locked
to a cloud-issued `token`/`key`, so the default AES key used by `ac.pl` is not
enough. This script logs into SmartHome, fetches those keys, and does the V3
handshake on port `6444` — that part (`0x8370` framing, AES-CBC, sha256
signing) is a standard, structurally complete implementation.

**`--get` is not actually usable yet**: it prints the raw response as hex
(`V3 response (N bytes): <hex>`) with no field decoding at all — no power,
mode, temperature, nothing parsed out, unlike `ac.pl`/`ac-ng.pl`. The status
query packet it sends (`v2_status_packet()`) also looks unfinished — its own
in-code comment admits uncertainty about the framing, and it's missing the
CRC8 trailing byte `ac.pl`'s own packets always have. There's no record of
this script ever being run against a real V3-locked unit. If you have one
and get it working (or fix the above), a PR would be very welcome.

If your unit already answers **V2** with the default key (most older / unbound
modules — check with `ac.pl --ip <addr> --get`), use **`ac.pl`** instead;
it's the one this project actually relies on day to day.

```sh
# List appliances
ac-cloud.pl --user EMAIL --pass PASS --list

# Fetch V3 token/key for an appliance (prints hex)
ac-cloud.pl --user EMAIL --pass PASS --device-id <APPLIANCE_ID> --token

# V3 handshake + status over LAN (cloud login to obtain keys)
ac-cloud.pl --ip 192.168.1.2 --device-id <APPLIANCE_ID> \
  --user EMAIL --pass PASS --get

# Same with keys already known (no login)
ac-cloud.pl --ip 192.168.1.2 --token-hex <TOKEN> --key-hex <KEY> --get

# Last-resort try of midealan preset keys (no cloud)
ac-cloud.pl --ip 192.168.1.2 --preset-keys --get
```

| Option | Description |
|--------|-------------|
| `--user EMAIL` / `--pass PASS` | Cloud credentials (or `MIDEA_EMAIL` / `MIDEA_PASSWORD`) |
| `--list` | List cloud appliances |
| `--device-id ID` | Appliance id (from `--list`) |
| `--token` | Print cloud V3 `token`/`key` hex for `--device-id` |
| `--ip HOST` | Device LAN address (port 6444) |
| `--get` | V3 authenticate + status query |
| `--token-hex` / `--key-hex` | Use known keys instead of fetching from cloud |
| `--preset-keys` | Try built-in midealan default keys (no cloud) |
| `--region R` | `us` / `eu` / … (optional; omit to auto-probe) |
| `--debug` | Redacted protocol dump on stderr |

## Build a script for your own AC

`ac.pl` drives every unit through one shared, hand-ported byte-offset
table. `ac-ng.pl` instead runs the exact protocol script Midea's own
cloud issues for *your* specific model — through a pure-Perl Lua 5.1
interpreter bundled with this project (`lua/PureLua/`) — so set/get
behavior matches what the official app actually does for that unit,
not a generic guess.

**`ac-00000Q1B.pl`** in this repo is one such script, already built —
a ready-to-run bundle for the **00000Q1B** AC model specifically. If
that's not your exact model (check the label on your unit, or run
`ac-check.pl --list` and look at the `sn` column), it won't be the
right protocol script for your device: build your own with the steps
below instead of assuming this one matches.

### Quick path: one command

```sh
export MIDEA_EMAIL='you@example.com'
export MIDEA_PASSWORD='...'

# Every 0xAC appliance on the account, one bundled script each
lua/download_and_bundle.pl --out ./bundled

# Just one appliance (id from: ac-check.pl --list)
lua/download_and_bundle.pl --out ./bundled --device-id <APPLIANCE_ID>

# Or pass credentials as flags instead of env vars
lua/download_and_bundle.pl --out ./bundled --user you@example.com --pass '...'
```

This logs in, downloads each appliance's own protocol script, and
writes a ready-to-run, self-contained `ac-<script-name>.pl` into
`./bundled/` for each — no other files needed, copy it anywhere with
Perl (a router, a NAS, …).

### Use the bundled script

Same `--get` / `--set` shape as `ac.pl`, but flag names come straight
from the device script's own field names, not `ac.pl`'s table:

```sh
./bundled/ac-<yourmodel>.pl --ip 192.168.1.2 --get
./bundled/ac-<yourmodel>.pl --ip 192.168.1.2 --set --power on --mode cool --temperature 24 --eco on
```

| Option | Values |
|--------|--------|
| `--power` | `on` / `off` |
| `--mode` | `auto` `cool` `dry` `heat` `fan` |
| `--temperature` | `16..30` (step 0.5) |
| `--wind_speed` | `auto` `high` `medium` `low` `silent`, or a raw number |
| `--eco` `--dry` `--ptc` `--tubro` (turbo) `--purifier` `--prevent_cold` `--power_saving` `--comfort_sleep` `--buzzer` `--wind_swing_lr` `--wind_swing_ud` … | `on` / `off` |

Run with `--help` for the full flag list (~35 boolean fields — same
names the device script itself uses). `--get` with no flags decodes
every field the device reports, including extended properties
(`security`, `self_clean`, `voice_control`, …) that `ac.pl --caps`
can't see.

### Doing it by hand

`download_and_bundle.pl` is just these two steps glued together, for
every appliance on the account if `--device-id` is omitted:

1. `ac-check.pl --device-id <ID> --lua` — downloads that appliance's
   actual protocol script (a `.lua` file) to the current directory.
   Needs credentials too, same as above — either
   `ac-check.pl --user you@example.com --pass '...' --device-id <ID> --lua`
   or `export MIDEA_EMAIL=... MIDEA_PASSWORD=...` first and drop
   `--user`/`--pass`.
2. `lua/bundle.pl <that .lua file> <output.pl>` — packs the engine,
   `ac-ng.pl`, and that script into one standalone file (no
   credentials needed, it only touches local files).

`ac-ng.pl` can also run straight off the downloaded `.lua` file without
bundling, e.g. while developing:

```sh
ac-ng.pl --ip 192.168.1.2 --lua T_..._2024013001.lua --get
```
