# midea-ac

Command-line tools for **Midea M-Smart** air conditioners:

- **`ac.pl`** — control directly over LAN (no cloud); V2 default-key modules
- **`ac-check.pl`** — cloud iCheck / diagnostics (MSmartLife overseas)
- **`ac-cloud.pl`** — cloud login + LAN V3 (`0x8370`) token handshake / status

LAN mode is potentially compatible with other brands sharing the same M-Smart V3
protocol (Midea, Idea, Bek, Electrolux, Carrier, Toshiba, Neoclima, etc.). You can identify such devices
using the "NetHome Plus" app.

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
| `--start` | Start iCheck, wait for `historyId`, then `--details` |
| `--details` | Fault details (`history/detail` when `--history-id` is set) |
| `--more` | Full checkMore catalog (needs `historyId` from start or flag) |
| `--history` | History list |
| `--history-id ID` | Specific past run |
| `--region R` | `us` / `eu` / … (optional; omit to auto-probe) |
| `--poll SEC` | Wait budget while `historyId` is still `1` |
| `--debug` | Redacted request/response on stderr |

`--region` can be omitted: the script probes MAS hosts (US is tried first).

## Cloud + LAN V3 (`ac-cloud.pl`)

For Wi‑Fi modules that speak **LAN V3** (`0x8370`): the local session is locked
to a cloud-issued `token`/`key`, so the default AES key used by `ac.pl` is not
enough. This script logs into SmartHome, fetches those keys, does the V3
handshake on port `6444`, and can query status (`--get`).

If your unit already answers **V2** with the default key (most older / unbound
modules), use **`ac.pl`** instead — `ac-cloud.pl` will not replace day-to-day
control for those devices.

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
