# midea-ac
midea air conditioning smart network device remote control (without m-smart cloud)

an attempt to reverse engineer the air conditioning control protocol of the manufacturer midea (potentially of other brands. it is possible to identify such a device by using "NetHome" software).
allows you to control the air conditioning smart device without using a m-smart cloud.

## synopsis

 ```./ac.pl --ip 192.168.1.2 --set --power on --mode cool --temp 20```
 
- __--help__             *brief help message*

-  __--ip__              *device IP address or host name*

-  __--get__             *fetch device settings*
-  __--set__             *update device settings*

-  __--discover__        *searches for compatible devices*

-  __--power__           *turn power state: __[on|off]__*
-  __--temp__            *set target temperature: __[16..30]__*
-  __--mode__            *set operational mode: __[auto|cool|dry|heat|fan]__*
-  __--fan__             *set fan speed: __[auto|high|medium|low|silent]__*
-  __--turbo__           *turn turbo mode: __[on|off]__*
-  __--swing__           *set swing mode: __[off|vertical|horizontal|both]__*
-  __--eco__             *turn eco mode: __[on|off]__*
-  __--buzzer__          *turn audible feedback: __[on|off]__*

-  __--value__           *output of values alone*
-  __--begin__           *beginning of the output string __[default: none]__*
-  __--end__             *end of the output string __[default: "\n"]__*
-  __--quote__           *use quotes __[default: none]__*
-  __--unquote_num__     *do not use quotes for numbers*
-  __--separator__       *field separator __[default: ":"]__*
-  __--delimiter__       *fields delimiter __[default: "\n"]__*

-  __--exit__            *exit code __0__ if value __ON__, else exit code __1__ __[eco|led|error|turbo|buzzer|unit|power]__*

## options

### --help
Print a brief help message and exits.

### --ip
IP address or host name of the device

### --get
The operation of reading device parameters
By default, all fields will be displayed

For example, you can display only one field (name with value) using
the following arguments:

```...--get --temp```

Or display only the requested fields using the following arguments:

```...--get --temp --mode --power```

Or display only the value of the desired field, using the following
arguments:

```...--get --power --value```

### --set
The operation of writing device parameters

For example, you can turn on the device using the following
arguments and their values:

```...--set --power on```

Or turn off the device using the following arguments and their
values:

```...--set --power off```

Or at the same time as the device is turned on, set the parameters
and modes of its operation, using the following arguments and their
values:

```...--set --power on --mode cool --swing off --fan auto --temp 20```

Or simply change one of the parameters using the following arguments
and their values:

```...--set --temp 17```

### --power
The parameter controls the power state of the device

It can take one of the following values: __[on|off]__

### --temp
The parameter controls the target temperature that the device will try to reach

It can take positive integer values from this range: __[16..30]__

### --mode
The parameter controls the operation mode of the device

It can take one of the following values: __[auto|cool|dry|heat|fan]__

### --fan
The parameter controls the operation mode of the device blower fan

It can take one of the following values: __[auto|high|medium|low|silent]__

### --turbo
The parameter controls the turbo mode of the device

It can take one of the following values: __[on|off]__

### --swing
The parameter controls the operation mode of the blinds of the
device

It can take one of the following values: __[off|vertical|horizontal|both]__

### --eco
The parameter controls the eco mode of the device

It can take one of the following values: __[on|off]__

### --buzzer
The parameter controls the sound response mode of the device

It can take one of the following values: __[on|off]__

### --value
Enables output of values alone, without field names

To output a simple array of values in JSON notation, use the
following combination of arguments and their values:

```...--begin "[" --end "]" --delimiter "," --quote "\"" --unquote_num --value```

### --begin
The string to be put at the beginning of the output

### --end
The string to be put at the end of the output

### --quote
Specifies the string that will be used at the beginning and at the
end of each field name and the value itself individually

### --unquote_num
Cancels the string that will be used at the beginning and at the end
of each value if the value itself is a number

To display the output as formatted JSON, use the following
combination of arguments and their values:

```...--begin "{\n\t" --end "\n}\n" --delimiter ",\n\t" --quote "\"" --unquote_num```

For unformatted JSON output, use the following combination of
arguments and their values:

```...--begin "{" --end "}" --delimiter "," --quote "\"" --unquote_num```

### --separator
The separator to be used between the name of the field and its value (tupple)

### --delimiter
Separator to be used between tuples

### --exit
The exit code will be used: if the specified parameter is set to __ON__, then the exit code will be __0__, otherwise, the exit code will be __1__

It can take one of the following values: __[eco|led|error|turbo|buzzer|unit|power]__

To check the power status of a device when used in third party scripts, use the following combination of arguments and their values:

```...--exit power && echo "ac is on"```

or

```...--exit power || echo "ac is off"```

### --discover
Searches for compatible devices on the specified network

The network can take values in the form of:

single IP address:
```...--discover --ip 192.168.1.1```

network with mask:
```...--discover --ip 192.168.1.0/255.255.255.0```

network with mask length:
```...--discover --ip 192.168.1.0/24```

range of IP addresses:
```...--discover --ip 192.168.1.1-192.168.1.8```

## Dependencies
### CPAN modules
- Crypt::Mode::ECB (or ```openssl``` binary. selection will be made automatically depending on the availability of one of the options)

### Perl core modules
- IO
- POSIX
- List::Util
- Scalar::Util
- Digest::MD5
- Getopt::Long
- Pod::Usage

ac_nodeps.pl - is an experimental script without any perl dependencies, it uses only "standard" utilities: nc, xxd, md5sum and also openssl
