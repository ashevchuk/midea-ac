# midea-ac
midea air conditioning smart network device remote control (without m-smart cloud)

an attempt to reverse engineer the air conditioning control protocol of the manufacturer midea (potentially of other brands).
allows you to control the air conditioning smart device without using a m-smart cloud.

## usage
 ```./ac.pl --id 21354673567853 --ip 192.168.1.2 --set --power on --mode cool --temp 20```

## options
- __--help__             *brief help message*

-  __--id__              *device id.  ex: 21354673567853*
-  __--ip__              *device address. ip address or host name*

-  __--get__             *fetch device settings*
-  __--set__             *update device settings*

-  __--power__           *turn power state: __[on|off]__*
-  __--temp__            *set target temperature: __[17..30]__*
-  __--mode__            *set operational mode: __[auto|cool|dry|heat|fan]__*
-  __--fan__             *set fan speed: __[auto|high|medium|low|silent]__*
-  __--turbo__           *turn turbo mode: __[on|off]__*
-  __--swing__           *set swing mode: __[off|vertical|horizontal|both]__*
-  __--eco__             *turn eco mode: __[on|off]__*
-  __--buzzer__          *turn audible feedback: __[on|off]__*

### Perl dependencies
- IO::Socket
- List::Util
- Digest::MD5
- Digest::CRC
- Crypt::Mode::ECB
- Getopt::Long
- Pod::Usage
