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

-  __--get__             *fetch device settings [default]*
-  __--set__             *update device settings*

-  __--power__           *turn power state: [on|off]*
-  __--temp__            *set target temperature: [17..30]*
-  __--mode__            *set operational mode: [auto|cool|dry|heat|fan]*
-  __--fan__             *set fan speed: [auto|high|medium|low|silent]*
-  __--turbo__           *turn turbo mode: [on|off]*
-  __--swing__           *set swing mode: [off|vertical|horizontal|both]*
-  __--eco__             *turn eco mode: [on|off]*
-  __--buzzer__          *turn audible feedback: [on|off]*

### Perl dependencies
- IO::Socket
- List::Util
- Digest::MD5
- Digest::CRC
- Crypt::Mode::ECB
- Getopt::Long
- Pod::Usage
