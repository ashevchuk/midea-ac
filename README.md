# midea-ac
midea air conditioning smart network device remote control (without m-smart cloud)

An attempt to reverse engineer the air conditioning control protocol of the manufacturer midea (potentially of other brands).
The script allows you to control the air conditioning smart device without using a m-smart cloud.

Usage:
 ./ac.pl --id 21354673567853 --ip 192.168.1.2 --set --power on --mode cool --temp 20

Options:
 --help            brief help message

 --id              device id.  ex: 21354673567853
 --ip              device address. ip address or host name

 --get             fetch device settings [default]
 --set             update device settings

 --power           turn power state: [on|off]
 --temp            set target temperature: [17..30]
 --mode            set operational mode: [auto|cool|dry|heat|fan]
 --fan             set fan speed: [auto|high|medium|low|silent]
 --turbo           turn turbo mode: [on|off]
 --swing           set swing mode: [off|vertical|horizontal|both]
 --eco             turn eco mode: [on|off]
 --buzzer          turn audible feedback: [on|off]

Perl dependencies:
 IO::Socket List::Util Digest::MD5 Digest::CRC Crypt::Mode::ECB Getopt::Long Pod::Usage
