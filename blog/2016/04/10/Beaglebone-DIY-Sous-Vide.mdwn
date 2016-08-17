[Sous Vide](https://en.wikipedia.org/wiki/Sous-vide) is a method of
cooking in which food is vaccum-sealed and then left to cook in a
temperature-controlled water bath for a long period.

Folks like
[Heston Blumenthal](http://www.seriouseats.com/2009/10/understanding-sous-vide-cooking-heston-blumenthal.html)
and [J. Kenji López-Alt](http://www.seriouseats.com/user/profile/Goodeaterkenji)
have talked about and written quite a bit on the subject.

I recently ran accross a tutorial on
[Adafruit](https://learn.adafruit.com/sous-vide-powered-by-arduino-the-sous-viduino/sous-vide)'s
site where they built a sous-vide machine using an
[Arduino Uno](https://www.adafruit.com/product/50),
a [DS18B20](https://www.adafruit.com/product/381) 1Wire Temp probe,
and a [PowerTail](https://www.adafruit.com/product/268) switch.

I had *most* of these parts lying around; however, while I didn't have a
spare Arduino Uno, I *did* have a spare
[Beaglebone Black Rev B](https://www.adafruit.com/products/1278).
Beaglenbone Blacks (BBBs) are neat little machines:

* 1GHz Arm processor, 512MB RAM, 2GB eMMC flash storage
* [Open source Hardware](http://elinux.org/Beagleboard:BeagleBoneBlack#Terms_of_Use):
  All design materials released under Creative Commons Attribution-Share
  Alike 3.0 license
* Runs full Debian (in the case of RevB:
  [Debian 7.5](https://beagleboard.org/latest-images), but for a
  sous-vide cooker, that seems adequate)

This post chronicals my afternoon of making a sous-vide cooker
with a beaglebone.

Materials
---
* [BBB RevB](https://www.adafruit.com/products/1278)
* [PowerTail](https://www.adafruit.com/product/268)
* [Water-Proofed DS18B20](https://www.adafruit.com/product/381)
* [Food-Grade Heat-Shrink](https://www.adafruit.com/product/1020)
* [USB-to-TTL Serial Connector](https://www.adafruit.com/products/954)
* [Half-sized Breadboard](https://www.adafruit.com/products/64)
* [Jumper Wires](https://www.adafruit.com/products/153)
* Gumption.

Flash eMMC with latest, available Debian image
---

The first thing to do, is to setup the BBB to do *anything*. By default
the BBB RevC comes with Debian already flashed to its eMMC; however, the
RevB comes with an esoteric variety of Linux known "Ångström". Since I'm
happy using Debian, I decided to flash the 2GB eMMC with the latest Debian
release I could find.

Flashing the BBB was actually kind of a pain, surprisingly. The newer
images evidenly make this pretty trivial
(simply editing a `/boot/uEnv.txt` file), but
the older Debian image did not make this too clear.

Since the RevB only has 2GB eMMC, I grabbed the latest image that would
fit onto 2GB: Debian 7.5 eMMC flasher release 2014-05-14. Then I extracted
the `xz` file to disk.

```
curl -sLO https://debian.beagleboard.org/images/BBB-eMMC-flasher-debian-7.5-2014-05-14-2gb.img.xz
unxz BBB-eMMC-flasher-debian-7.5-2014-05-14-2gb.img.xz
```

I then copied the extracted disk image to a microSD card I had around.

```
sudo dd bs=4M if=BBB-eMMC-flasher-debian-7.5-2014-05-14-2gb.img of=/dev/mmcblk0 conv=fsync
```

Once that process completed, I hooked up the USB-to-TTL serial cable to
my laptop running Debian 8 using the J1 serial port-headers on the BBB.

* Connect *Black Wire* to *Pin 1* (closest to 5v barrel)
* Connect *Green Wire* to *Pin 4* (2 pins from pin 1)
* Connect *White Wire* to *Pin 5*

Once I had the serial connector attached to the BBB, I plugged the USB cable
into my computer. After checking `lsusb` and `dmesg` I noticed that the
new usb device was attached at `/dev/ttyUSB0`. Using that information, I
opened a screen session with the baud-rate set to 115,200, the rate necessary
for the BBB serial terminal:

```
screen /dev/ttyUSB0 115200
```

then I put the formatted microSD into the BBB, held down the S2 button
on the BBB, plugged in USB-power, and waited. And waited. And...wait,
why is there a login prompt, I thought I was flashing the eMMC?

All of the posts on the internet seem to suggest that you should see a
cylon-type pattern of indicator lights 
[[!meta date="2016-04-10"]][[!meta author="Tyler Cipriani"]][[!meta license="""
[[Creative Commons Attribution-ShareAlike License|https://creativecommons.org/licenses/by-sa/4.0/]]
"""]][[!meta copyright="""
Copyright &copy; 2016 Tyler Cipriani
"""]][[!meta title="Beaglebone-DIY-Sous-Vide"]]
