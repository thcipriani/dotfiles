[[!meta title="Asus debian jessie install"]]
[[!meta date="Mon Jun 8 10:48:42 2015 -0600"]]

0. Reboot, hitting F2 over-and-over

1. Turn off Fastboot and Secureboot in UEFI

2. Download Non-free firmware

    * [Linux Questions Answer](http://www.linuxquestions.org/questions/debian-26/how-to-provide-non-free-firmware-files-to-the-debian-jessie-installer-4175542680/)
    * [firmware-iwlwifi download](https://packages.debian.org/jessie/all/firmware-iwlwifi/download)

3. Extract firmware from deb

    ar xv firmware-iwlwifi.deb
    tar xvf data.tar.xz

4. Copy to ext4 formatted sd-card

    mkfs.ext4 /dev/[sd-card]
    mount -t ext4 /dev/[sd-card] /mnt/sd-card
    cp -r lib/firmware/* /mnt/sd-card

5. Insert Install Media

6. Setup installer to recognize dmraid

    * [SataRaid DebianInstaller](https://wiki.debian.org/DebianInstaller/SataRaid)
    * Hit 'E' with 'Install' selected
    * append 'dmraid=true' to eol
    * F10 to save-and-exit


7. Detect network hardware

    * Ctrl-Alt F2
    * mkdir /lib/firmware
    * blkid (to list mounts)
    * mount -t ext4 /dev/sdd1 /lib/firmware
    * modprobe ahci
    * Ctrl-Alt F1 <yes>

9. Manual partition

    https://wiki.debian.org/DebianInstaller/SoftwareRaidRoot

8. Run live disk, chroot, fuck around

    http://forums.debian.net/viewtopic.php?f=17&t=116036


Troubleshooting:

https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=711147

- Show root partition:

      cat /proc/cmdline

- initramfs modules

    /etc/initramfs-tools/modules
    update-initramfs –k all –u

- Large xterm:

    xterm -fn 10x20

- chroot

    boot ubuntu live
    sudo -i
    cryptsetup luksOpen /dev/mapper/isw_cejababahfd_ASUS_03 cryptroot
    lvm vgscan
    lvm vgchange -ay
    mount /dev/mapper/taskmaster--vg-root /mnt
    mount /dev/mapper/isw_cejababahfd_ASUS_02 /mnt/boot
    mount -o bind /sys /mnt/sys
    mount -t proc /proc /mnt/proc
    mount -o bind /dev /mnt/dev
    chroot /mnt

- chroot & install dmraid

  chroot
  apt-get install dmraid
  initramfs modules

- break raid:

http://ubuntuforums.org/showthread.php?t=2193133&page=2&p=12913689#post12913689

# For realz Steps!

1. Download Ubuntu 14.04 Live OS and Debian 8.1.0 netinst
2. Copy to a Ubuntu USB Drive:

    * use lsblk/blkid to find USB drive name without number, e.g. /dev/sdb NOT /dev/sdb1
    * `dd bs=4M ~/Downloads/ubuntu-14.04.2-desktop-amd64.iso of=/dev/[usbdrive-without-number]`

3. Download Non-free firmware

    * [Linux Questions Answer](http://www.linuxquestions.org/questions/debian-26/how-to-provide-non-free-firmware-files-to-the-debian-jessie-installer-4175542680/)
    * [firmware-iwlwifi download](https://packages.debian.org/jessie/all/firmware-iwlwifi/download)

4. Extract firmware from deb

    * `ar xv firmware-iwlwifi.deb`
    * `tar xvf data.tar.xz`

5. Copy to ext4 formatted sd-card

    * `mkfs.ext4 /dev/[sd-card]`
    * `mount -t ext4 /dev/[sd-card] /mnt/sd-card`
    * `cp -r lib/firmware/* /mnt/sd-card`

6. Boot Ubuntu from usb/change UEFI opts

    * Insert Ubuntu USB drive in Asus, Boot while holding F2
    * Boot → Fastboot [Disabled]
    * Security → Secure Boot Menu → Secure Boot Control [Disabled]
    * Advanced → SATA Configuration → SATA Mode Selection [AHCI]
    * Boot → Boot Option Priorites (hit + until USB drive is at the top)
    * F10 (save and exit)

11. Try Ubuntu without installing
12. Enable network, start terminal (Start → uxterm → xterm -fn 10x20)
13. sudo gparted, delete all partitions on /dev/sda /dev/sdb
14. Create partition table on /dev/sdb:

    * Select /dev/sdb
    * Device → Create Partition Table…

15. Shutdown, remove usb
16. Copy Debian netinst to USB Drive:

    * use lsblk/blkid to find USB drive name without number, e.g. /dev/sdb NOT /dev/sdb1
    * `sudo dd bs=4M if=Downloads/debian-8.0.0-amd64-netinst.iso of=[usbdrive-without-number]`

17. Detect network hardware

    * Ctrl-Alt F2
    * mkdir /lib/firmware
    * blkid (to list mounts)
    * mount -t ext4 /dev/sdd1 /lib/firmware
    * modprobe ahci
    * Ctrl-Alt F1 <yes>

18. Partition Disks

    * Manual
      1. sda & sbd delete existing partitions
      2. sda1 256MB EFI System partition
      3. sda2 512MB mountat /boot ext4
      4. sda3 REMAINDER physical volume for raid
      5. sdb1 ALL physical volument for raid
      6. configure software raid, raid0 [x]sda3 [x]sdb1
      7. raid0 disk1 use as physical volume for encryption
      8. configure encrypted volumes [x]/dev/md0
      9. encrypted-disk 1: use as physical volume for lvm
      10. configure lvm volume group
      11. 20 GB / ; 4 GB swap ; remainder /home

11. Post xmonad-install disable GDM login:

    - [XMonad Install](https://github.com/thcipriani/TheSetup/blob/master/puppet/modules/haskell/manifests/debian.pp#L38-L50)
    - [Disable GDM systemd](https://github.com/thcipriani/TheSetup/blob/master/puppet/modules/haskell/manifests/debian.pp#L38-L50)
