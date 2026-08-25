# Drachuri Player Mac installer

Double-click `Build Drachuri Player Mac Installer.command` on the development Mac. It creates:

`releases/Drachuri-Player-Mac-<version>.dmg`

The DMG contains one combined installer package. It installs the tested Intel R 4.2 runtime and
`Drachuri Player.app`; Intel Macs run it directly and Apple Silicon Macs run it through Rosetta.
RStudio is not needed.

This private build is ad-hoc signed but not Developer ID signed or Apple-notarized. Recipients may
need to right-click the installer and choose Open. A paid Apple Developer certificate can remove
that warning later.
