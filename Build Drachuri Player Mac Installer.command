#!/bin/sh
cd "$(dirname "$0")" || exit 1
clear
echo "Building the Drachuri Player Mac installer..."
echo
sh installer/mac/build_mac_installer.sh
status=$?
echo
if [ "$status" -eq 0 ]; then
  echo "BUILD COMPLETE. The DMG is in the releases folder."
else
  echo "BUILD FAILED. Read the error above."
fi
echo
read -r -p "Press Return to close..." _
exit "$status"
