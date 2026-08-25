#!/bin/sh
set -eu
installer_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
root=$(CDPATH= cd -- "$installer_dir/../../.." && pwd)
control_source="$root/DND APP Drachuri Edition 2 Control"
player_source="$root/DND APP Drachuri Edition Player_v2"
build=$(mktemp -d /private/tmp/drachuri-control-build.XXXXXX)
trap 'rm -rf "$build"' EXIT INT TERM
cache="$root/installer/mac/cache"
release_dir="$root/releases"
version=$(tr -d '\r\n' < "$root/VERSION")
numeric_version=$(printf '%s' "$version" | sed 's/[^0-9.].*$//')
r_pkg="$cache/R-4.2.3-x86_64.pkg"
app="$build/root/Applications/Drachuri Control.app"
resources="$app/Contents/Resources"
mkdir -p "$resources/workspace" "$resources/library" "$app/Contents/MacOS" "$release_dir"
common_excludes="--exclude=.Rproj.user --exclude=.RData --exclude=.Rhistory --exclude=.DS_Store --exclude=renv/library --exclude=renv/cache --exclude=renv/staging --exclude=launcher/bootstrap-library --exclude=launcher/logs --exclude=rsconnect"
rsync -a $common_excludes "$control_source/" "$resources/workspace/DND APP Drachuri Edition 2 Control/"
rsync -a $common_excludes --exclude=www/models --exclude=www/audio "$player_source/" "$resources/workspace/DND APP Drachuri Edition Player_v2/"
cp "$installer_dir/Info.plist" "$app/Contents/Info.plist"
cp "$installer_dir/DrachuriControl.icns" "$resources/DrachuriControl.icns"
cp "$installer_dir/Drachuri Control" "$app/Contents/MacOS/Drachuri Control"
cp "$root/installer/player/installed_run.R" "$resources/installed_run.R"
chmod 755 "$app/Contents/MacOS/Drachuri Control"
R_LIBS_USER="$control_source/launcher/bootstrap-library:$player_source/launcher/bootstrap-library" RENV_CONFIG_AUTOLOADER_ENABLED=FALSE Rscript --vanilla "$root/installer/mac/prepare_mac_library.R" "$control_source" "$resources/library"
/usr/libexec/PlistBuddy -c "Set :CFBundleShortVersionString $numeric_version" "$app/Contents/Info.plist"
find "$app" -name '._*' -delete
chmod -R a+rX "$app"; xattr -cr "$app"; codesign --force --deep --sign - "$app"
pkgbuild --root "$build/root" --identifier com.kerrybrown.drachuri.control.pkg --version "$numeric_version.5" --install-location / --component-plist "$installer_dir/component.plist" "$build/DrachuriControlApp.pkg"
pkgutil --expand "$r_pkg" "$build/R-expanded"
pkgutil --flatten "$build/R-expanded/R-fw.pkg" "$build/RFramework.pkg"
productbuild --synthesize --package "$build/RFramework.pkg" --package "$build/DrachuriControlApp.pkg" "$build/Distribution.xml"
productbuild --distribution "$build/Distribution.xml" --package-path "$cache" --package-path "$build" "$build/Drachuri-Control-$version.pkg"
mkdir "$build/dmg"; cp "$build/Drachuri-Control-$version.pkg" "$build/dmg/Install Drachuri Control.pkg"
printf '%s\n' 'Install the package, then open Drachuri Control from Applications or Spotlight.' > "$build/dmg/README.txt"
output="$release_dir/Drachuri-Control-Mac-$version.dmg"; rm -f "$output"
attempt=1
created_dmg=false
while ! hdiutil create -volname "Drachuri Control" -srcfolder "$build/dmg" -format UDZO -ov "$output"; do
  [ "$attempt" -ge 3 ] && { cp "$build/Drachuri-Control-$version.pkg" "$release_dir/Drachuri-Control-Mac-$version.pkg"; echo "DMG creation failed; package saved instead."; break; }
  attempt=$((attempt + 1))
  sleep 2
done
[ -f "$output" ] && created_dmg=true
if $created_dmg; then echo "Control installer ready: $output"; else echo "Control installer ready: $release_dir/Drachuri-Control-Mac-$version.pkg"; fi
