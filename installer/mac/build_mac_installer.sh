#!/bin/sh
set -eu

installer_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
root=$(CDPATH= cd -- "$installer_dir/../.." && pwd)
player_source="$root/DND APP Drachuri Edition Player_v2"
build=$(mktemp -d /private/tmp/drachuri-mac-build.XXXXXX)
trap 'rm -rf "$build"' EXIT INT TERM
cache="$installer_dir/cache"
release_dir="$root/releases"
version=$(tr -d '\r\n' < "$root/VERSION")
numeric_version=$(printf '%s' "$version" | sed 's/[^0-9.].*$//')
package_version="$numeric_version.2"
r_version="4.2.3"
r_pkg="$cache/R-$r_version-x86_64.pkg"
r_url="https://cran.r-project.org/bin/macosx/base/R-$r_version.pkg"
app="$build/root/Applications/Drachuri Player.app"
resources="$app/Contents/Resources"

mkdir -p "$resources/app" "$resources/library" "$app/Contents/MacOS" "$cache" "$release_dir"

echo "Copying the player application..."
rsync -a \
  --exclude '.Rproj.user' --exclude '.RData' --exclude '.Rhistory' --exclude '.DS_Store' \
  --exclude 'renv/library' --exclude 'renv/cache' --exclude 'renv/staging' --exclude 'renv_new' \
  --exclude 'launcher/bootstrap-library' --exclude 'launcher/logs' --exclude 'rsconnect' \
  --exclude 'www/models' \
  "$player_source/" "$resources/app/"

cp "$installer_dir/Info.plist" "$app/Contents/Info.plist"
cp "$installer_dir/Drachuri Player" "$app/Contents/MacOS/Drachuri Player"
cp "$root/installer/player/installed_run.R" "$resources/installed_run.R"
chmod 755 "$app/Contents/MacOS/Drachuri Player"

echo "Collecting the tested Intel Mac package library..."
R_LIBS_USER="$player_source/launcher/bootstrap-library" \
  RENV_CONFIG_AUTOLOADER_ENABLED=FALSE \
  Rscript --vanilla "$installer_dir/prepare_mac_library.R" "$player_source" "$resources/library"

echo "Creating Drachuri Player.app..."
/usr/libexec/PlistBuddy -c "Set :CFBundleShortVersionString $numeric_version" "$app/Contents/Info.plist"
find "$app" -name '._*' -delete
chmod -R a+rX "$app"
xattr -cr "$app"
codesign --force --deep --sign - "$app"
pkgbuild --root "$build/root" \
  --identifier com.kerrybrown.drachuri.player.pkg \
  --version "$package_version" \
  --install-location / \
  --component-plist "$installer_dir/component.plist" \
  "$build/DrachuriPlayerApp.pkg"

if [ ! -f "$r_pkg" ]; then
  echo "Downloading the official Intel R $r_version installer..."
  curl --fail --location "$r_url" --output "$r_pkg"
fi

echo "Combining R and Drachuri Player into one installer..."
pkgutil --expand "$r_pkg" "$build/R-expanded"
pkgutil --flatten "$build/R-expanded/R-fw.pkg" "$build/RFramework.pkg"
productbuild --synthesize \
  --package "$build/RFramework.pkg" \
  --package "$build/DrachuriPlayerApp.pkg" \
  "$build/Distribution.xml"
productbuild --distribution "$build/Distribution.xml" \
  --package-path "$cache" \
  --package-path "$build" \
  "$build/Drachuri-Player-$version.pkg"

dmg_root="$build/dmg"
mkdir -p "$dmg_root"
cp "$build/Drachuri-Player-$version.pkg" "$dmg_root/Install Drachuri Player.pkg"
cat > "$dmg_root/README.txt" <<'EOF'
Double-click “Install Drachuri Player.pkg”. The installer includes R and the game.
After installation, open Drachuri Player from Applications or Spotlight.

This private build is not notarized. If macOS blocks it, right-click the package,
choose Open, and confirm that you want to open it.
EOF

output="$release_dir/Drachuri-Player-Mac-$version.dmg"
rm -f "$output"
hdiutil create -volname "Drachuri Player" -srcfolder "$dmg_root" -format UDZO -ov "$output"
echo "Mac installer ready: $output"
