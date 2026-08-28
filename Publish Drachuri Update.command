#!/bin/sh
set -eu

root=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
current=$(tr -d '\r\n' < "$root/VERSION")
repository=$(tr -d '\r\n' < "$root/distribution/GITHUB_REPOSITORY")

printf 'Current version: %s\nNew version: ' "$current"
read -r version
[ -n "$version" ] || { echo "No version entered; nothing was published."; read -r _; exit 1; }

case "$repository" in
  */*) ;;
  *) printf 'GitHub release repository (owner/repository): '; read -r repository ;;
esac

printf 'Short release notes: '
read -r notes
[ -n "$notes" ] || notes="Drachuri improvements and fixes."

"$root/scripts/release_drachuri.sh" --version "$version" --repository "$repository" --notes "$notes"
git -C "$root" add VERSION distribution/GITHUB_REPOSITORY
if ! git -C "$root" diff --cached --quiet; then
  git -C "$root" commit -m "Release Drachuri $version"
fi
git -C "$root" push origin main
printf '\nRelease complete. Press Return to close.\n'
read -r _
