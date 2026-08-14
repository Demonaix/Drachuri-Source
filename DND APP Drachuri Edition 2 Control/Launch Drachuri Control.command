#!/bin/sh
set -eu

app_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
cd "$app_dir"

if command -v Rscript >/dev/null 2>&1; then
  rscript=$(command -v Rscript)
elif [ -x /Library/Frameworks/R.framework/Resources/bin/Rscript ]; then
  rscript=/Library/Frameworks/R.framework/Resources/bin/Rscript
elif [ -x /usr/local/bin/Rscript ]; then
  rscript=/usr/local/bin/Rscript
else
  printf '\n%s\n' \
    "R could not be found on this Mac." \
    "Install R from https://cran.r-project.org/bin/macosx/ and try again." \
    "RStudio is not required."
  printf '\nPress Return to close...'
  read answer
  exit 1
fi

printf '%s\n' "Starting Drachuri Control..."
"$rscript" --vanilla launcher/bootstrap_and_run.R

