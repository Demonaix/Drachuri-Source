# Drachuri releases

`GITHUB_REPOSITORY` is the one-time public GitHub destination in
`owner/repository` form. It is embedded into each installer so the installed
launcher can check the latest release without storing credentials.

Publish the installers with:

```sh
scripts/release_drachuri.sh --version 0.4.2 --notes "Short player-facing summary"
```

On a Mac, `Publish Drachuri Update.command` provides the same workflow as a
double-clickable prompt for the new version and release notes.

The command runs the full tests, builds Player and Control for macOS, creates
checksums and `drachuri-update.json`, and uploads only those release files
through GitHub CLI. The application source is not uploaded. If a matching Windows Player `.exe` is already
in `releases/`, it is included automatically. Until then its manifest entry is
`null`, so Windows remains reserved without advertising a missing download.

One-time setup requires GitHub authentication and a small public release
repository. A project-local GitHub CLI is used, so Homebrew is not required.
The repository can remain separate from the private application source:

```sh
.local-tools/gh_2.98.0_macOS_arm64/bin/gh auth login
```
