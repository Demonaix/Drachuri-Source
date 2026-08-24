# Drachuri Player Windows installer

Run `Build Drachuri Player Installer.cmd` from the repository root on a Windows 10/11 PC.
The first build downloads the official R runtime, restores the Windows packages from `renv.lock`,
installs Inno Setup through `winget` if necessary, and creates:

`releases/Drachuri-Player-Setup-<version>.exe`

That EXE is the only file players need. It installs per-user without administrator access, creates
normal Start Menu and optional Desktop shortcuts, bundles its own R, and never requires RStudio.

The installer is not digitally signed. Windows may therefore show an `Unknown publisher` warning.
Code signing can be added later if a trusted Windows signing certificate is purchased.

Do not build the installer on macOS: compiled R packages are operating-system specific. Re-run the
builder after changing `VERSION` or whenever a new player release is ready.
