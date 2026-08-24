#ifndef AppVersion
  #define AppVersion "0.0.0"
#endif

[Setup]
AppId={{D47768AC-4529-4D0A-8538-F65EF12ADABC}
AppName=Drachuri Player
AppVersion={#AppVersion}
AppPublisher=Kerry Brown
DefaultDirName={localappdata}\Programs\Drachuri Player
DefaultGroupName=Drachuri Player
DisableProgramGroupPage=yes
PrivilegesRequired=lowest
ArchitecturesAllowed=x64compatible
ArchitecturesInstallIn64BitMode=x64compatible
MinVersion=10.0
OutputDir=..\..\releases
OutputBaseFilename=Drachuri-Player-Setup-{#AppVersion}
Compression=lzma2/ultra64
SolidCompression=yes
WizardStyle=modern
UninstallDisplayName=Drachuri Player
VersionInfoDescription=Drachuri Player Installer
ChangesAssociations=no
CloseApplications=yes
RestartApplications=no

[Files]
Source: "build\app\*"; DestDir: "{app}\app"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "build\runtime\R\*"; DestDir: "{app}\runtime\R"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "build\library\*"; DestDir: "{app}\library"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "installed_run.R"; DestDir: "{app}"; Flags: ignoreversion
Source: "Drachuri Player.cmd"; DestDir: "{app}"; Flags: ignoreversion
Source: "Drachuri Player.vbs"; DestDir: "{app}"; Flags: ignoreversion

[Icons]
Name: "{autoprograms}\Drachuri Player"; Filename: "{sys}\wscript.exe"; Parameters: """{app}\Drachuri Player.vbs"""; WorkingDir: "{app}"
Name: "{autodesktop}\Drachuri Player"; Filename: "{sys}\wscript.exe"; Parameters: """{app}\Drachuri Player.vbs"""; WorkingDir: "{app}"; Tasks: desktopicon

[Tasks]
Name: "desktopicon"; Description: "Create a desktop shortcut"; GroupDescription: "Shortcuts:"; Flags: checkedonce

[Run]
Filename: "{sys}\wscript.exe"; Parameters: """{app}\Drachuri Player.vbs"""; Description: "Launch Drachuri Player"; Flags: nowait postinstall skipifsilent
