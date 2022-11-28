!include "MUI2.nsh"
!include WinMessages.nsh

; General

!define MUI_ICON "favicon.ico"

; Name and file
Name "LIGO"
OutFile "ligo_installer.exe"
Unicode True

; Default installation folder
InstallDir $Profile\ligo

!define MUI_ABORTWARNING

; Pages

!insertmacro MUI_PAGE_LICENSE "LICENSE.md"
; We could also install esy here maybe?
;!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES

!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

; Languages
 
!insertmacro MUI_LANGUAGE "English"

 
; default section start
Section "Installation" SecInst
  # define output path
  SetOutPath $INSTDIR
  # specify file to go in output path
  File /r *

  # Run postinstall.js
  System::Call 'Kernel32::SetEnvironmentVariable(t, t)i ("OCAML_VERSION", "n.00.0000").r0'
  System::Call 'Kernel32::SetEnvironmentVariable(t, t)i ("OCAML_PKG_NAME", "ocaml").r0'
  System::Call 'Kernel32::SetEnvironmentVariable(t, t)i ("ESY_RELEASE_REWRITE_PREFIX", "true").r0'
  StrCmp $0 0 error
  # ExecWait ProgThatReadsEnv.exe
  nsExec::Exec 'node $INSTDIR\esyInstallRelease.js'
  Goto done
  error:
    MessageBox MB_OK "Can't set environment variable"
  done:

  EnVar::Check "Path" "NULL"
  Pop $0
  DetailPrint "EnVar::Check write access HKCU returned=|$0|"

  EnVar::SetHKCU
  EnVar::Check "Path" "$INSTDIR\bin"
  Pop $0
  ${If} $0 = 0
    DetailPrint "Already in Path"
  ${Else}
    EnVar::AddValue "Path" "$INSTDIR\bin"
    EnVar::Update "HKCU" "Path"
    Pop $0 ; 0 on success
    DetailPrint "Added to Path"
  ${EndIf}
  
  SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=5000

  ;Store installation folder
  WriteRegStr HKCU "Software\LIGO" "" $INSTDIR
  # define uninstaller name
  WriteUninstaller $INSTDIR\ligo_uninstaller.exe
# default section end
SectionEnd
 
# create a section to define what the uninstaller does.
# the section will always be named "Uninstall"
Section "Uninstall" SecUninst
  # Delete the uninstaller
  Delete "$INSTDIR\ligo_uninstaller.exe"
  # Delete the directory
  RMDir "$INSTDIR"
  EnVar::SetHKCU
  EnVar::DeleteValue "Path" "$INSTDIR\bin"
  EnVar::Update "HKCU" "Path"
  SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=5000

  DeleteRegKey /ifempty HKCU "Software\LIGO"
SectionEnd
