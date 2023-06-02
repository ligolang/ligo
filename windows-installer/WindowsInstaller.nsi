!include "MUI2.nsh"
!include WinMessages.nsh

; General

!define MUI_ICON "favicon.ico"

; Name and file
Name "LIGO"
OutFile "ligo_installer.exe"
Unicode True

; Default installation folder
InstallDir $PROGRAMFILES32\ligo

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

  EnVar::Check "Path" "NULL"
  Pop $0
  DetailPrint "EnVar::Check write access HKCU returned=|$0|"

  EnVar::SetHKCU
  EnVar::Check "Path" "$INSTDIR"
  Pop $0
  ${If} $0 = 0
    DetailPrint "Already in Path"
  ${Else}
    EnVar::AddValue "Path" "$INSTDIR"
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
  RMDir /r "$INSTDIR"
  EnVar::SetHKCU
  EnVar::DeleteValue "Path" "$INSTDIR"
  EnVar::Update "HKCU" "Path"
  SendMessage ${HWND_BROADCAST} ${WM_WININICHANGE} 0 "STR:Environment" /TIMEOUT=5000

  DeleteRegKey /ifempty HKCU "Software\LIGO"
SectionEnd
