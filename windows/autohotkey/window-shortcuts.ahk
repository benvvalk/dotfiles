; Recommended for performance and compatibility with future AutoHotkey releases.
#NoEnv
; Enable warnings to assist with detecting common errors.
#Warn
; Recommended for new scripts due to its superior speed and reliability.
SendMode Input
; Ensures a consistent starting directory.
SetWorkingDir %A_ScriptDir% 

; Allow substring matches on window title/class/exe for WinActivate
SetTitleMatchMode, 2 
; Slow and thorough search for windows with WinActivate
SetTitleMatchMode, slow

MaximizeOnMonitor(monitorNumber)
{
  SysGet, Display_, Monitor, %monitorNumber%
  Width := Display_Right - Display_Left
  Height := Display_Bottom - Display_Top
  WinMove, A, , %Display_Left%, %Display_Top%, %Width%, %Height%
  WinMaximize, A
}

;; Maximize and focus apps on left monitor

^F1::
  WinActivate, ahk_exe Code.exe
  MaximizeOnMonitor(1)
  Return
^F2::
  WinActivate, ahk_class mintty
  MaximizeOnMonitor(1)
  Return
^F3::
  WinActivate, ahk_exe Unity.exe
  MaximizeOnMonitor(1)
  Return
^F4::
  WinActivate, ahk_class MozillaWindowClass
  MaximizeOnMonitor(1)
  Return

;; Maximize and focus apps on right monitor

^F5::
  WinActivate, ahk_exe Code.exe
  MaximizeOnMonitor(2)
  Return
^F6::
  WinActivate, ahk_class mintty
  MaximizeOnMonitor(2)
  Return
^F7::
  WinActivate, ahk_exe Unity.exe
  MaximizeOnMonitor(2)
  Return
^F8::
  WinActivate, ahk_class MozillaWindowClass
  MaximizeOnMonitor(2)
  Return