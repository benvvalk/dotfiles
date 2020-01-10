#NoEnv
#Warn

SendMode Input
SetWorkingDir %A_ScriptDir% 

SetTitleMatchMode, 2 
SetTitleMatchMode, slow

GetMonitorRect(monitorIndex)
{
  SysGet, Monitor, Monitor, %monitorIndex%
  return [MonitorLeft, MonitorTop, MonitorRight, MonitorBottom]
}

GetCenterPoint(hWnd)
{
  WinGetPos, x, y, width, height, ahk_id %hWnd%

  centerX := x + width / 2
  centerY := y + height / 2

  return [centerX, centerY]
}

IsVisible(hWnd)
{
    WinGet, style, Style, ahk_id %hWnd%
    return style & 0x10000000
}

IsTitleEmpty(hWnd)
{
  WinGetTitle, title, ahk_id %hWnd%
  return title = ""
}

Contains(rect, point)
{
    x := point[1]
    y := point[2]
    minX := rect[1]
    minY := rect[2]
    maxX := rect[3]
    maxY := rect[4]

    return x >= minX && x <= maxX && y >= minY && y <= maxY
}

GetTopWindowOnMonitor(monitorIndex)
{
  rect:= GetMonitorRect(monitorIndex)
  
  WinGet, WinList, List
  Loop %WinList% {
      If (!IsVisible(WinList%A_Index%) || IsTitleEmpty(WinList%A_Index%)) {
          Continue
      }
      center := GetCenterPoint(WinList%A_Index%)
      If (Contains(rect, center)) {
          return WinList%A_Index%
      }
  }
}

GetMonitorIndex(hWnd)
{
  rect1 := GetMonitorRect(1)
  center := GetCenterPoint(hWnd)
  
  if (Contains(rect1, center)) {
    return 1
  } else {
    return 2
  }
}

GetActiveMonitorIndex()
{
  WinGet, hWnd, ID, A
  return GetMonitorIndex(hWnd)
}

GetOtherMonitorIndex()
{
  monitorIndex := GetActiveMonitorIndex()
  if (monitorIndex = 1) {
    return 2
  } else {
    return 1
  }
}

ActivateTopWindowOnMonitor(monitorIndex)
{
  hWnd := GetTopWindowOnMonitor(monitorIndex)
  WinActivate, ahk_id %hWnd%
}

MaximizeActiveWindowOnMonitor(monitorIndex)
{
  rect := GetMonitorRect(monitorIndex)
  x := rect[1]
  y := rect[2]
  width := rect[3] - rect[1]
  height := rect[4] - rect[2]
  WinRestore, A
  WinMove, A, , x, y, width, height
  WinMaximize, A
}

RaiseWindowByClass(className)
{
  WinGetPos, x, y, width, height, A
  WinActivate, ahk_class %className%
  WinRestore, A
  WinMove, A, , x, y, width, height
}

RaiseWindowByClassOtherMonitor(className)
{
  monitorIndex := GetOtherMonitorIndex()
  hWnd := GetTopWindowOnMonitor(monitorIndex)
  WinGetPos, x, y, width, height, ahk_id %hWnd%
  WinActivate, ahk_class %className%
  WinRestore, A
  WinMove, A, , x, y, width, height
}

ReadKeys()
{
  Input, keys, T1 C, {Tab}, 1,2,!,@,wf,of
  
  ; Note: AutoHotkey is supposed to support
  ; `switch` statements, but at the time of
  ; writing (version 1.1.30.01) they do not seem
  ; seem to recognized.
  
  if (keys = "1") {
    ActivateTopWindowOnMonitor(1)
  } else if (keys = "2") {
    ActivateTopWindowOnMonitor(2)
  } else if (keys = "!") {
    MaximizeActiveWindowOnMonitor(1)
  } else if (keys = "@") {
    MaximizeActiveWindowOnMonitor(2)
  } else if (keys = "wf") {
    RaiseWindowByClass("MozillaWindowClass")
  } else if (keys = "of") {
    RaiseWindowByClassOtherMonitor("MozillaWindowClass")
  }
}

^Space::
  ReadKeys()
  Return