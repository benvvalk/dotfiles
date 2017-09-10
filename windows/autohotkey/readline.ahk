#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
#Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.

!<::
  Send, {Home}
  Return
!>::
  Send, {End}
  Return
^a::
  Send, {Home}
  Return
!b::
  Return
^e::
  Send, {End}
!f::
  Send, ^{Right}
  Return
^h::
  Send, {Backspace}
  Return
^n::
  Send, {Down}
  Return
^p::
  Send, {Up}
  Return
^w::
  Send, !{Backspace}
  Return
