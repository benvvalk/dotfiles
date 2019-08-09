#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
#Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.

;; I disabled most of the shortcuts below because they interfered with normal shortcut
;; behaviours in bash under WSL. (Even though the shortcuts work great in most windows,
;; I was not successful at writing an exclusion rule for the special case of WSL/mintty
;; windows.)

; !<::
;   Send, {Home}
;   Return
; !>::
;   Send, {End}
;   Return
; ^a::
;   Send, {Home}
;   Return
; !b::
;   Return
; ^e::
;   Send, {End}
; !f::
;   Send, ^{Right}
;   Return
; ^h::
;   Send, {Backspace}
;   Return

^n::
  Send, {Down}
  Return
^p::
  Send, {Up}
  Return
