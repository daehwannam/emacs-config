#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.


; --------------------- CONFIG ---------------------
; [OriginKey::DestinationKey]

; [Ctrl CapsLock]
$Ctrl::CapsLock
$CapsLock::Ctrl

; ; [Ctrl Shift CapsLock]
; $Ctrl::CapsLock
; $LShift::Ctrl
; $CapsLock::Shift


; ; [Ctrl Alt CapsLock]
; $Ctrl::CapsLock
; $Alt::Ctrl
; $CapsLock::Alt


; ; [CapsLock ==> Ctrl / Ctrl & Alt ==> CapsLock]
; ; https://autohotkey.com/board/topic/119764-toggle-caps-lock-when-enter-is-pressed/
; $CapsLock::Ctrl
; Ctrl & Alt::SetCapsLockState, % (t:=!t) ?  "On" :  "Off"
; Alt & Ctrl::SetCapsLockState, % (t:=!t) ?  "On" :  "Off"
