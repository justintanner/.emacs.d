#SingleInstance
#Installkeybdhook
#UseHook
SetKeyDelay 0

global activeChords := {}
global markActive := False

^a::
If !(Rule("MacroStartOfLine", "", ""))
  SendDefault()
Return

^b::
If !(Rule("^o", "x", "chrome.exe") || Rule("MacroBackChar","", ""))
  SendDefault()
return

^c::
If !(Rule("^c", "", "ConEmu64.exe"))
  Send %A_ThisHotkey%
StartChord("c")
return

^d::
If !(Rule("MacroDeleteChar", "", ""))
  SendDefault()
Return

^e::
If !(Rule("MacroEndOfLine", "", ""))
  SendDefault()
Return

^f::
If !(Rule("^l", "x", "chrome.exe") || Rule("^o", "x", "") || Rule("MacroForwardChar", "", ""))
  SendDefault()
Return

^g::
; Evernote is doing weird things on Escape, I don't know a way to unselect all text.
If !(Rule("^f", "x", "") || Rule("", "", "Evernote.exe") || Rule("MacroEscape", "", ""))
  SendDefault()
Return

^h::
If !(Rule("^a", "x", ""))
  SendDefault()
Return

^i::
If (IsEmacs())
  SendDefault()
Return

^j::
Send, {Ctrl up}{LWin}
Return

^k::
If !(Rule("MacroKillLine", "", ""))
  SendDefault()
Return

^l::
SendDefault()
Return

^m::
SendDefault()
Return

^n::
If !(Rule("MacroNextLine", "", ""))
  SendDefault()
Return

^o::
; This is not exactly open line
If !(Rule("MacroEnter", "", ""))
  SendDefault()
Return

^p::
If !(Rule("MacroPreviousLine", "", ""))
  SendDefault()
Return

^q::
if (IsEmacs())
  SendDefault()
Return

^r::
If !(Rule("MacroFind", "", ""))
  SendDefault()
Return

^s::
If !(Rule("^s", "x", "") || Rule("MacroFind", "", ""))
  SendDefault()
Return

^t::
Send !{Esc}
Return

^u::
If !(Rule("^z", "x", ""))
  SendDefault()
Return

^v::
If !(Rule("MacroPageDown", "", ""))
  SendDefault()
Return

^w::
If !(Rule("MacroCopyCut", "", "chrome.exe") || Rule("MacroCut", "", ""))
  SendDefault()
Return

^x::
StartChord("x")
If (IsEmacs())
  Send %A_ThisHotkey%
Return

^y::
If !(Rule("MacroPaste", "", ""))
  SendDefault()
Return

;; ALT Keys
!a::
Return

!b::
Return

!c::
Return

!d::
Return

!e::
Return

!f::
If !(Rule("MacroJumpWordRight", "", ""))
  SendDefault()
Return

!g::
If (IsEmacs())
  SendDefault()
Return

!h::
Return

!i::
Return

!j::
Return

!k::
Return

!l::
Return

!m::
If !(Rule("MacroCopyCut", "", "chrome") || Rule("MacroCopy", "", ""))
  SendDefault()
Return

!n::
If !(Rule("^t", "", "chrome.exe") || Rule("MacroNew", "", ""))
  SendDefault()
Return

!o::
Return

!p::
If (IsEmacs())
  SendDefault()
Return

!q::
Return

!r::
Return

!s::
Return

!t::
Return

!u::
Return

!v::
If !(Rule("MacroPageUp", "", ""))
  SendDefault()
Return

!w::
If !(Rule("MacroCopy", "", ""))
  SendDefault()
Return

!x::
Return

!y::
If !(Rule("MacroPaste", "", ""))
  SendDefault()
Return

!z::
Return

;; Misc Keys
!+,::
If !(Rule("MacroDocumentHome", "", ""))
  SendDefault()
Return

!+.::
If !(Rule("MacroDocumentEnd", "", ""))
  SendDefault()
Return

$^Space::
markActive := !markActive
If (IsEmacs())
{
  Send ^{Space}
}
Return

; Note: ~$ commands will let input passthrough without capturing it.
~$Enter::
markActive := False
Return

~$Ctrl UP::
ClearActiveChords()
markActive := False
Return

MacroSkip()
{
  Return
}

MacroEnter()
{
  SendAndClearMark("{Enter}")
}

MacroCopy()
{
  SendAndClearMark("^c")
}

MacroCopyCut()
{
  SendAndClearMark("^x^c")
}

MacroPaste()
{
  SendAndClearMark("^v")
}

MacroCut()
{
  SendAndClearMark("^x")
}

MacroFind()
{
  SendAndClearMark("^f")
}

MacroEscape()
{
  SendAndClearMark("{ESC}")
}

MacroDeleteChar()
{
  SendAndClearMark("{Del}")
}

MacroKillWord()
{
  SendAndClearMark("^{Backspace}")
}

MacroNew()
{
  SendAndClearMark("^n")
}

MacroSwitchApps()
{
  SendAndClearMark("!{Tab}")
}

MacroJumpWordRight()
{
  SendMarkSensitive("^{Right}")
}

MacroBackChar()
{
  SendMarkSensitive("{Left}")
}

MacroForwardChar()
{
  SendMarkSensitive("{Right}")
}

MacroNextLine()
{
  SendMarkSensitive("{Down}")
}

MacroPreviousLine()
{
  SendMarkSensitive("{Up}")
}

MacroEndOfLine()
{
  SendMarkSensitive("{End}")
}

MacroStartOfLine()
{
  SendMarkSensitive("{Home}")
}

MacroPageUp()
{
  SendMarkSensitive("{PgUp}")
}

MacroPageDown()
{
  SendMarkSensitive("{PgDn}")
}

MacroDocumentHome()
{
  SendMarkSensitive("^{Home}")
}

MacroDocumentEnd()
{
  SendMarkSensitive("^{End}")
}

MacroKillLine()
{
  Send {ShiftDown}{END}{ShiftUp}
  Sleep 50
  Send ^x
  ; Can't detect an empty line!... hmm
  ;Send {Del}
  markActive := False
}

SendAndClearMark(key)
{
  IfInString, key, "{"
  {
    stripedKey := StripCurlies(key)
    Send +{%stripedKey%}
  }
  Else
  {
    Send %key%
  }
  markActive := False
}

SendMarkSensitive(key)
{
  If (markActive)
  {
    IfInString, key, "{"
    {
      stripedKey := StripCurlies(key)
      Send +{%stripedKey%}
    }
    Else
    {
      Send +%key%
    }
  }
  Else
  {
    IfInString, key, "{"
    {
      stripedKey := StripCurlies(key)
      Send {%stripedKey%}
    }
    Else
    {
      Send %key%
    }
  }
}

ClearActiveChords()
{
  For key, value in activeChords
    activeChords[key] := False
  Return
}

ChordTimer()
{
  ClearActiveChords()
  Return
}

StartChord(key)
{
  SetTimer, ChordTimer, -750
  activeChords[key] := True

  Return
}

IsProgram(classOrExec)
{
  IfWinActive, ahk_class %classOrExec%
  {
    Return 1
  }
  IfWinActive, ahk_exe %classOrExec%
  {
    Return 1
  }

  Return 0
}

IsEmacs()
{
  Return (IsProgram("Emacs") || IsProgram("rubymine64.exe") || IsProgram("ConEmu64.exe"))
}

Rule(key, withChord := "", withProgram := "")
{
  If (IsEmacs())
  {
    Return 0
  }

  If ((withProgram != "") && !IsProgram(withProgram))
  {
    Return 0
  }

  If ((withChord != "") && !activeChords[withChord])
  {
    Return 0
  }

  IfInString, key, "{"
  {
    stripedKey := StripCurlies(key)
    Send {%stripedKey%}
  }
  Else If IsFunc(key)
  {
    %key%()
  }
  Else
  {
    Send %key%
  }

  ClearActiveChords()

  Return 1
}

StripCurlies(string)
{
  striped := StrReplace(string, "{", "")
  striped := StrReplace(striped, "}", "")
  Return striped
}

SendDefault()
{
  Send %A_ThisHotkey%
  ClearActiveChords()

  Return
}
