^!g::
Loop, Parse, Clipboard, `n, `r
{
	query := A_LoopField

	; URL-encode the search query.
	encoded := StrReplace(query, " ", "+")
	
	; Note: In AutoHotkey, double-quote chars are escaped by using "".
	url := Format("""https://google.com/search?q={1}""", encoded)
	
	; The following works because the Windows "Run" dialog
	; recognizes URLs and opens them in the default browser.
	Run, %url%
	
	break
}