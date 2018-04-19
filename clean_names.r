clean_names <- compose(
	function(x) gsub("(_)(?=_*\\1)|_$", "", x, perl = T), # repeat "_" and end "_"
	function(x) gsub("\\W", "_", x), # not [A-Za-z0-9_] and replace with "_"
	function(x) gsub("\\(.+\\)", "", x),# parenthesis and its contents
	tolower)
