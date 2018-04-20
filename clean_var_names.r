clean_var_names <- compose(
	# remove repeat "_" and extreme "_"
	function(x) gsub("(_)(?=_*\\1)|^_|_$", "", x, perl = T), 
	# not [A-Za-z0-9_] and replace with "_"
	function(x) gsub("\\W", "_", x), 
	# parenthesis and its contents
	function(x) gsub("\\(.+\\)", "", x),
	tolower)
