DirBustErl
==========

Dependencies
------------

 - Recent Erlang distribution
 - ibrowse https://github.com/cmullaparthi/ibrowse

Erlang interface
----------------

	dirbusterl:bust(URL, Config)

 - `URL` is a string that is requested first (entry point)
 - `Config` can be a list of the following configuration parameters

### Configuration parameters

 - `follow_dirs` recurse into subdirectories
 - `follow_redirs` follow HTTP redirections
 - `parse_body` parse HTML, CSS, robots.txt files for URLs
 - `{url_restriction, RE}` allows only such requests that match the regular expression `RE` supplied as a string, example: `{url_restriction, "^http://localhost/foo/"}
 - `{postfix, List}` append the elements of `List` one by one after each dictionary word, example: `{postfix, [".php", ".html"]}`
 - `{mangle_found, Rules}` for every file found, other files are requested by applying each item of the list `Rules`, the original filename will replace `\1`, example: `{mangle_found, ["\\1~", ".\\1.swp"]}` and some more ideas: https://code.google.com/p/fuzzdb/source/browse/trunk/discovery/FilenameBruteforce/Extensions.Backup.fuzz.txt
