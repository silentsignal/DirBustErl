DirBustErl
==========

This project aims to become a successor of DirBuster, which is slow, requires
Java, lacks active development, and is generally PITA. Erlang was chosen as a
platform to provide high-performance I/O, robustness, concurrency and
transparent distribution over multiple nodes.

License
-------

The whole project is available under MIT license, see `LICENSE.txt`.

Dependencies
------------

 - Recent Erlang distribution
 - `ibrowse` https://github.com/cmullaparthi/ibrowse

Erlang interface
----------------

There's currently one entry point:

	dirbusterl:bust(URL, Config)

 - `URL` is a string that is requested first (entry point)
 - `Config` can be a list of the following configuration parameters (if omitted, an empty one is assumed)

### Configuration parameters

 - `follow_dirs` recurse into subdirectories
 - `follow_redirs` follow HTTP redirections
 - `parse_body` parse HTML, CSS, robots.txt files for URLs
 - `{url_restriction, RE}` allows only such requests that match the regular expression `RE` supplied as a string, example: `{url_restriction, "^http://localhost/foo/"}
 - `{postfix, List}` append the elements of `List` one by one after each dictionary word, example: `{postfix, [".php", ".html"]}`
 - `{mangle_found, Rules}` for every file found, other files are requested by applying each item of the list `Rules`, the original filename will replace `\1`, example: `{mangle_found, ["\\1~", ".\\1.swp"]}` and some more ideas: https://code.google.com/p/fuzzdb/source/browse/trunk/discovery/FilenameBruteforce/Extensions.Backup.fuzz.txt
 - `{http_cfg, Params}` set `ibrowse` parameters (proxy, authentication) directly, example: `{http_cfg, [{proxy_host, "localhost"}, {proxy_port, 8081}]}`

TODO
----

 - Burp history import
 - nmap favicon DB hash detection
 - Etag and Last-Modified parsing
 - web interface
 - detection of case-insensitive behavior (Windows servers)
 - detection of duplicate files (Apache MultiViews, symbolic links, etc.)
 - multi-node support
 - content analysis mode when failed attempts come back as 200
 - custom HTTP headers
