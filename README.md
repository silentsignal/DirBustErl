DirBustErl
==========

This project aims to become a successor of [DirBuster][1], which is slow, requires
Java, lacks active development, and is generally PITA. Erlang was chosen as a
platform to provide high-performance I/O, robustness, concurrency and
transparent distribution over multiple nodes.

License
-------

 - The `ece_resource_static` module is from the [eCloudEdit][3] project, developed
   by James Yu, released under MIT license.
 - Twitter Bootstrap (a subset of the files in `priv/static`) is licensed under
   the Apache 2.0 license.
 - Everything else is available under MIT license, see `LICENSE.txt`.

Dependencies
------------

 - Recent Erlang distribution
 - `ibrowse` https://github.com/cmullaparthi/ibrowse (fetched automatically)
 - RFC 1808 http://tools.ietf.org/rfc/rfc1808.txt (for unit tests only, fetched automatically)
 - Python 2.6+, Requests, Flask (for server tests only)

Building
--------

	$ make

Running unit tests
------------------

	$ make test

Web interface
-------------

	$ ./start.sh
	...
	=PROGRESS REPORT==== 13-Nov-2014::14:08:27 ===
         application: dirbusterl
          started_at: nonode@nohost

The web interface is available at http://localhost:8000/ by default, you can
change that by setting the `WEBMACHINE_PORT` environment variable.

![Screenshot of the web interface](doc/screenshot-web.png?raw=true)

Erlang interface
----------------

There are currently two entry points:

	dirbusterl:bust(URL, UserConfig)
	dirbusterl:bust_async(Id, URL, UserConfig)

 - `Id` is a binary that can be used to access the result
 - `URL` is a string that is requested first (entry point)
 - `UserConfig` can be a list of the following configuration parameters (required since a wordlist or a list of URLs is mandatory)

While `bust` is synchronous and blocks execution until finished, `bust_async`
returns immediately and the `Id` parameter can be used to check progress. Such
value can be generated using `dirbusterl_storage:generate_bust_id/0` but any
unique binary value will do. Findings can be queried by calling
`dirbusterl_storage:get_findings/1` with the `Id` as the sole parameter. The
structure of the return value is the same as with the `dirbusterl:bust/2` function.

### Configuration parameters

 - `follow_dirs` recurse into subdirectories
 - `follow_redirs` follow HTTP redirections
 - `parse_body` parse HTML, CSS, `robots.txt` files for URLs
 - `{url_restriction, RE}` allows only such requests that match the regular expression `RE` supplied as a string, example: `{url_restriction, "^http://localhost/foo/"}`
 - `{postfix, List}` append the elements of `List` one by one after each dictionary word, example: `{postfix, [".php", ".html"]}`
 - `{mangle_found, Rules}` for every file found, other files are requested by
   applying each item of the list `Rules`, the original filename will replace
   `\1`, example: `{mangle_found, ["\\1~", ".\\1.swp"]}` and some more ideas:
   [fuzzdb/discovery/FilenameBruteforce/Extensions.Backup.fuzz.txt][2]
 - `{http_cfg, Params}` set `ibrowse` parameters (proxy, authentication) directly, example: `{http_cfg, [{proxy_host, "localhost"}, {proxy_port, 8081}]}`
 - `{wordlist, FileName}` read words from this file (mandatory parameter, only the first occurrence is used), example: `{wordlist, "DirBuster-0.12/directory-list-2.3-small.txt"}`
 - `{url_list, FileName}` read URLs from this file (zero or more can be used), example: `{url_list, "burp-urls.txt"}`
 - `{headers, List}` append HTTP headers to each request, example: `{headers, [{"User-Agent", "Agent Smith"}, {"Authorization", "..."}]}`

### Input file formats

URLs and words read from files are parsed according to the following rules.

 - empty lines are ignored
 - lines starting with a `#` are treated as comments and are ignored
 - lines can end with either `\n` (line feed, 0x0a, Unix-style) or `\r\n`
   (carriage return + line feed, 0x0c0a, Windows-style)
 - characters that are treated specially in URLs (for example `%` or `/`)
   automatically get URL encoded in wordlists but not in URL lists

Example run
-----------

_Note:_ if the web interface works for you, you don't need this anymore.

For those poor Erlang-unaware souls

	$ erl -pa ebin -pa deps/ibrowse/ebin/ -s ibrowse
	1> mnesia:create_schema([node()]).
	2> mnesia:start().
	3> dirbusterl_storage:init_schema().
	4> dirbusterl_requests:start_link().
	5> dirbusterl:bust("http://www.example.com", [{wordlist,"wordlist.txt"},{postfix,[".html"]},parse_body]).

TODO
----

 - nmap favicon DB hash detection
 - Etag and Last-Modified parsing
 - detection of case-insensitive behavior (Windows servers)
 - detection of duplicate files (Apache MultiViews, symbolic links, etc.)
 - multi-node support

  [1]: https://www.owasp.org/index.php/Category:OWASP_DirBuster_Project
  [2]: https://code.google.com/p/fuzzdb/source/browse/trunk/discovery/FilenameBruteforce/Extensions.Backup.fuzz.txt
  [3]: https://github.com/tsloughter/eCloudEdit
