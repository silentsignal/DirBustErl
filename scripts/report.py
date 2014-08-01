#!/usr/bin/env python

from __future__ import unicode_literals
from collections import defaultdict
import requests

REQ_SES = requests.session()

def print_header(header):
    print ''
    print header
    print '-' * len(header)

def report(ids):
    files = defaultdict(set)
    dirs = set()
    redirs = dict()
    for bust_id in ids:
        for result in REQ_SES.get(b'http://localhost:8000/busts/' + bust_id + '/findings.json').json():
            redir = result.get('redir')
            if result.get('dir'):
                dirs.add(result['url'])
            elif redir:
                redirs[result['url']] = redir
            else:
                files[result['code']].add(result['url'])
    print 'DirBustErl report :: https://github.com/silentsignal/DirBustErl'
    for code, file_list in sorted(files.iteritems()):
        print_header('Files found with response code {0}'.format(code))
        for file_name in file_list:
            print file_name
    if dirs:
        print_header('Directories found')
        for dir_name in sorted(dirs):
            print dir_name
    if redirs:
        print_header('Redirections found')
        for redir_from, redir_to in sorted(redirs.iteritems()):
            print redir_from, '->', redir_to


if __name__ == '__main__':
    from sys import argv
    report(argv[1:])
