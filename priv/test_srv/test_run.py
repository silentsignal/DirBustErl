#!/usr/bin/env python

from __future__ import unicode_literals
from time import sleep
from unittest import TestCase, main
import json
import requests

DIRB_ROOT = 'http://localhost:8000/'
TEST_ROOT = 'http://localhost:5000/'
BUST_RESOURCE_URL = DIRB_ROOT + 'busts'

class TestDirBustErl(TestCase):
    def setUp(self):
        self.session = requests.session()

    def start_bust(self, **params):
        return self.session.post(BUST_RESOURCE_URL, data=json.dumps(params),
                headers={'Content-Type': 'application/json'})

    def simple_bust(self, url):
        bust = self.start_bust(url=url, wordlist='TEST.txt',
                follow_dirs=True, follow_redirs=True, parse_body=True)
        return self.get_bust_results(bust)

    def get_bust_results(self, bust):
        self.assertEquals(bust.status_code, requests.codes.created)
        self.assertTrue(bust.url.startswith(DIRB_ROOT))
        bust_url = bust.headers['location']
        _, bust_id = bust_url.rsplit('/', 1)
        for _ in xrange(500):
            sleep(0.1)
            results = self.session.get(BUST_RESOURCE_URL).json()
            result = next(r for r in results if r['id'] == bust_id)
            if result['status'] == 'finished':
                break
        else:
            self.fail("Bust didn't finish within 5 seconds")
        results = self.session.get(bust_url + '/findings.json').json()
        self.session.delete(bust_url)
        return results

    def test_smoke(self):
        findings = self.simple_bust(TEST_ROOT)
        self.assertEquals(findings,
                [{'url': TEST_ROOT, 'code': requests.codes.ok}])

    def test_broken404(self):
        findings = self.simple_bust(TEST_ROOT + 'broken404/')
        self.assertEquals(findings, [])

    def test_mangling(self):
        url = TEST_ROOT + 'mangling/'
        bust = self.start_bust(url=url, wordlist='TEST.txt',
                mangle_found=[r'.\1.swp', r'\1~'], url_restriction='^' + url)
        findings = self.get_bust_results(bust)
        expected = [{'url': url + p, 'code': requests.codes.ok}
                for p in ('', 'foo', '.foo.swp', '.qux.swp')]
        expected.append({'url': url + 'qux', 'code': requests.codes.found,
            'redir': url + 'qux2'})
        self.assertEquals(sorted(findings), sorted(expected))

    def test_headers(self):
        url = TEST_ROOT + 'header-req'
        for headers, expected in [([], []), ([['X-Files', 'TrustNo1']],
                [{'url': url, 'code': requests.codes.ok}])]:
            bust = self.start_bust(url=url, wordlist='TEST.txt',
                    url_restriction='^' + url, headers=headers)
            findings = self.get_bust_results(bust)
            self.assertEquals(findings, expected)

    def test_postfix(self):
        url = TEST_ROOT + 'postfix/'
        bust = self.start_bust(url=url, wordlist='TEST.txt',
                postfix=['.html', '.php'], url_restriction='^' + url)
        findings = self.get_bust_results(bust)
        self.assertEquals(findings,
                [{'url': url + 'bar.html', 'code': requests.codes.ok}])


if __name__ == '__main__':
    main()
