#!/usr/bin/env python

from time import sleep
from unittest import TestCase, main
import json
import requests

DIRB_ROOT = 'http://localhost:8000/'
TEST_ROOT = 'http://localhost:5000/'
BUST_RESOURCE_URL = DIRB_ROOT + 'bust'

class TestDirBustErl(TestCase):
    def setUp(self):
        self.session = requests.session()

    def start_bust(self, **params):
        return self.session.post(BUST_RESOURCE_URL, data=json.dumps(params),
                headers={'Content-Type': 'application/json'})

    def simple_bust(self, url):
        bust = self.start_bust(url=url, wordlist='TEST.txt',
                follow_dirs=True, follow_redirs=True, parse_body=True)
        self.assertEquals(bust.status_code, requests.codes.created)
        self.assertTrue(bust.url.startswith(DIRB_ROOT))
        return bust.headers['location']

    def test_smoke(self):
        bust_url = self.simple_bust(TEST_ROOT)
        _, bust_id = bust_url.rsplit('/', 1)
        for _ in xrange(500):
            sleep(0.1)
            results = self.session.get(BUST_RESOURCE_URL).json()
            result = next(r for r in results if r['id'] == bust_id)
            if result['status'] == 'finished':
                break
        else:
            self.fail("Bust didn't finish within 5 seconds")
        findings = self.session.get(bust_url).json()
        self.assertEquals(findings,
                [{'url': TEST_ROOT, 'code': requests.codes.ok}])

    def test_broken404(self):
        bust_url = self.simple_bust(TEST_ROOT + 'broken404/')
        _, bust_id = bust_url.rsplit('/', 1)
        for _ in xrange(500):
            sleep(0.1)
            results = self.session.get(BUST_RESOURCE_URL).json()
            result = next(r for r in results if r['id'] == bust_id)
            if result['status'] == 'finished':
                break
        else:
            self.fail("Bust didn't finish within 5 seconds")
        findings = self.session.get(bust_url).json()
        self.assertEquals(findings, [])


if __name__ == '__main__':
    main()
