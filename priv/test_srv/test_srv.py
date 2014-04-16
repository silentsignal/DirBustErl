#!/usr/bin/env python

from flask import Flask

app = Flask(__name__)

@app.route('/')
def home():
    return 'Got r00t? Sounds g00d!'

@app.route('/broken404/', defaults={'param': ''})
@app.route('/broken404/<param>')
def broken404(param):
    return 'I answer with 200 to everything!'

@app.route('/mangling/')
@app.route('/mangling/foo')
@app.route('/mangling/.foo.swp')
@app.route('/mangling/.bar.swp')
def mangling():
    return 'Mangling is useful'

@app.route('/postfix/bar.html')
def postfix():
    return '<html><body>Welcome to the bar!</body></html>'

if __name__ == '__main__':
    app.run()
