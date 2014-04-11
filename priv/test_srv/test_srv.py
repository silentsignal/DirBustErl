#!/usr/bin/env python

from flask import Flask

app = Flask(__name__)

@app.route('/')
def home():
    return 'Got r00t? Sounds g00d!'

if __name__ == '__main__':
    app.run()
