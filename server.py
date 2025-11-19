#!/usr/bin/env python3
from http.server import HTTPServer, SimpleHTTPRequestHandler

class CORSRequestHandler(SimpleHTTPRequestHandler):
    def end_headers(self):
        # Allow CORS for fetch from workers
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET, OPTIONS')
        self.send_header('Access-Control-Allow-Headers', '*')
        SimpleHTTPRequestHandler.end_headers(self)

if __name__ == '__main__':
    httpd = HTTPServer(('localhost', 8003), CORSRequestHandler)
    print("Server running on http://localhost:8003")
    httpd.serve_forever()
