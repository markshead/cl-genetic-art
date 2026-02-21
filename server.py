#!/usr/bin/env python3
import http.server
import socketserver
import sys

PORT = 8080
if len(sys.argv) > 1:
    PORT = int(sys.argv[1])

class RestrictedHTTPRequestHandler(http.server.SimpleHTTPRequestHandler):
    def do_GET(self):
        # Map root to the viewer
        if self.path == '/':
            self.path = '/viewer.html'
            
        # Ignore query string parameters (like the ?t= cache buster)
        base_path = self.path.split('?')[0]
        
        # Maintain a strict whitelist of files that are allowed to be downloaded
        allowed_files = ['/viewer.html', '/current-best-candidate.png']
        
        if base_path in allowed_files:
            # We must trick SimpleHTTPRequestHandler into using the base path
            # so it doesn't fail trying to read a file with "?t=" in the filename
            self.path = base_path
            return super().do_GET()
        else:
            # Reject all directory listing or unauthorized file access attempts
            self.send_error(403, "Forbidden: Security policy restricts access to this file.")

with socketserver.TCPServer(("", PORT), RestrictedHTTPRequestHandler) as httpd:
    print(f"\n✅ Restricted Viewer Dashboard starting...")
    print(f"🌐 Open your browser to: http://0.0.0.0:{PORT}")
    print(f"🔒 Security: Only serving viewer.html and current-best-candidate.png")
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        print("\nShutting down server...")
        httpd.server_close()
