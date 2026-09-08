from http.server import ThreadingHTTPServer, SimpleHTTPRequestHandler
import os
import re
import sys
from urllib.parse import unquote

class RangeRequestHandler(SimpleHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def end_headers(self):
        # Helpful for local MapLibre/PMTiles testing.
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Headers", "Range")
        self.send_header(
            "Access-Control-Expose-Headers",
            "Accept-Ranges, Content-Length, Content-Range"
        )
        self.send_header("Accept-Ranges", "bytes")
        super().end_headers()

    def send_head(self):
        path = self.translate_path(self.path)

        if os.path.isdir(path):
            parts = self.path.split("?", 1)[0].split("#", 1)[0]
            if not parts.endswith("/"):
                self.send_response(301)
                self.send_header("Location", parts + "/")
                self.end_headers()
                return None

            for index in ("index.html", "index.htm"):
                index_path = os.path.join(path, index)
                if os.path.exists(index_path):
                    path = index_path
                    break
            else:
                return self.list_directory(path)

        ctype = self.guess_type(path)

        try:
            f = open(path, "rb")
        except OSError:
            self.send_error(404, "File not found")
            return None

        try:
            fs = os.fstat(f.fileno())
            file_size = fs.st_size

            range_header = self.headers.get("Range")

            if range_header:
                match = re.match(r"bytes=(\d*)-(\d*)$", range_header.strip())

                if not match:
                    self.send_error(400, "Invalid Range header")
                    f.close()
                    return None

                start_text, end_text = match.groups()

                if start_text == "":
                    # Suffix-byte-range-spec, e.g. bytes=-500
                    suffix_len = int(end_text)
                    if suffix_len <= 0:
                        self.send_error(416, "Requested Range Not Satisfiable")
                        f.close()
                        return None
                    start = max(0, file_size - suffix_len)
                    end = file_size - 1
                else:
                    start = int(start_text)
                    end = int(end_text) if end_text else file_size - 1

                if start >= file_size or start < 0:
                    self.send_response(416)
                    self.send_header("Content-Range", f"bytes */{file_size}")
                    self.end_headers()
                    f.close()
                    return None

                end = min(end, file_size - 1)
                length = end - start + 1

                self.send_response(206)
                self.send_header("Content-type", ctype)
                self.send_header("Content-Range", f"bytes {start}-{end}/{file_size}")
                self.send_header("Content-Length", str(length))
                self.send_header("Last-Modified", self.date_time_string(fs.st_mtime))
                self.end_headers()

                f.seek(start)
                self.range = (start, end)
                return f

            self.send_response(200)
            self.send_header("Content-type", ctype)
            self.send_header("Content-Length", str(file_size))
            self.send_header("Last-Modified", self.date_time_string(fs.st_mtime))
            self.end_headers()

            self.range = None
            return f

        except:
            f.close()
            raise

    def copyfile(self, source, outputfile):
        if getattr(self, "range", None):
            start, end = self.range
            remaining = end - start + 1
            bufsize = 64 * 1024

            while remaining > 0:
                chunk = source.read(min(bufsize, remaining))
                if not chunk:
                    break
                outputfile.write(chunk)
                remaining -= len(chunk)
        else:
            super().copyfile(source, outputfile)


if __name__ == "__main__":
    port = 8000

    if len(sys.argv) > 1:
        port = int(sys.argv[1])

    server = ThreadingHTTPServer(("", port), RangeRequestHandler)

    print(f"Serving range-enabled HTTP on port {port}")
    print("Press Ctrl+C to stop.")

    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nStopping server.")
