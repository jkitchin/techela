import os
import techela
import threading
import webbrowser

import socket
import errno

RUNNING = False
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

port = 5543

# # Get a free port.
# while not RUNNING:
#     try:
#         s.bind(("127.0.0.1", port))
#         RUNNING = True
#         break
#     except socket.error as e:
#         if e.errno == errno.EADDRINUSE:
#             print("Port is already in use")
#             port += 1
#         else:
#             # something else raised the socket.error exception
#             print(e)

# s.close()

url = "http://127.0.0.1:{0}".format(port)

# I don't recall exactly why this step is needed. It must be you need to be
# sure the browser is open first.

threading.Timer(1.25, lambda: webbrowser.open(url)).start()

# We start in the assignments directory. I do not remember why.
# os.chdir(os.path.expanduser(f'{techela.COURSEDIR}/assignments'))

techela.app.run(port=port, debug=True)
