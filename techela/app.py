import techela
import threading
import webbrowser

port = 5543
host = '0.0.0.0' # use 0.0.0.0 to listen on all ports, not just for local connections
url = "http://{0}:{1}".format(host,port)

threading.Timer(1.25, lambda: webbrowser.open(url)).start()
techela.app.run(host=host, port=port, debug=True, use_reloader=False)
