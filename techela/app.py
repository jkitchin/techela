import techela
import threading
import webbrowser

port = 5543
url = f"http://127.0.0.1:{port}"

threading.Timer(1.25, lambda: webbrowser.open(url)).start()
techela.app.run(port=port, debug=True, use_reloader=False)
