import sys
import time

def start():
    """test"""
    a = 0
    sys.stdout.write("Starting\n")

    for i in range(1, 3):
        sys.stdout.write("Got %s\n" % i)
        sys.stdout.flush()
        time.sleep(1)
        a+=i
    return a

start()
