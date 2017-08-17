import sys

def start():
    """test"""
    a = 0
    for i in range(1, 1000):
        print("xxx_swo")
        sys.stdout.write("Got %s\n" % i)
        a+=i
    return a

start()
