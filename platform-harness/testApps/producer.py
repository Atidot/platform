import time
import harness

def main():
    while True:
        time.wait(1)
        sendTick(harness.consumers())

def sendTick():
    pass
