from time import sleep
from datetime import datetime
import pika

def main():
    connection = pika.blockingConnection(
            pika.ConnectionParameters(host='localhost'))
    try:
        channel = connection.channel()
        channel.queue_declare(queue='ticks')
        while True: # switch over to runSignal() if necessary
            sleep(1)
            channel.basic_publish(exchange='', 
                                  routing_key='ticks', 
                                  body=timeString())
    except:
        raise
    finally:
        connection.close()

# inspect the environment variables
# and return True if the app is supposed to be running
def runSignal():
    return True

def timeString():
    return datetime.strftime(datetime.now(), '%X')
