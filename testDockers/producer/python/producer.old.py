from time import sleep
from datetime import datetime
import pika
import .harness as h

def main():
    if not h.platformCheck():
        raise Exception("Platform harness not initialized.")
    host = h.amqpURL()
    consumer = h.consumers()[0]

    connection = pika.BlockingConnection(
        pika.ConnectionParameters(host=host))
    try:
        channel = connection.channel()
        channel.queue_declare(queue=consumer)
        while True:
            channel.basic_publish(exchange='', routing_key=consumer, body=timeString())
            sleep(1)
    except:
        raise
    finally:
        connection.close()

def timeString():
    return datetime.strftime(datetime.now(), '%X')

if __name__ == '__main__':
    main()
