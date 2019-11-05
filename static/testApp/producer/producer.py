from time import sleep
from datetime import datetime
import pika

def main():
    connection = pika.BlockingConnection(
        pika.ConnectionParameters(host='localhost'))
    try:
        channel = connection.channel()
        channel.queue_declare(queue='ticks')
        while True:
            channel.basic_publish(exchange='', routing_key='ticks', body=timeString())
            sleep(1)
    except:
        raise
    finally:
        connection.close()

def timeString():
    return datetime.strftime(datetime.now(), '%X')

if __name__ == '__main__':
    main()
