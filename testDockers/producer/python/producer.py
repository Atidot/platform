from time import sleep
from datetime import datetime
import pika
import os

def main():
    if not platformCheck():
        raise Exception("Platform harness not initialized.")
    host = amqpURL()
    consumer = consumers()[0]

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

def amqpURL():
    return os.environ.get('PLATFORM_AMQP_URL', '')

def platformCheck():
    s = os.environ.get('PLATFORM_HARNESS', '')
    if s == '1':
        return True
    else:
        return False

# The harness module provides environment variables and queues
def consumers():
    return getPairs('PLATFORM_CONSUMERS')

def producers():
    return getPairs('PLATFORM_PRODUCERS')

def getPairs(envVar):
    l = os.environ.get(envVar, '')
    producerList = list()
    for elt in l:
        d = dict()
        pairs = elt.split(',')
        for pair in pairs:
            p = pair.split('=')
            key, val = p[0], p[1]
            d[key] = val
        producerList.append(Entity(**d))
    return producerList

if __name__ == '__main__':
    main()
