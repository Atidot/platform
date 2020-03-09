import pika
import logging
import os

def main():
    with open("test.txt", "a") as myfile:
        myfile.write("started main\n")
    if not platformCheck():
        raise Exception("Platform harness not initialized.")
    with open("test.txt", "a") as myfile:
        myfile.write("platform checked\n")
    host = amqpURL()
    producer = producers()[0]

    connection = pika.BlockingConnection(
        pika.ConnectionParameters(host=host))
    logging.basicConfig(
            filename='messages_received.txt',
            level=logging.INFO
    )
    with open("test.txt", "a") as myfile:
        myfile.write("started connection\n")
    try:
        channel = connection.channel()
        channel.queue_declare(queue=producer)
        channel.basic_consume(queue=producer, auto_ack=True,
                              on_message_callback=logMsg)
        with open("test.txt", "a") as myfile:
            myfile.write("set up consumption\n")
        channel.start_consuming()
        with open("test.txt", "a") as myfile:
            myfile.write("finished consuming cleanly\n")
    except:
        raise
    finally:
        connection.close()
        with open("test.txt", "a") as myfile:
            myfile.write("closed connection\n")

def logMsg(ch, method, properties, body):
    logging.info(body)

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

with open("test.txt", "a") as myfile:
    myfile.write("read through file\n")

if __name__ == '__main__':
    main()

