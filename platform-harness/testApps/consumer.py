import pika
import logging

def main():
    logging.basicConfig(filename='messages_received.txt')
    connection = pika.blockingConnection(
            pika.ConnectionParameters(host='localhost'))
    try:
        channel = connection.channel()
        channel.queue_declare(queue='ticks')
        channel.basic_consume(queue='ticks', auto_ack=True,
                              on_message_callback=logMsg)
        channel.start_consuming()
    except:
        raise
    finally:
        connection.close()

def logMsg(ch, method, properties, body):
    logging.debug(body)
