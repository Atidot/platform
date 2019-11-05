import pika
import logging

def main():
    connection = pika.BlockingConnection(
        pika.ConnectionParameters(host='localhost'))
    logging.basicConfig(filename='messages_received.txt')
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

if __name__ == '__main__':
    main()
