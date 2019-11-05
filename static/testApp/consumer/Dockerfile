FROM ubuntu:18.04
RUN mkdir /app
COPY consumer.py /app
RUN apt-get update
RUN apt-get install -y python3 python3-pip
RUN pip3 install pika
CMD python3 app/consumer.py
