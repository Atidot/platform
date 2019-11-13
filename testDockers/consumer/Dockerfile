FROM ubuntu:18.04
RUN mkdir /app
COPY consumer.py /app
RUN apt-get && apt-get install -y \
    python3 \
    python3-pip \
 && pip3 install pika
CMD ["python3" "app/consumer.py"]
