FROM ubuntu:18.04
RUN mkdir /app
COPY producer.py /app
RUN apt-get update && apt-get install -y \
    python3 \
    python3-pip \
 && pip3 install pika
CMD ["python3" "/app/producer.py"]
