FROM ubuntu:18.04
COPY ../../../platform-harness/testApps/consumer.py /app
RUN apt-get update
RUN apt-get install -y python3
CMD python3 app/consumer.py
