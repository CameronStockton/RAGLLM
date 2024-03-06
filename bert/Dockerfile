FROM python:3.8-slim-buster

WORKDIR /bert

RUN apt-get -y update; apt-get -y install curl

COPY . /bert

RUN pip install --trusted-host pypi.python.org -r requirements.txt

EXPOSE 8000

CMD ["python", "run.py"]