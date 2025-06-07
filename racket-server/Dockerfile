FROM racket/racket:latest

WORKDIR /app

RUN apt-get update && apt-get install -y xvfb

RUN raco pkg install --auto web-server
RUN raco pkg install --auto redex
RUN raco pkg install --auto hosted-minikanren
RUN raco pkg install --auto redex-etc

COPY src/ .

ENV PLTSTDERR=error
ENV DISPLAY=:99

EXPOSE 5000

CMD xvfb-run -a racket app.rkt
# CMD racket app.rkt
