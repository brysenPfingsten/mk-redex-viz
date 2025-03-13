FROM racket/racket:latest

WORKDIR /app

RUN apt-get update && apt-get install -y xvfb

COPY requirements.txt .

RUN raco pkg install --auto web-server
RUN raco pkg install --auto redex
RUN raco pkg install --auto minikanren
RUN raco pkg install --auto redex-etc

COPY definitions.rkt .
COPY metafunctions.rkt .
COPY reduction-relations.rkt .
COPY judgment-forms.rkt .
COPY app.rkt .
COPY reification.rkt .
COPY transpiler.rkt .
 
ENV PLTSTDERR=error
ENV DISPLAY=:99

EXPOSE 5000

CMD xvfb-run -a racket app.rkt
# CMD racket app.rkt
