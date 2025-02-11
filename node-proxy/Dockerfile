FROM node:latest

WORKDIR /app

RUN apt-get update && apt-get install -y xdg-utils

COPY package*.json ./

RUN npm install

COPY . .

EXPOSE 3000

CMD ["node", "server.js"]