# Modeling-miniKanren-in-Redex
## **Docker Setup**

Follow the steps below to clone this repository, set up Docker, and run the application.

### **Prerequisites**
Before you begin, ensure you have the following installed:

- **[Docker](https://docs.docker.com/get-docker/)** – to run containers
- **[Docker Compose](https://docs.docker.com/compose/install/)** – to manage multi-container applications

---

## **Installation and Setup**

Open a terminal and run:
```sh
git clone https://github.com/brysenPfingsten/Modeling-miniKanren-in-Redex.git
cd Modeling-miniKanren-in-Redex
docker-compose build
docker-compose up
```
Finally, visit [localhost:3000](http://localhost:3000).

## **Configuration**

The Docker images expect an amd64 platform. Users on Apple Silicon or other arm64 based architectures,
will need to work around this, by either

- Set the `DOCKER_DEFAULT_PLATFORM` environment variable (permanently
  by adding `export DOCKER_DEFAULT_PLATFORM=linux/amd64` to your
  `bashrc`) or
- If you regularly use Docker for other purposes, you could add a
  `platform` to the YAML file found in this directory
