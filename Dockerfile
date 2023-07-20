FROM folioci/alpine-jre-openjdk17:latest

USER root

# Install latest patch versions of packages: https://pythonspeed.com/articles/security-updates-in-docker/
RUN apk upgrade --no-cache

# Copy your fat jar to the container
ENV APP_FILE mod-remote-storage-fat.jar

# - should be a single jar file
ARG JAR_FILE=./target/*.jar
# - copy
COPY ${JAR_FILE} ${JAVA_APP_DIR}/${APP_FILE}

ARG RUN_ENV_FILE=run.sh

COPY ${RUN_ENV_FILE} ${JAVA_APP_DIR}/
RUN chmod 755 ${JAVA_APP_DIR}/${RUN_ENV_FILE}

# Run as this user
USER folio

# Expose this port locally in the container.
EXPOSE 8081
