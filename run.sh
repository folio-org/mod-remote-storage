#!/bin/sh
#
#
#JAVA_OPTS=$JAVA_OPTIONS
#
DB_URL="jdbc:postgresql://${DB_HOST}:${DB_PORT}/${DB_DATABASE}"
#
APP_OPTS="--spring.datasource.username=${DB_USERNAME} \
        --spring.datasource.password=${DB_PASSWORD} \
        --spring.datasource.url=${DB_URL}"
#
#
echo "APP_OPTS=${APP_OPTS}"
#
exec java org.springframework.boot.loader.JarLauncher ${APP_OPTS}
