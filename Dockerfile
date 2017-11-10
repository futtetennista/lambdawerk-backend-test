FROM postgres:10-alpine

COPY init-role.sh /docker-entrypoint-initdb.d/init-role.sh
COPY person.sql.gz /docker-entrypoint-initdb.d/
