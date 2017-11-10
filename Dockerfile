FROM postgres:10-alpine

COPY init-role.sh /docker-entrypoint-initdb.d/1_init-role.sh
COPY person.sql.gz /docker-entrypoint-initdb.d/2_person.sql.gz
COPY create-fun.sh /docker-entrypoint-initdb.d/3_create-fun.sh
