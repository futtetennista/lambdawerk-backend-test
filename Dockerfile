FROM postgres:10-alpine

COPY person.sql.gz /docker-entrypoint-initdb.d/1_init_db.sql.gz
COPY setup_db.sh /docker-entrypoint-initdb.d/2_setup_db.sh
