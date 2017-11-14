#!/usr/bin/env bash

set -euf -o pipefail

# createuser admin --createdb --createrole
createuser updater --no-login
createuser authenticator --no-inherit --no-login
createuser incognito --no-login

# https://www.postgresql.org/docs/10/static/functions-json.html
psql -v ON_ERROR_STOP=1 --username="$POSTGRES_USER" --dbname="$POSTGRES_DB" <<-EOSQL
  UPDATE person SET lname='unknown' WHERE lname IS null;
  UPDATE person SET dob='-infinity' WHERE dob IS null;

  ALTER TABLE person ADD PRIMARY KEY (fname,lname,dob);

  CREATE FUNCTION upsert(xs json) RETURNS void
    AS \$\$ INSERT INTO person AS p
            SELECT * FROM json_to_recordset(xs)
            AS x(fname character varying, lname character varying, dob date, phone character(10))
            ON CONFLICT (fname,lname,dob) DO UPDATE
            SET phone = EXCLUDED.phone
            WHERE p.phone != EXCLUDED.phone OR p.phone IS null;
       \$\$
    LANGUAGE SQL IMMUTABLE STRICT;

  GRANT incognito TO postgres;
  GRANT updater TO postgres;
  GRANT updater to authenticator;

  GRANT USAGE ON SCHEMA public TO incognito;
  GRANT USAGE ON SCHEMA public TO updater;

  GRANT SELECT ON public.person TO incognito;
  GRANT EXECUTE ON FUNCTION public.upsert TO updater;
EOSQL

# To check if the permissions where set correctly using psql
# \du
# SELECT * FROM information_schema.role_routine_grants WHERE routine_name='upsert';
