#!/usr/bin/env bash

set -euf -o pipefail

createuser importer --no-login
createuser authenticator --no-inherit --no-login
createuser incognito --no-login

# https://www.postgresql.org/docs/10/static/functions-json.html
psql -v ON_ERROR_STOP=1 --username="$POSTGRES_USER" --dbname="$POSTGRES_DB" <<-EOSQL
  -- needed to add the PRIMARY KEY constraint
  UPDATE person SET lname='unknown' WHERE lname IS null;
  UPDATE person SET dob='-infinity' WHERE dob IS null;

  ALTER TABLE person ADD PRIMARY KEY (fname,lname,dob);

  CREATE FUNCTION upsert(members json) RETURNS json
  AS \$\$
    DECLARE
      row_stats integer;
    BEGIN
      INSERT INTO person AS p
      SELECT * FROM json_populate_recordset(null::person,members)
      ON CONFLICT (fname,lname,dob) DO UPDATE
      SET phone = EXCLUDED.phone
      WHERE p.phone != EXCLUDED.phone OR p.phone IS null;

      GET DIAGNOSTICS affected_row_count = ROW_COUNT;
      RETURN json_build_object('row_stats',row_stats);
    END;
  \$\$ LANGUAGE PLPGSQL VOLATILE STRICT;

  -- setup roles for postgREST
  -- grant read permissions to anonymous users
  GRANT USAGE ON SCHEMA public TO incognito;
  GRANT SELECT ON public.person TO incognito;

  -- allow authenticator to execute "upsert" funtion
  GRANT importer to authenticator;
  -- grant read/write permissions to the importer
  GRANT USAGE ON SCHEMA public TO importer;
  GRANT INSERT,UPDATE,SELECT ON public.person TO importer;

EOSQL

# To check if the permissions where set correctly using psql
# \du
# SELECT * FROM information_schema.role_table_grants WHERE table_name = 'person';
# SELECT * FROM information_schema.role_routine_grants WHERE routine_name='upsert';
