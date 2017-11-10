#!/usr/bin/env bash

set -euf -o pipefail

# ALTER TABLE person ALTER COLUMN lname SET DEFAULT 'unknown';
# ALTER TABLE person ALTER COLUMN dob SET DEFAULT '-infinity';
# ALTER TABLE person ADD PRIMARY KEY (fname,lname,dob);

# https://www.postgresql.org/docs/10/static/functions-json.html
psql -v ON_ERROR_STOP=1 <<-EOSQL
  CREATE FUNCTION upsert(xs json) RETURNS void
    AS \$\$ INSERT INTO person AS p
            SELECT * FROM json_to_recordset(xs)
            AS x(fname character varying, lname character varying, dob date, phone character(10))
            ON CONFLICT (fname,lname,dob) DO UPDATE
            SET phone = EXCLUDED.phone
            WHERE p.phone != EXCLUDED.phone OR p.phone IS null;
       \$\$
    LANGUAGE SQL IMMUTABLE STRICT;
EOSQL
