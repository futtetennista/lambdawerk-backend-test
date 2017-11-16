# LambdaWerk backend developer test

## Sanitisation data in XML file
There are some not well-formed data in the XML file, i.e.:
`"ABD AL", "PIKULSKI", "\\N", "3158419207"`
my strategy has been to simply saniting it this way:
`"ABD AL", "PIKULSKI", "", "3158419207"`

Other options would have been:
- deleting the member from the import set
- somehow reporting the "impure data"
- rejecting the whole file

but since the assignment focus wasn't on this and the initial bulk of persons also
contains such data, I applied the easiest solution I could think of.

## Considered solutions analysis

### 1. Reading the whole XML file

#### Algorithm description
- parse all persons from XML file
- SELECT all persons from the database
- for each person retrieved from the database, check if their telephone number
is different from the one in the XML file: if so, UPDATE the record otherwise do nothing
- INSERT all remaining (new) persons

#### Performance evaluation
The algorithm needs O(P) memory, it would require O(1) SELECT and O(P) UPDATEs /
INSERTs where P is the number of persons in the XML file.

CONs: the memory requirement are undesirable, the ideal solution would require
constant memory

### 2. Streamig the XML file

#### Algorithm description
- parse one person from XML file
- `SELECT` the person from the database
- if the result is empty `INSERT` the person otherwise check if the telephone
number is different from the one in the XML file: if so, `UPDATE` the record
otherwise do nothing
- repeat for all persons in the XML file

#### Performance evaluation
The algorithm needs O(1) memory but would require O(P) `SELECT`s and O(P) `UPDATE`s /
`INSERTION`s where P is the number of persons in the XML file.

PRO: constant memory usage
CONs: O(P) `SELECT`s required. That's slow, since they involve I/O and puts too much
load on the database

### 3. Batching database operations

#### Algorithm description
- parse N persons from XML file (N might depend on different factors, i.e. memory available)
- `SELECT` N persons from the database
- for each person retrieved from the database, check if their telephone number
is different from the one in the XML file: if so, `UPDATE` the record otherwise
do nothing
- `INSERT` all remaining (new) persons
- repeat for all persons in the XML file

#### Performance evaluation
The algorithm needs O(1) memory and it would require O(P / M) `SELECT`s and O(P)
`UPDATE`s / `INSERTION`s where P is the number of persons in the XML file and N is
the fixed batch size - this might depend on different factors, i.e. memory available.

PRO: constant memory usage
CONs: O(P / N) `SELECT`s required. That's better than the previous algorithm but
still potentially slow. Is it possible to avoid this step completely?


### 4. Batch-upserting

#### Prerequisites
- `UPDATE` the persons table and add default `lname`s and `dob`s where missing
in order to be able to add a primary key constraint
- `ALTER` the persons table and add a `PRIMARY KEY` constraint needed by the
`ON CONFLICT` statement.

#### Algorithm description
- parse N persons from XML file (N might depend on different factors,
i.e. memory available)
- `UPSERT` N persons leveraging Postegres own `ON CONFLICT` statement
- repeat for all persons in the XML file

The `UPSERT` statement would be something like:
``` sql
INSERT INTO person AS p VALUES
('JIARA','HERTZEL','1935-06-05','5859012134'),
('RONJARVIOU','COMELLO','1932-09-27','7702713416')
ON CONFLICT (fname,lname,dob) DO UPDATE
SET phone = EXCLUDED.phone
WHERE p.phone != EXCLUDED.phone OR p.phone IS null;
```

#### Performance evaluation
The algorithm needs O(1) memory and O(P) `UPSERT`ions where P is the number of
persons in the XML file.

##### PROs
- client runs in constant memory
- leverage postgres native merging capabilities to:
  - simplify the client logic
  - avoid the need to do any (slow) I/O to `SELECT` rows first to apply merging
  in the client
  - achieve good separation of concerns:
    - client code parses the update XML file and asks the database to run the updates
    - database function handles the merge logic

##### CONs
- there is a one-time cost to pay to sanitise the data in the persons table
- there is a one-time cost to pay to add a primary key constraint in the persons table
- not sure how the postgres function can be tested in an automated fashion

The last solution seems to be the most performant in terms of both time and space
and that's the reason why I decided ot implement it.

## Assumptions
- The database is on a remote machine therefore the program must connect to it over
the wire
- Removing the 267 entries that do not have either a last name or a birthdate is
an unacceptable data loss
- Importing persons is a periodic task so the one-time costs to pay upfront are
acceptable


## Providing environment variables for the docker images
- Create a file named `postgres.env` and set the following values:
  * `POSTGRES_DB`
  * `POSTGRES_USER`
  * `POSTGRES_PASSWORD`
- Create a file named `postgREST.env` and set the following values:
  * `PGRST_DB_URI` of the form:
  `postgres://[POSTGRES_USER]:[POSTGRES_PASSWORD]@postgres_container_alias:5432/[POSTGRES_DB]`
  * `PGRST_DB_SCHEMA`
  * `PGRST_DB_ANON_ROLE`
  * `PGRST_SERVER_PROXY_URI`
  * `PGRST_JWT_SECRET`

Have a look at the sample files provided: `postgres.env.sample`,
`postgREST.env.sample`, `rsa.jwk.pub.sample`
