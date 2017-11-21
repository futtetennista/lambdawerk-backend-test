# LambdaWerk backend developer test

## Problem analysis

### Ubiquitous Language
- person: an entity that has a first name `fname`, last name
`lname` and date of birth `dob` and telephone number `phone`.
A person is uniquely identified by their `fname`, `lname`, `dob`.
- database: a PostgreSQL instance
- persons table: a table in the database containing person records
- person record: a person stored in the persons table
- XML input file: a file on disk that contains the data to be merged
- entry: an XML element contained in the XML input file that represents a
person with the following form:
```
<member>
 <firstname>JOHN</firstname>
 <lastname>DOE</lastname>
 <date-of-birth>2002-02-01</date-of-birth>
 <phone>9548938821</phone>
</member>
```
- well-formed (of an entry or person record): that has `fname`, `lname`, and
`dob` not empty and and of the correct type
- importer: a program that takes the entries in the XML input file and asks the
database to merge them with the person records in the persons table
- merge process: see "Definition"

### Definition
Write a importer and update the persons table in such a way that, for each entry:
  1. if the `phone` in the persons table is equal to the `phone` in the entry
     nothing should be changed in the persons table
  2. if the `phone` in the persons table is not equal to the `phone` in the entry
     it should be changed in the persons table
  3. if the entry is not stored in the persons table, a new person record needs
     to be created

The importer can be written in a programming language of your choice and the
means of communication with the database are also not constrained.

### Success criteria
- merge process code must be *clean*
- merge process code must be *correct*
- provide basic loading statistics at the end of the merge process
- process the XML input file efficiently
- minimize the overall run time of the merge process
- reason about performance and memory usage of the merge process

## Plan

### Technologies
- database: [PostgreSQL 10](https://www.postgresql.org/) (given byt the problem).
In particular I use `PL/pgSQL` mostly to provide stats about the merge process on
the database side.
- importer: [Haskell](https://haskell-lang.org/). It's a purely functional
and strongly, statically typed language that allowed me to have high assurances
on correctness and cleanness of my code and develop my code in a type-driven fashion.
The vast ecosystem made it easy to write code to efficiently read entries from
in XML input file in and parse them writing a mimimum amount of code, and to
communicate to the database over HTTP.
- importer / database communication: [PostgREST](http://postgrest.com/) is a library
that provides an easy and convenient vai to interface with a PostgreSQL database
instance using a REST API over HTTP. It takes care of authentication using JWT
tokens (authorization is left to PostgreSQL)
- Docker: PostgreSQL, postgREST run inside [Docker](https://www.docker.com/)
containers using Docker Compose. The importer can be run from any machine
manually or also packaged in a Docker container.

### Description
I divided the problem into three sub-problems:
1. reading and parsing the XML input file
2. serialising the content of the XML input file
3. merging entries respecting the given invariants

To achieve a *clean* architecture I my design decision early on has been to give
the importer will be responsable for 1., the database for 3. and to make them
communicate using a REST API.

#### 1. Reading and parsing the XML input file
It was clear from reading the problem definition that the XML input file was
potentially to large to be read and parsed in memory. So the alternative was to
stream it. Because lazy I/O is quite tricky and has subtleties that makes it easy
to do something wrong, I used the [conduit](https://www.stackage.org/package/conduit-combinators)
library that provides a safer and more convenient way of streaming content. The
importer reads and parses `batchSize` entries at a time, where `batchSize` is a
parameter that must be configured when invoking the importer to tell it how many
entries should be parsed in one "read & parse" pass to be submitted to the database.
When choosing a `batchSize` one should consider at least the following factors:
- how much memory is available on the machine where the importer runs
- how much memory is needed to represent a person. I wrote down details about
that in `Persons.hs:22`
- the optimal batch size for multi-row `INSERT`s in PostgreSQL

### 3. Merging entries respecting the given invariants
The database is responsanble for merging entries supplied by the importer in a
correct and efficient way. I explored various possibilities:
* `SELECT`ing person records one-by-one and checking the invariants is hopelessly slow
* conditional `UPDATE`s don't cover the case of new person records
* `INSERT`ing using the `ON CONFLICT` clause looked promising since it's basically
what the invariants describe. The only catch here is that the `conflict_target`
must be unique. That meant adding a `PRIMARY KEY` constraint to the persons table.

Ultimately I decided to implement the last option. When the database is created
the persons table is `ALTER`ed to add a `PRIMARY KEY` but that lead to another
issue: 257 person records are not well-formed. The "Assumptions" and
"Sanitising data in XML input file" sections talk about how I decided to tackle
this. The final SQL statement to merge entries looks like the following:

``` SQL
DECLARE
  row_stats integer;
BEGIN
  INSERT INTO person AS p
  SELECT * FROM json_populate_recordset(null::person,entries)
  ON CONFLICT (fname,lname,dob) DO UPDATE
  SET phone = EXCLUDED.phone
  WHERE p.phone != EXCLUDED.phone OR p.phone IS null;

  GET DIAGNOSTICS row_stats = ROW_COUNT;
  RETURN json_build_object('row_stats',row_stats);
END;
```

It uses `PL/pgSQL` to be able to return basic statistics about the affected
person records in the database back to the importer.

### Profiling and benchmarks

I run these benchmarks on my laptop: MacBook Pro (Retina, 15-inch, Mid 2015),
2,5 GHz Intel Core i7, 16 GB 1600 MHz DDR3. First I did some tests playing
around with different batch size to check how that impacted performance:

* batchSize=1000, max_wal_size=1GB => 69.265649s (lots of connection errors:
postgREST can't keep up apparently)
* batchSize=10000, max_wal_size=1GB => 62.356193s (2-3 req/sec)
* batchSize=100000, max_wal_size=1GB => 65.404345s (1 req/~5secs)

Looking at PostgreSQL logs I noticed lots of warnings like
`checkpoints are occurring too frequently (29 seconds apart)` so I configured
PostgreSQL to use a bigger `max_wal_size`:

* batchSize=2000, max_wal_size=2GB => 61.797248s (20+ req/sec)
* batchSize=5000, max_wal_size=2GB => 64.429382s (4-5 req/sec)
* batchSize=10000, max_wal_size=2GB => 61.140297s (2-3 req/sec)

The warnings were gone but the running time of the merge process wasn't
changed.

Changing the batch size didn't seem to affect the total running time of the merge
process. In order to make it run quicker I could think of two strategies:

1. make the SQL in the database more efficient
2. make the parsing/serialising in the parser more efficient

Let's start with the database:
1. sending the entries to the database: there is going to be some I/O involved,
I tried tuning that by changing the batch size already and didn't really get
anything out of it
2. deserialisation of the input: this is internal to the database and
it cannot be optimised as far as I know
3. optimising the SQL statement: the statement is a database function that is a
stored procedure. It's already compiled and optimised by the database query planner.
The index on the `PRIMARY KEY` slows `INSERT`ions down but it cannot be removed
since the constraint is needed for `ON CONFLICT` to work. Tuning some database
settings is also an option, I explored changing `max_wal_size` but didn't get any
perfomance gains
4. hardware: this is out of the scope of the assignment
5. serialisation of the output: the database return a minimal response and I don't
see this as a bottleneck

I didn't walk this road too long because from my tests it didn't seem like the
database couldn't keep up the tasks given. So I went to the importer with the
goal of raising the number of requests per second that it could send to the
database focusing on:
1. deserialisation of the entries in the XML input file
2. serialisation of the entries into JSON
3. deserialisation of the database response

Here things got more interesting: while profiling the importer to optimise its
running time I actually found a memory leak that prevented it from running in
constant memory and fixed it. The files `importer-memleak.ps` and `importer-nomemleak.ps`
show the situation before and after for a reduced number of entries (50000).

## General assumptions
- The database is hosted on a remote machine therefore the importer must connect
to it over the internet.
- Creating the persons table is a one-time task and the one-time costs to pay
upfront to sanitise the person records (see below) so that a `PRIMARY KEY`
constraint can be created are acceptable costs. A `PRIMARY KEY` constraint is added
because is generally a good practice to have it and allows to implement the merge
process conveniently.
- Removing the 267 entries that do not have either a last name or a birthdate is
an unacceptable data loss.

## Sanitising data in XML input file
There are some not well-formed entries in the XML input file, i.e.:
`"ABD AL", "PIKULSKI", "\\N", "3158419207"`.
I decided to simply sanitise it this way:
`"ABD AL", "PIKULSKI", "-infinity", "3158419207"`.
Other options I considered where:

- filtering out the entry
- somehow reporting the entries that are not well-formed
- rejecting the whole file

The focus of the assignment wasn't on this particular aspect and 267 person records
are also not well-formed, I applied the easiest solution I could think of.

### Default values
- `dob`: `-infinity`
- `lname`, `fname`: `_` (in Haskell `_` means: "ignore this")


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


## Running the project

### Installation
- Docker
- the Stack build tool to use Haskell and install all needed dependencies

### Configuration
Components need some configurations parameters that need to be supplied via
environment variables, specifically:

- to set the environment variables needed for the PostgreSQL instance,
create a file named `postgres.env` and provide values for the following variables:
  * `POSTGRES_DB`
  * `POSTGRES_USER`
  * `POSTGRES_PASSWORD`
  have a look at the sample file `postgres.env.sample`
- to set the environment variables needed for the PostgRESTL instance,
create a file named `postgREST.env` and provide values for the following variables:
  * `PGRST_DB_URI` of the form:
  `postgres://[POSTGRES_USER]:[POSTGRES_PASSWORD]@postgres_container_alias:5432/[POSTGRES_DB]`
  * `PGRST_DB_SCHEMA`
  * `PGRST_DB_ANON_ROLE`
  * `PGRST_SERVER_PROXY_URI`
  * `PGRST_JWT_SECRET`
  have a look at the sample file `postgREST.env.sample` and `rsa.jwk.pub.sample`.
  The latter contains a sample secret that can be set as `PGRST_JWT_SECRET`
  and to generate JWT tokens to use as `API_TOKEN`
- the environment variables needed for the importer are:
  * `API_ENDPOINT`
  * `API_TOKEN`


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
- no backpressure login: `UPSERT`ions are sent in parallel and might overwhelm the db

The last solution seems to be the most performant in terms of both time and space
and that's the reason why I decided ot implement it.
