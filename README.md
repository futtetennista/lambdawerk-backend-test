# LambdaWerk backend developer test

## Ubiquitous Language

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
- well-formed (referred to an entry or aperson record): that has `fname`, `lname`,
and `dob` not empty and and of the correct type
- importer: a program that takes the entries in the XML input file and asks the
database to merge them with the person records in the persons table
- merge process: described in "Problem definition"
- batch: a sub-unit consisting of a given number of entries in which the merge
process is split for efficiency reasons

### Problem definition

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
- minimize the overall run-time of the merge process
- reason about performance and memory usage of the merge process

## Running the project

```
# create and start database
docker-compose up
# compile the importer
stack build
# run the importer
stack exec importer -- update-file.xml 10000
```

### Technologies

- database: [PostgreSQL 10](https://www.postgresql.org/). I use `PL/pgSQL`
mostly to provide stats on person records created or updated during the merge process.
- importer: [Haskell](https://haskell-lang.org/). It's a purely functional
and strongly, statically typed language that allowed me to have high assurances
on correctness and cleanness of my code and develop my code in a type-driven fashion.
- database communication: [PostgREST](http://postgrest.com/) is a library
that provides an easy and convenient vai to interface with a PostgreSQL database
instance using a REST API over HTTP. It also takes care of authentication using
JWT (authorization is left to the database)
- Docker: PostgreSQL, postgREST run inside [Docker](https://www.docker.com/)
containers using Docker Compose. The importer can be run from any machine
manually or also packaged in a Docker container.

#### PROS

- Haskell ecosystem is big and provided a lot of nice tools for handling concurrency
such as Software Transactional Memory or the Async library that made expressing
relatively complex logic quite straight-forward and enjoyable
- The vast Haskell ecosystem made it also easy to write code to efficiently
read entries from in XML input file in and parse them writing a mimimum amount
of code, and to communicate to the database over REST API
- Haskell profiling tools are really enjoyable to work with and allow to easily
pin-point performance bottlenecks
- Working with the DB as a REST API was also enjoyable and helped with separation
of concerns since PostgREST forces consumers to write store procedures in the
database to define user-defined workflows. This is also good for performance

#### CONS

- Haskell is not currently used at Lambdawerk as far as I understood and this
could make it harder to get deep feedback or for the reviewer to easily understand
the code I've written
- PostgREST is a relatively simple tool that can be used as a black-box but
internally it depends on other tools and frameworks - i.e. ngnix - and these become
transitive dependencies

### Installation

- [Docker](http://docker.com/) to have a sandboxed and reproduceable infrastructure
- [Stack](https://docs.haskellstack.org/) build tool to have a sandboxed and
reproduceable Haskell environment and install all needed dependencies

### Configuration

Components need some configurations parameters that need to be supplied via
environment variables, specifically:

- to set the environment variables needed for the PostgreSQL instance, you could
create a file named `postgres.env` and provide values for the following variables:
  * `POSTGRES_DB`
  * `POSTGRES_USER`
  * `POSTGRES_PASSWORD`
  have a look at the sample file `postgres.env.sample`
- to set the environment variables needed for the PostgRESTL instance, you could
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

## Implementation considerations

While pondering and evaluating a solution, I identified three sub-problems:
1. reading and parsing the XML input file
2. serialising the content of the XML input file
3. merging entries respecting the given invariants

To achieve a *clean* architecture my design decision early on has been that
the importer will be responsable for 1. and the database for 3.

### 1. Reading and parsing the XML input file

It was clear from reading the problem definition that the XML input file was
potentially to large to be read and parsed in memory. So the alternative was to
stream it. Because lazy I/O is quite tricky and has subtleties that makes it easy
to do something wrong, I used the [conduit](https://www.stackage.org/package/conduit-combinators)
library that provides a safer (when it come to releasing resources) and more
convenient way of streaming content. The importer reads and parses `batchSize`
entries at a time, where `batchSize` is a parameter that must be configured when
invoking the importer to tell it how many entries should be parsed in one
"read & parse" pass to be submitted to the database.
When choosing a `batchSize` one should consider at least the following factors:
- how much memory is available on the machine where the importer runs
- how much memory is needed to represent a person (I wrote down details about
that in `Persons.hs:22`)
- the optimal batch size for multi-row `INSERT`s in PostgreSQL

### 3. Merging entries respecting the given invariants

The database is responsanble for merging entries supplied by the importer in a
correct and efficient way. I explored various possibilities:
* `SELECT`ing person records one-by-one and checking the invariants is hopelessly slow
* conditional `UPDATE`s don't cover the case of new person records
* `INSERT`ing using the `ON CONFLICT` clause looked promising since it's basically
what the invariants describe. The only catch here is that the `conflict_target`
must be unique. That meant adding a `PRIMARY KEY` constraint to the persons table.

Ultimately I decided to implement the last option: when the database is created
and populated the `person` table is `ALTER`ed to add a `PRIMARY KEY` but that lead
to another issue: 257 person records are not well-formed. The "Assumptions" and
"Sanitising data" sections talk about how I decided to tackle this.
The final SQL statement to merge entries looks like the following:

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
2,5 GHz Intel Core i7, 16 GB 1600 MHz DDR3. First I did some general tests
playing around with different batch size to check how that impacted performance:

* `batchSize=1000, max_wal_size=1GB` => 69.265649s (lots of connection errors:
postgREST can't keep up apparently)
* `batchSize=10000, max_wal_size=1GB` => 62.356193s (2-3 req/sec)
* `batchSize=100000, max_wal_size=1GB` => 65.404345s (1 req/~5secs)

Looking at PostgreSQL logs I noticed lots of warnings like
`checkpoints are occurring too frequently (29 seconds apart)` so I configured
PostgreSQL to use a bigger `max_wal_size`:

* `batchSize=2000, max_wal_size=2GB` => 61.797248s (20+ req/sec)
* `batchSize=5000, max_wal_size=2GB` => 64.429382s (4-5 req/sec)
* `batchSize=10000, max_wal_size=2GB` => 61.140297s (2-3 req/sec)

The warnings were gone but the running time of the merge process wasn't
changed.

Changing the batch size didn't seem to affect the total running time of the merge
process. In order to make it run quicker I could think about two strategies:

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
substantial perfomance gains
4. hardware: this is out of the scope of the assignment
5. serialisation of the output: the database return a minimal response and I don't
see this as a bottleneck

I didn't walk this road too long because from my tests it didn't seem like the
database was the bottleneck. So I moved my attention to the importer with the
goal of raising the number of requests per second that it could send to the
database focusing on:
1. deserialisation of the entries in the XML input file
2. serialisation of the entries into JSON
3. deserialisation of the database response

Here things got more interesting: while profiling the importer to optimise its
running time I actually found a space leak that prevented it from running in
constant memory and fixed it. The following image shows how memory consumption
grows linearly with the number of entries (50000 in this test)

![Importer taking linear memory](images/importer-linearmem.png)

and the following shows how the memory consumption stays mostly constant

![Importer taking constant memory](images/importer-constantmem.png)

When testing the space leak-free version of the importer I noticed that the merge
process running time increased dramatically. It turned out the importer wasn't
running batches fully in parallel, that is it was waiting for a batch to complete
before merging the next one. The way I fixed it it's described in the commit
message with SHA-1 333086d.

## General assumptions
- The database is hosted on a remote machine therefore the importer must connect
to it over the internet.
- Creating the persons table is a one-time task and the one-time costs to pay
upfront to sanitise the person records (see below) so that a `PRIMARY KEY`
constraint can be created are acceptable costs. A `PRIMARY KEY` constraint is added
because is generally a good practice to have it and allows to implement the merge
process conveniently and efficiently.
- Removing the 267 entries that do not have either a last name or a birthdate is
an unacceptable data loss.

## Sanitising data
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

## Solutions exploration

This is a short analysis of the various solutions I pondered before implementing
the final one

### 1. Reading the whole XML file

#### High-level algorithm description

- parse all entries in XML input file
- `SELECT `all person records
- for each person record check if their telephone number is different from the
one in the XML file: if so, `UPDATE` the person record otherwise do nothing
- `INSERT` all remaining entries

#### Performance evaluation

The algorithm needs O(P) memory, it would require O(1) `SELECT` and O(P) `UPDATE`s /
`INSERT`s where P is the number of entries in the XML input file.

The memory requirement are undesirable, the ideal solution would require
constant memory

### 2. Streamig the XML file

#### High-level algorithm description

- parse one entry in the XML input file
- `SELECT` the person record corresponding to the entry from the database
- if the result is empty `INSERT` the entry otherwise check if the phone
number of the person record is different from the one of the wentry: if so,
`UPDATE` the person record otherwise do nothing
- repeat for all entrie in the XML input file

#### Performance evaluation

The algorithm needs O(1) memory but would require O(P) `SELECT`s and O(P) `UPDATE`s /
`INSERTION`s where P is the number of entries in the XML input file.

Constant memory usage is good but O(P) `SELECT`s are required. That's slow and
puts too much load on the database.

### 3. Batching database operations

#### High-level algorithm description

- parse `batchSize` entries in the XML input file (`batchSize` might depend on
different factors, i.e. memory available)
- `SELECT` `batchSize` person records
- for each person record check if their telephone number is different from the
one in the XML file: if so, `UPDATE` the person record otherwise do nothing
- `INSERT` all remaining entries
- repeat for all entris in the XML input file

#### Performance evaluation

The algorithm needs O(1) memory and it would require O(P / M) `SELECT`s and O(P)
`UPDATE`s / `INSERTION`s where P is the number of entries in the XML input file
and `batchSize` is the fixed batch size - this might depend on different factors,
i.e. memory available.

Constant memory usage is good but again O(P / N) `SELECT`s are required. That's
better than the previous algorithm but still potentially slow. Is it possible to
avoid this step completely?


### 4. Batch-upserting

#### Prerequisites

- `UPDATE` the persons table and add default `lname`s and `dob`s where missing
in order to be able to add a primary key constraint
- `ALTER` the persons table and add a `PRIMARY KEY` constraint needed by the
`ON CONFLICT` statement.

#### High-level algorithm description

- parse `batchSize` entries from XML input file (`batchSize` might depend on
different factors, i.e. memory available)
- `UPSERT` `batchSize` entries leveraging the database own merge capabilities
- repeat for all entries in the XML input file

Merging would look like this in SQL:
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
- leverage native merging capabilities of the database to:
  - simplify the importer logic
  - avoid the need to do any (slow) I/O to `SELECT` person records first to apply
  merging in the client
  - achieve good separation of concerns:
    - importer parses the XML input file and sends the entries to the database
    - database handles the merge logic

##### CONs

- there is a one-time cost to pay to sanitise the data in the persons table
- there is a one-time cost to pay to add a `PRIMARY KEY` constraint in the
persons table


## Not implemented

The following problems are not solved in satisfactory fashion, mostly because of
time constraints but I wanted to mention them since I did ponder them.

### Backpressure

The importer doesn't have any short-circuiting when it come to temporarly halting
or slowing down the merge process. This might overwhelm the database in case for
some reason it cannot keep up.

### Error handling

If the database is down or the is any other issue that prevents the request from
reaching the database, the importer should either store the failed entries - i.e.
in an `update-failed.xml` file - to retry later or be manually re-submitted.

### Tests
- I'm not entirely sure how merge can be tested in an automated fashion.
That would include spinning up the infrastructure that holds the database and
invoke its `merge` function and then check the returned stats and query the database.
- Fuzzy testing on parsing / serialising entries
