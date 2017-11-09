# LambdaWerk backend developer test

## Assumptions

- The database is on a remote machine therefore the program must connect to it over
the wire.
- A person is identified by all the field in the table. Rationale: first and last
name alone are not sufficient since two persons could be homonyms, adding the
date of birth makes it safer but it's still possible that the record identifies
two distinct persons. The telephone number alone is not sufficient because it is
not specified if the number is a home number - which could belong to a family
unit for example and appear in multiple persons records - or a personal number (
now in theory this can also be insufficient but this is as good as it gets given
the data we have).

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

#### Prerequisite
`ALTER` the persons table and add a `PRIMARY KEY` constraint needed by the
`ON CONFLICT` statement.

#### Algorithm description
- parse N persons from XML file (N might depend on different factors,
i.e. memory available)
- `UPSERT` N persons leveraging Postegres own `ON CONFLICT` statement
- repeat for all persons in the XML file

`UPSERT`ing would look something like:
``` sql
INSERT INTO person AS p VALUES
('JIARA','HERTZEL','1935-06-05','5859012134'),
('RONJARVIOU','COMELLO','1932-09-27','7702713416')
ON CONFLICT (fname,lname,dob,phone) DO UPDATE
SET phone = EXCLUDED.phone
WHERE p.phone != EXCLUDED.phone OR p.phone IS null;
```

#### Performance evaluation
The algorithm needs O(1) memory and O(P) `UPDATE`s / `INSERTION`s where P is
the number of persons in the XML file.

PROs: constant memory usage and no need to issue multiple `SELECT`s
CONs: the table must be modified


The last solution seems to be the most performant in terms of both time and space
and that's the reason why I decided ot implement it.
