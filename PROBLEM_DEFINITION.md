# LambdaWerk backend developer test

## The task

There is a PostgreSQL table of persons (person), uniquely identified
by their first name (fname), last name (lname) and date of birth
(dob).  Every person has a telephone number (phone).

This table needs to be updated from an XML file containing elements of
the form

```
<member>
 <firstname>JOHN</firstname>
 <lastname>DOE</lastname>
 <date-of-birth>2002-02-01</date-of-birth>
 <phone>9548938821</phone>
</member>
```

If the phone number is already correct, nothing should be changed in
the database.  If a person record does not exist, it needs to be
created.

The person database table contains 10 million rows.

The update file contains 1.5 million entries.

## Objective

 - Write clean code that performs the operation correctly
 - Provide basic loading statistics at the end of the operation
 - Use proper mechanisms to process the input file
 - Find ways to minimize the overall run time of the merge process
 - Reason about performance and memory usage

The number of records in the sample database and the input file are
meant to reflect the number of records in a production system.  A
production system would have more individual fields per person,
consider that when choosing an implementation strategy.

## Files

The file person.sql.gz contains a database dump of the person table
which can be imported into PostgreSQL.

The file update-file.xml.gz contains the XML input file to be merged
into the database.

---
## Clarifications
- what are "loading statistics"?
- is altering the table allowed?
- "uniquely identified by their first name (fname), last name (lname)
and date of birth (dob)" BUT there are rows where dob is null...so this
doesn't look like a valid primary key: 15 lname = NULL, 252 dob = NULL (o.O).
Can I do smth about it? Delete them, put a default value? Get all dups with:
``` sql
WITH dups AS (SELECT fname, lname FROM person GROUP BY fname, lname HAVING COUNT(*) > 1)
SELECT * FROM person
WHERE fname IN (SELECT fname FROM dups) AND lname IN (SELECT lname FROM dups)
ORDER BY fname, lname;
```

## Evaluation metrics
1. correctness
2. code cleaness
3. efficiency
   - reading file
   - executing updates (add indexes? NOPE quite the contrary)
4. evaluate performance

## Implementation strategies
1. Algo :: insert/update all from code
  - parse entire XML file
  - foreach person:
      if db.phone /= file.phone then update else if db.phone == null then create
   Perf: O(N) memory, O(1) selects, O(N) updates
         where N = #(entries in XML file)
2. Algo :: insert/update from code
  - read entry in XML file
  - select by phone
  - if db.phone /= file.phone then update else if db.phone == null then create
   Perf: O(1) memory, O(N) selects, O(N) updates or insertions
         where N = #(entries in XML file)
3. Algo :: batch insert+update from code
  - read M entries in mem
  - select by phones
  - foreach phone:
      if db.phone /= p.phone then update else if db.phone == null then create
   Perf: O(M) memory, O(N / M) selects, O(N) updates or insertions
         where N = #(entries in XML file), M = #(items in batch)
4. Algo :: upsert-all using postgres
   ALTER TABLE: Add uniqueness constraint on the `person` table or add pkey
   - parse entire XML file
   - create massive insert_on_conflict statement
5. Algo :: batch-upsert using postgres
   ALTER TABLE: Add uniqueness constraint on the `person` table or add pkey
   - read M entries in mem
   - create insert_on_conflict statement
   Perf: O(M) memory, O(N) updates or insertions
         where N = #(entries in XML file), M = #(items in batch)


## Example queries

### Using upsertions

``` sql
INSERT INTO person AS p VALUES
('JIARA','HERTZEL','1935-06-05','5859012134'),
('RONJARVIOU','COMELLO','1932-09-27','7702713416'),
('KELRICK','ASPERIN','1931-05-27','2404431156'),
('ARAFAT','REZNITSKII','1936-07-03','1112223334')
ON CONFLICT (fname,lname,dob,phone) DO UPDATE
SET phone = EXCLUDED.phone
WHERE p.phone != EXCLUDED.phone OR p.phone IS null;
```
