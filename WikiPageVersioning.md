# Wiki page version history #

This document lists a few implementation strategies for implementing a wiki page revision history for a wiki that uses SQL as its storage mechanism.

## Goals ##
  * Fast page views even if a single page has been edited hundreds of times
  * Not wasting a lot of disk space or memory
  * Conflict resolution has to work in a web-based environment

## Requirements ##
  * A web page save should always succeed, even in the case of conflicts on concurrent saves
    * Conflicts are not easy to resolve in a web based environment
    * Conflict resolution is easy for programmer tools like SVN because users are programmers
  * Text search should always find only the latest version of a wiki page
  * Locking: two concurrent wiki page saves must produce a coherent DB result (i.e., saves may require table/row locking/txns/sth else)

## Approaches ##

### A1: Store full text for each revision ###
A revision is:
  * rev\_id: Revision ID
  * rev\_text: Full text of this version

Page view: read most recent revision and display full text directly.

#### Current version 1 schema + extensions ####
Bolded rows are additions to version 1.

**Table** `pages`
| **Column** | **Type** | **Modifiers** |
|:-----------|:---------|:--------------|
| id         | bigint   |  not null default nextval('pages\_id\_seq'::regclass) |
| **head\_revision**| **bigint** | **not null default 0** |
| page\_descr | character varying(256) |               |


**Table** `wikitext`
| **Column**  | **Type** | **Modifiers** |
|:------------|:---------|:--------------|
| page\_id    | bigint   |               |
| **page\_revision** | **bigint** |               |
| **page\_created** | **timestamp** | **not null default now()** |
| **page\_created\_user\_id** | **bigint** |               |
| page\_text  | text     |               |
| page\_searchv | tsvector |               |

+ index (wikitext\_index : gist (page\_searchv)
+ wikitext\_page\_id\_fkey : FOREIGN KEY (page\_id) REFERENCES pages(id)
+ wikitext\_page\_created\_by\_fkey : FOREIGN KEY (page\_created\_user\_id) REFERENCES users(id)

Code to get the latest wiki page version:

```
SELECT page_text FROM wikitext WHERE page_id = x AND page_revision = y;
```

Saving a wiki page changes from an UPDATE to an INSERT:

```
INSERT INTO wikitext (page_id,page_revision,page_text) 
            VALUES   (x, y, 'wiki page text');
```

Table `pages` needs to be updated to point to the latest revision when saving a wiki page.  Some row locking (at least) should happen when these writes are being made to protect against two concurrent saves.

Creating a new revision for an existing page that'd update both pages and wikitext relations might look something like this:

```
BEGIN;
-- Ensure no one else can update the head revision while we're modifying it
-- Selecting for UPDATE means no one else can SELECT FOR UPDATE this row.
-- If value (head_revision+1) is only computed and used inside this row lock,
-- we should be protected against two (or more) users creating the same 
-- revision head.
--
-- Note that SELECT FOR SHARE would not work here.

SELECT * from pages WHERE id = <page_id> FOR UPDATE;
INSERT INTO wikitext (page_id,page_revision,page_text)
     VALUES (<page_id>, (SELECT head_revision+1 FROM pages WHERE id = <page_id>),
             'page text ...');

-- Finally update new head_revision.  No one else should've been able to modify
-- it outside of this transaction.
UPDATE pages SET head_revision = pages.head_revision+1 WHERE id = <page_id>;
COMMIT;
```

### A2: Store diff against previously saved version ###
A revision is:
  * rev\_id Revision ID for THIS revision
  * rev\_prev\_id Previous revision ID that this revision is saved (diff'd) against
  * rev\_diff: Diff computed by diffing rev\_id -> rev\_prev\_id full text

Page view: Take first revision and apply diffs against the base until we hit the most recent version.

Conflict resolution: last save operation becomes most recent revision.

### A3: A1 but page old revisions to compressed storage ###
Store revisions just as in A1 but use a cron job that pages out old revisions to compressed storage (could use delta packing or just gzip).

A revision is same as A1.

Page view: Same as A1 except old revisions need to be restored from compressed storage.

Conflict resolution: A1.

Performance characteristics: Make it fast to read latest revision, other revisions can be viewed at significantly slower speeds.

Open issues:
  * How can this be easily implemented inside the SQL server?
  * Use cron to do this optimization every now and then?

## Approach comparison ##
A comparison of the strengths and weaknesses of the various approaches.

Scoring: 1 - Good, 2 - Average, 3 - Below avg, bad

| **Characteristic**  | **A1** | **A2** | **A3** | **Comments** |
|:--------------------|:-------|:-------|:-------|:-------------|
| Speed: Page view    | 1      | 3      | 1      | A2: assuming no last-revision-caching |
| Speed: Page view of a page with 1000 revisions| 1      | 3      | 1      | Viewing most recent version.  A2: assuming no last-revision-caching |
| Speed: Page view of a page with 1 revision| 1      | 1      | 1      |              |
| Speed: Page view of an older revision | 1      | 3      | 1      | A2: pretty much same speed as most recent, depends on how many revisions there are |
| Speed: DB ops happen autonomously on SQL server| 1      | 2      | ?      | A2: Depends on implementation, could be 1 or could be 3. |
| Disk efficiency     | 3      | 1      | 2      | A1 stores full text per revision.  A2 stores only changes. A1 and A2 becomes equivalent if all lines change and are further apart when only a very small subset of lines change.  A3 could be 1. |
| Ease of implementation: Concurrency| 1      | 3      | 1      |              |
| Ease of implementation: Text search| 1      | 2      | 1      |              |

## Implementation details ##

Nurpawiki 1.1 implements approach A1.

### Support for tsearch2 and revisions (A1) ###
Full search with `tsearch2` works out of the box although it searches by default from all revisions of all pages rather than restricting the search to the head revision.

I don't know what would be an easy way to restrict `tsearch2`'s attention to only head revisions.  I guess one option would be to nuke the search indices for old revisions when saving a page.  This might be a good idea anyway, as it would reduce the DB disk size.

**Update:** full text search can be restricted to head by killing the previous version search vectors and only setting it up for the head:

```
  let sql = "
BEGIN;
SELECT * from pages WHERE id = "^page_id_s^";

-- Set ID of next revision
UPDATE pages SET head_revision = pages.head_revision+1 
  WHERE id = "^page_id_s^";

-- Kill search vectors for previous version so that only
-- the latest version of the wikitext can be found using
-- full text search.
--
-- NOTE tsearch2 indexing trigger is set to run index updates
-- only on INSERTs and not on UPDATEs.  I wanted to be 
-- more future proof and set it trigger on UPDATE as well,
-- but I don't know how to NOT have tsearch2 trigger 
-- overwrite the below UPDATE with its own index.
UPDATE wikitext SET page_searchv = NULL WHERE page_id = "^page_id_s^";

INSERT INTO wikitext (page_id, page_created_by_user_id, page_revision, page_text)
  VALUES ("^page_id_s^", "^user_id_s^",
  (SELECT head_revision FROM pages where id = "^page_id_s^"),
  E'"^escaped^"');

COMMIT" in
```