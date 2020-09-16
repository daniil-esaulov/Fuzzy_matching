# Fuzzy matching

This was the part of the project for the Institute for Industrial and Market studies at HSE. The task was to merge two large tables: table A (consisted of ~ 33 thousand rows) was inner joined with the table B downloaded frim the Ruslana BvD database consisted of 9.5 mln rows. The difficulty was in the key_column which was the name of the company. Thus, different variations of writing this name together with possibilities of misprints in the able A lead to the necessity of upgrading the simple "merge" function.

**Results** The percentage of matches (for the unique rows) increased from 9.5% (after simple editing like lowercasing and deleting punctuation) to 65%

Due to the privacy issues I can share only the code without any data itself.

The implemented steps of the abovementioned fuzzy matching process are represented below in details. The R script is given [here](https://github.com/daniil-esaulov/Fuzzy_matching/blob/master/fuzzy_matching_ruslana.R)

## Procedural book

After downloading data from Ruslana and performing [SQL script](https://github.com/daniil-esaulov/Fuzzy_matching/blob/master/Vlookup_Ruslana_regions_id.sql) we’ve got:

1.	Folder with data files (.csv) from Ruslana grouped by regions. Each file contains information on firms in the corresponding region including: 
  a.	long and short names of the company separately; 
  b.	BvD, OKPO, INN ids; 
  c.	region, city 
  d.	status of the firm (active, closed, in the process of reorganization), year of status changing (the only date in Ruslana helpful with determining which firms had been
  already closed by the time of research), foundation year
  e.	organization form (“ООО”, “ЗАО”, “ИП” etc.)
2.	Initial file from ICSID db in .csv format
**Task:** to match the name of the firm in ICSID table with long or short name of the firm in Ruslana db in order to get basic identificators: OKPO, INN, BvD number. We account for the region and city where it is possible.

**Part 1. Editing ICSID table**

1)	Leave just the columns needed for the matching task: "reg_idnew", "inn", "name", "address", "firm"

2)	Delete all the symbols of new line and return of carriage that can affect operations with rows

3)	Fill in the blank spaces with the info for the corresponding school

4)	Assign the “row_id” to each row. It makes looking for the corresponding row in initial table easier

5)	Extract the city from the college address. Leave only the name without prefix “г.”, “село” etc. I’ve written the specific code using _regular expressions_ for that.

6)	Edit the columns “firm” and “city”. Make new columns “ed_firm” and “ed_city” with the following changes:
  a.	Change hyphen "-" and dash " - " to space " "
  b.	Separate “firm” if there are parentheses. Make 2 new columns with info inside and outside of parentheses
  c.	Remove all the punctuation
  d.	Convert to lower case
  e.	Remove multiple whitespaces and ones from start and ending of the string
  f.	Standardize the symbol "№..."
  g.	Transliterate if needed. Using specific R function “stri_trans_general” from “stringr” package
  h.	Remove all rows with blank or zero ("0") "firm" cell (it means that there are no companies corresponding to the particular school)

**Part 2. Creating subsidiary data frames**

1)	Create vector of regions from ICSID table

2)	Final report – data frame consisting of the following data:
  a.	Region
  b.	Phase of matching process (so far 7 of them)
  c.	Number of unique rows in ICSID table corresponding to the region
  d.	Number and percentage of (unique rows) matches found during the corresponding phase
  e.	Number of unique firm names in ICSID table
  f.	Number and percentage of (unique firm) matches found during the corresponding phase
  g.	Number of rows in Ruslana db corresponding to the region
  h.	Total number of rows found after the final phase

**Part 3. Matching phases**

For every region id from “region” vector do the following:

1)	Take Ruslana data file corresponding to the region

2)	Shrink Ruslana data according to the following:
  a.	Delete all the firms that were founded after Decemver, 31, 2013
  b.	Delete all the firms that have become inactive before January, 1, 2013

3)	Edit Ruslana rows (the process is similar to ICSID case)
  a.	Change hyphen "-" and dash " - " to space " "
  b.	Remove all the punctuation
  c.	Convert to lower case
  d.	Remove multiple whitespaces and ones from start and ending of the string
  e.	Standardize the symbol "№..."
  f.	Extract the name of the city. Leave only the name without prefix “г.”, “село” etc.  

For each phase the duplicated rows in the end are deleted

  _Phase 1_

  a.	Finding perfect match with long firm name in Ruslana
  b.	Finding perfect match with short firm name in Ruslana

  _Phase 2_

  a.	Adding form abbreviation to the long name of the company («ооо», «оао», «зао», «ао», «муп», «ип», «фгуп»)
  b.	Editing rows with "ип" - if #of words in the string with "ип" is more than 4 make them 4 (to leave only "ип иванов владимир викторович" instead of "ип иванов владимир викторович магазин ромашка")
  c.	Deleting initials from the name and adding "ип" to the long name of the company
  d.	Getting initials from the full name and adding "ип" to the long name of the company
  e.	Editing rows with "ип" - if #of words in the string with "ип" is more than 4 make them 4 in ICSID table (the same logics as in b)
  f.	Changing long business form to short business form. («общество с ограниченной ответственностью» to «ооо» etc.)

  _Phase 3_

  The same as in Phase 2 but for the short firm name

  _Phase 4. Working with ICSID database_

  Adding organization forms to firm names to ICSID table. (“ооо”, “зао”, “ао”, “оао”, “общество с ограниченной ответственностью” etc)

  _Phase 5_

  Matching names outside and inside parentheses using the same methods as in Phases 1-4

  _Phase 6_

  a.	Leave only rows where cities of school and firms match. If there are no firms that match to school by city then leave all the firms found
  b.	Account for business form. Delete matches if business forms don't match for sure. Leave matches if there is no correspondence in form (the same logics as in correspondence of cities)

  _Phase 7. Fuzzy Matching_

  For firm names that don’t have matches after Phases 1-6 we use Fuzzy string matching algorithm. It is based on special metrics – string distance. There are several popular ones.
  Comparison of different string distance algorithms can be found, for example, [here](http://www.joyofdata.de/blog/comparison-of-string-distance-algorithms/)

  a.	We use generalized Levenshtein (edit) distance as it has showed better results than the others. It gives the minimal possibly weighted number of insertions, deletions and substitutions needed to transform one string into another (details [here](https://stat.ethz.ch/R-manual/R-devel/library/utils/html/adist.html)). 

  We use chosen algorithm to find the best match for firm name from ICSID among long names and short names from Ruslana separately. “Best” here means less value of the distance.

  Thus, in the end we have two different tables (for each region) with matches found using fuzzy string matching algorithm (one for long names, another one for short names).

  b.	Manual coding matches found in a). Adding column Match_true. 3 possible values: 0- for clearly incorrect match, 1 – for clearly correct match, 2- if the correctness is not obvious

  c.	Create table with clearly true matches consolidating matching results for “long” and “short name” cases

After seven phases we get 4 data frames for each region: 1st  – for results from phases 1-6, 2nd  – for results from phase 7, 3rd – for firms with no matches after all phases, 4th –report with statistics (described in Part 2.2 above)

_Some Notes:_

One of the possible approaches after Phase 7: do Fuzzy matching for the firm names that we didn’t match after 7 phases. But now take Ruslana row corresponding to the second minimum (second best answer).  It looks like it works reasonably well, especially for the rows that were coded “2” at Phase 7 b). Corresponding part of coding can be found in the end of the R script.
>>>>>>> c8de2498448ca07d7f404955173086fe352f45ae
