# vCard import/export for BBDB

`bbdb-vcard.el` imports and exports vCards (version 3.0) as defined in
RFC 2425 and RFC 2426 to/from The Insidious Big Brother Database
(BBDB).  Version 2.1 vCards are converted into version 3.0 on import.

This version only supports BBDB 3.x and Emacs 24.3 and up.

For full documentation, refer to the included Info docs.

## Usage

On a file, a buffer or a region containing one or more vCards, use
`bbdb-vcard-import-file`, `bbdb-vcard-import-buffer`, or
`bbdb-vcard-import-region` respectively to import them into BBDB.

In buffer *\*BBDB\**, press v to export the record under point.  Press `* v`
to export all records in buffer into one vCard file.  Press `* C-u v` to
export them into one file each.

To put one or all vCard(s) into the kill ring, press `V` or `* V`
respectively.

Refer to the Commentary in file bbdb-vcard.el for further information.

## Installation

### Dependencies

If you install using `package.el`, dependencies should be taken care of
automatically. Otherwise, *BBDB 3* and `cl-lib` are required.
Note that this bbdb-vcard does not work with *BBDB 2.x* and versions of
Emacs less than 24.3.

### Installing with *package.el*

The development version is available on [Melpa](http://melpa.milkbox.net).
Assuming you have added the Melpa repository as a source, the following
command will install bbdb-vcard (and its dependencies).

`M-x package-install RET bbdb-vcard RET`

### Installing from source

First checkout the repository:
```sh
git clone git://github.com/vgeddes/bbdb-vcard.git
```

Compile and Install:

```sh
make EMACS=EMACS_BIN
make install EMACS=EMACS_BIN
```

Then add this to your init file:

```lisp
(add-to-list 'load-path "/path/to/bbdb-vcard")
(require 'bbdb-vcard)
```

## Implementation

For a full treatise on the inner workings of bbdb-vcard, read the
included Info documentation.

Below is a mapping of vCard entities to their BBDB equivalents:

+-------------------------+----------------------------------------+
| *VCARD TYPE;PARAMETERS* | *STORAGE IN BBDB*                      |
| *PARAMETERS*            |                                        |
|-------------------------+----------------------------------------|
| VERSION                 | -                                      |
|-------------------------+----------------------------------------|
| N                       | First occurrence:                      |
|                         | Firstname                              |
|                         | Lastname                               |
|                         |                                        |
|                         | Rest:                                  |
|                         | AKAs (append)                          |
|-------------------------+----------------------------------------|
| FN                      | AKAs (append)                          |
| NICKNAME                | AKAs (append)                          |
|-------------------------+----------------------------------------|
| ORG                     | First occurrence:                      |
|                         | Organization                           |
|                         |                                        |
|                         | Rest:                                  |
|                         | Notes<org                              |
|                         | (repeatedly)                           |
|-------------------------+----------------------------------------|
| ADR;TYPE=x,HOME,y       | Addresses<Home                         |
| ADR;TYPE=x;TYPE=HOME    | Addresses<Home                         |
| ADR;TYPE=x,WORK,y       | Addresses<Office                       |
| ADR;TYPE=x;TYPE=WORK    | Addresses<Office                       |
| ADR;TYPE=x,y,z          | Addresses<x,y,z                        |
| ADR;TYPE=x;TYPE=y       | Addresses<x,y                          |
| ADR                     | Addresses<Office                       |
|-------------------------+----------------------------------------|
| TEL;TYPE=x,HOME,y       | Phones<Home (append)                   |
| TEL;TYPE=x;TYPE=HOME    | Phones<Home (append)                   |
| TEL;TYPE=x,WORK,y       | Phones<Office (append)                 |
| TEL;TYPE=x;TYPE=WORK    | Phones<Office (append)                 |
| TEL;TYPE=x,CELL,y       | Phones<Mobile (append)                 |
| TEL;TYPE=x;TYPE=CELL    | Phones<Mobile (append)                 |
| TEL;TYPE=x,y,z          | Phones<x,y,z (append)                  |
| TEL;TYPE=x;TYPE=y       | Phones<x,y (append)                    |
| TEL                     | Phones<Office (append)                 |
|-------------------------+----------------------------------------|
| EMAIL;TYPE=x,y,z        | Net-Addresses (append)                 |
| URL                     | Notes<www                              |
|-------------------------+----------------------------------------|
| BDAY                    | Notes<anniversary (append as birthday) |
| X-BBDB-ANNIVERSARY      | Notes<anniversary (append)             |
|-------------------------+----------------------------------------|
| NOTE                    | Notes<notes (append)                   |
| REV                     | Notes<creation-date                    |
| CATEGORIES              | Notes<mail-alias (append)              |
| SORT-STRING             | Notes<sort-string                      |
| KEY                     | Notes<key                              |
| GEO                     | Notes<geo                              |
| TZ                      | Notes<tz                               |
| PHOTO                   | Notes<photo                            |
| LABEL                   | Notes<label                            |
| LOGO                    | Notes<logo                             |
| SOUND                   | Notes<sound                            |
| TITLE                   | Notes<title                            |
| ROLE                    | Notes<role                             |
| AGENT                   | Notes<agent                            |
| MAILER                  | Notes<mailer                           |
| UID                     | Notes<uid                              |
| PRODID                  | Notes<prodid                           |
| CLASS                   | Notes<class                            |
| X-foo                   | Notes<x-foo                            |
| X-BBDB-bar              | Notes<bar                              |
|-------------------------+----------------------------------------|
| ANYJUNK;a=x;b=y         | Notes<anyjunk;a=x;b=y                  |
+-------------------------+----------------------------------------+

