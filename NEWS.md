# NEWS for BBDB vCard 

This fork of bbdb-vcard was created from http://github.com/trebb/bbdb-vcard
on 2013-12-01.

Please refer to COPYING for a historical listing of authors as well as
licensing information.

## Git Master (2013-12-08)

Authors: Vincent Geddes

* Support for BBDB 3.0

* Inline base64-encoded photos, sounds, and cryptographic keys
  are imported into local filesystem, and their filenames, relative to
  `bbdb-vcard-directory`, are stored under the following BBDB xfields,
  respectively:

  - image-filename
  - sound-filename
  - gpg-key-filename

  Upon package initialization, if the `bbdb-image` variable is `nil`, it
  will be set to `bbdb-vcard-image-basename`. BBDB will use this function to
  locate image files in `bbdb-image-path` when displating records.

* vCard URIs for images, sounds, and cryptographic keys are stored under
  the following BBDB xfields:
  - image-uri
  - sound-uri
  - gpg-key-uri

* Introduced customized variable `bbdb-vcard-directory`, representing
  the directory under which media objects are stored.

* Parse vCard name prefixes and suffixes in the `N` field and store in the new
  BBDB 3.0 `affixes` field.

* Discard the REV vCard field, its intent is meaningless in BDBB. The previous
  behavior was to set the `creation-date` xfield to REV.

* Moved most of existing inline documentation into an Info user manual.

* Added Makefile for manual installation. The compiled elisp files
  are installed under the default `site-lisp` directory under the
  directory represented by the Makefile `PREFIX` variable.

* Fleshed out README.md for the benefit of casual users.

* Internal changes:
  - Internals rewrite, so as to use the external interfaces provided
    by BBDB 3.0 where possible.
  - Use the field merging algorithms in BBDB where possible.
  - Fixes for vCard parameter/value parsing. For example,
    don't look for parameters/values in base64 encoded field
    content.
  - Recognize and parse `VALUE` parameters in vCard fields.
  - Migrate test infrastructure to `ERT`, where possible.
  - Port to `cl-lib`
  - Depends on Emacs 24.3 and above
