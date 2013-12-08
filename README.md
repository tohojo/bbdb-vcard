# BBDB vCard

`bbdb-vcard.el` imports and exports vCards (version 3.0) as defined in
RFC 2425 and RFC 2426 to/from The Insidious Big Brother Database
(BBDB). Version 2.1 vCards are converted into version 3.0 on import.

This version only supports **BBDB 3.x** and requires **Emacs 24.3** or higher.

For full documentation, refer to the included Info docs.

## Usage

On a file, a buffer or a region containing one or more vCards, use
`bbdb-vcard-import-file`, `bbdb-vcard-import-buffer`, or
`bbdb-vcard-import-region` respectively to import them into BBDB.

In the BBDB buffer, press `v` to export the record under point.  Press `* v`
to export all records in buffer into one vCard file.  Press `* C-u v` to
export them into one file each.

To put one or all vCard(s) into the kill ring, press `V` or `* V`
respectively.

### vCard Media Objects

The importer stores inline base64-encoded images, sounds, and
cryptographic keys to the local disk under the `bbdb-vcard-directory`
directory. The relative filenames for these objects are stored in
the following BBDB xfields, respectively:

* image-filename: `media/image-<sha1sum>.<suffix>`
* sound-filename: `media/sound-<sha1sum>.<suffix>`
* gpg-key-filename: `media/key-<sha1sum>.<suffix>`

If the variable `bbdb-image` is uncustomized when bbdb-vcard is initialized,
it will be set to `bbdb-vcard-image-basename`. This will allow to BBDB to locate
images when displaying records.

## Installation

### Dependencies

If you install using `package.el`, dependencies should be taken care of
automatically. Otherwise, BBDB 3 and `cl-lib` are required.

### Installing with **package.el**

The development version is available on [Melpa](http://melpa.milkbox.net).
Assuming you have added Melpa as a package source, the following
command will install bbdb-vcard (and its dependencies).

`M-x package-install RET bbdb-vcard RET`

### Installing from Source

First, checkout the repository:
```sh
git clone git://github.com/vgeddes/bbdb-vcard.git
```

Use the included Makefile to compile and install the package. By default
bbdb-vcard is installed in `/usr/local/share/emacs/site-lisp/bbdb-vcard`,
You can alter the install location by passing the `PREFIX` parameter
to `make`. The choice of Emacs for byte-code compilation can be overrided
by passing the `EMACS` parameter.

```sh
make
make install
```

Then add the following code to your init file. if you installed
bbdb-vcard in a non-standard location. You may need to modify your
`load-path` accordingly. 

```lisp
(require 'bbdb-vcard)
```
