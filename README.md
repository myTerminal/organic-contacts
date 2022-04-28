# organic-contacts

[![License](https://img.shields.io/badge/LICENSE-GPL%20v3.0-blue.svg)](https://www.gnu.org/licenses/gpl.html)

A local address-book for Emacs

## Background

(Coming soon...)

## Installation

### Manual

Save the file 'organic-contacts.el' to disk and add the directory containing it to 'load-path' using a command in your '.emacs' file like:

    (add-to-list 'load-path "~/.emacs.d/")

The above line assumes that you've placed the file into the Emacs directory '.emacs.d'.

Start the package with:

    (require 'organic-contacts)

## Usage

Specify file to load data from

    (organic-contacts/load/db "~/organic-contacts-db.el")

Alternatively, load the supplied sample DB

    (organic-contacts/load/sample-db)

And add a set of key-bindings

    (global-set-key (kbd "C-*") 'organic-contacts/view/browse)
    (global-set-key (kbd "C-&") 'organic-contacts/view/search)
    (global-set-key (kbd "C-^") 'organic-contacts/view/find-by-tag)

## To-Do

- Import from popular file formats
