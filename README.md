# organic-contacts

[![License](https://img.shields.io/badge/LICENSE-GPL%20v3.0-blue.svg)](https://www.gnu.org/licenses/gpl.html)  
[![ko-fi](https://ko-fi.com/img/githubbutton_sm.svg)](https://ko-fi.com/Y8Y5E5GL7)

A local address book for Emacs

## Background

Now that we don't maintain physical diaries with phone numbers against names or transfer contacts between cellphones through SIM cards, for most of us, the greater part of our address book resides with cloud service providers like Google, Apple, Microsoft, etc. while the rest of it is on other social platforms like Facebook, Twitter, Reddit, etc. Though these cloud address books help us carry our contacts across platforms and computers, without a lot of extra effort, it ends up making it decentralized.

### Problems with using web platforms

- With all our contact information **scattered** across these platforms and services, looking up a means to communicate with a person may need referring to multiple sources.
- Each of these platforms may be **limited for fields** that may be supported for a contact, such that one may not be able to store an ID or alias for a particular platform on another.
- Referring to one's address book has a **dependency on the internet**, without which a large portion of their contacts may not be accessible.

### My wish for better contact management

My "ideal" address book...

- should be a **single point of reference** containing everything including short-term and long-term contacts all in a single data file
- is **available offline**, knocking out dependency on any network
- is **available across my devices** (I can use a local sync like [Syncthing](https://syncthing.net) for this)
- supports unlimited **custom fields** so that I can store as much information about a contact as I need
- is stored as **plain text** for easy editing through any text-editing tool
- enables **searching by any field** rather than just by the key fields
- is accessible through a **familiar interface** for easy reference (and possibly edits)

Hence, I created *organic-contacts*.

## Flexible contact structure

    (("John" "Shepard") ; The first line contains the display fields, which could be First Name, Last Name, etc.
     ("me1" "me2" "me3") ; The second line is dedicated for a list of tags
     (title . "N7 Alliance Marine / Spectre") ; The rest of the lines could be any other name values pairs
     (race . "Human")
     (gender . "Male"))

## Installation

### Manual

Save the file 'organic-contacts.el' to disk and add the directory containing it to 'load-path' using a command in your '.emacs' file like:

    (add-to-list 'load-path "~/.emacs.d/")

The above line assumes that you've placed the file into the Emacs directory '.emacs.d'.

Start the package with:

    (require 'organic-contacts)

## Usage

### Loading contacts

Specify a file to load contacts from

    (organic-contacts/load/db "~/organic-contacts-db.el")

Alternatively, load the supplied sample DB to try the currently available features

    (organic-contacts/load/sample-db)

### Searching/Viewing contacts

1. **`organic-contacts/view/browse`** lets you browse through your contacts in form of a list of names. Selecting a name opens up the corresponding contact card in a new buffer.
2. **`organic-contacts/view/search`** lets you search for a contact (or contacts) that contain a specified keyword within any of their fields. One of the contacts can then be viewed upon selection.
3. **`organic-contacts/view/find-by-tag`** lets you find contacts that are tagged by a particular tag. The prompt starts with a list of tags, choosing one turns it into a list of contacts associated with that tag, followed by a contact card upon selection.

### More to come...

## To-Do

- Import from popular file formats
- Add new contacts
- Edit existing contacts
- Find potential duplicates
