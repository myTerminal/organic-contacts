;;; organic-contacts.el --- A local address-book for Emacs

;; This file is not part of Emacs

;; Author: Mohammed Ismail Ansari <team.terminal@gmail.com>
;; Version: 0.9
;; Keywords: pim
;; Maintainer: Mohammed Ismail Ansari <team.terminal@gmail.com>
;; Created: 2021/07/25
;; Package-Requires: ((emacs "27") (cl-lib "0.5"))
;; Description: A local address-book for Emacs
;; URL: https://myterminal.me
;; Compatibility: Emacs27


;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details.
;;

;;; Install:

;; Put this file on your Emacs-Lisp load path, add following into your
;; ~/.emacs startup file
;;
;;     (require 'organic-contacts)
;;
;; Specify file to load data from
;;
;;     (organic-contacts-load-db "~/organic-contacts-db.el")
;;
;; Alternatively, load the supplied sample DB
;;
;;     (organic-contacts-load-sample-db)
;;
;; And add a set of key-bindings
;;
;;     (global-set-key (kbd "C-*") 'organic-contacts-browse-contacts)
;;     (global-set-key (kbd "C-&") 'organic-contacts-lookup-people)
;;

;;; Commentary:

;;     You can use this package work with a local plain-text address-book that
;;     can hold contact data across practically infinite data-fields. All you
;;     have to do is to set a database file path, and you should be ready to use
;;     your local address-book.
;;
;;  Overview of features:
;;
;;     o   Browse your contacts either through the inbuild ido mode, or
;;         third-party completion packages like ivy
;;     o   Search through your address-book to view a contact card
;;

;;; Code:

(require 'cl-lib)

(defvar organic-contacts--base-path
  (file-name-directory load-file-name))

(defvar organic-contacts--db-path
  nil)

(defvar organic-contacts--data
  ())

(defun organic-contacts--get-contact-visual-name (contact)
  "Gets a contact's visual name."
  (cl-reduce (lambda (a b)
               (concat a " " b))
             (car contact)))

(defun organic-contacts--get-contact-card-buffer-name (contact)
  "Gets a contact's visual name."
  (concat "Contact: "
          (organic-contacts--get-contact-visual-name contact)))

(defun organic-contacts--contact-card-show (contact)
  "Shows the specified contact in a special buffer."
  (let* ((new-buffer (get-buffer-create (organic-contacts--get-contact-card-buffer-name contact))))
    (with-current-buffer new-buffer
      (cl-flet* ((print-contact-title (contact)
                                      (insert (concat (propertize (organic-contacts--get-contact-visual-name contact)
                                                                  'face
                                                                  '(:height 2.0))
                                                      "\n\n")))
                 (print-contact-details (contact)
                                        (mapc (lambda (field)
                                                (insert (concat (propertize (capitalize (symbol-name (car field)))
                                                                            'face
                                                                            '(:height 1.2 :weight bold))
                                                                ": "))
                                                (insert (concat (propertize (cdr field)
                                                                            'face
                                                                            '(:height 1.2))
                                                                "\n")))
                                              (cdr contact)))
                 (print-other-details ()
                                      (insert (propertize "\n(Press 'q' to close)"
                                                          'face
                                                          '(:slant italic))))
                 (bind-close-key ()
                                 (local-set-key (kbd "q")
                                                (lambda ()
                                                  (interactive)
                                                  (organic-contacts--contact-card-hide))))
                 (show-contact-card-buffer ()
                                           (set-window-buffer (get-buffer-window)
                                                              new-buffer)))
        (print-contact-title contact)
        (print-contact-details contact)
        (print-other-details)
        (organic-contacts-contact-mode)
        (bind-close-key)
        (show-contact-card-buffer)))))

(defun organic-contacts--contact-card-hide ()
  "Hides an open contact card."
  (kill-buffer))

(defun organic-contacts--find-matching-entries (contacts search-term)
  "Returns a list of contacts with fields matching the search term."
  (cl-flet* ((contains-matching-value-p (list)
                                        (cl-remove-if-not (lambda (value)
                                                            (string-match-p (regexp-quote search-term)
                                                                            value))
                                                          list))
             (value-matches-p (pair)
                              (string-match-p (regexp-quote search-term)
                                              (cdr pair)))
             (fields-match-p (contact)
                             (or (contains-matching-value-p (car contact))
                                 (cl-remove-if-not #'value-matches-p
                                                   (cdr contact)))))
    (cl-remove-if-not #'fields-match-p
                      contacts)))

(defun organic-contacts--present-choice-of-contacts (collection)
  "Presents a choice of contacts using either ivy or ido"
  (cl-flet* ((pick-matching-contact (contacts selection)
                                    (car (cl-remove-if-not (lambda (contact)
                                                             (equal (organic-contacts--get-contact-visual-name contact)
                                                                    selection))
                                                           contacts)))
             (show-contact (selection)
                           (organic-contacts--contact-card-show (pick-matching-contact collection
                                                                                    selection))))
    (let* ((choices (mapcar #'organic-contacts--get-contact-visual-name
                            collection)))
      (if (featurep 'ivy)
          (let* ((ivy-wrap t))
            (ivy-read "Select a Contact to view: "
                      choices
                      :action #'show-contact))
        (show-contact (ido-completing-read "Select a Contact to view: "
                                           choices))))))

(defun organic-contacts--show-contact-list-to-browse (collection)
  "Shows a list of contacts and opens a contact card foxr the selection."
  (cond ((equal (length collection)
                0) (message "organic-contacts: No contacts to show!"))
        ((equal (length collection)
                1) (organic-contacts--contact-card-show (car collection)))
        (t (organic-contacts--present-choice-of-contacts collection))))

;;;###autoload
(defun organic-contacts-load-db (file-path)
  "Loads the specified DB file"
  (setq organic-contacts--db-path
        file-path)
  (load-file file-path)
  (message (concat "organic-contacts: Loaded "
                   (number-to-string (length organic-contacts--data))
                   " records!")))

;;;###autoload
(defun organic-contacts-load-sample-db ()
  "Loads included sample database"
  (interactive)
  (organic-contacts-load-db (expand-file-name "example/organic-contacts-db.el"
			                               organic-contacts--base-path)))

;;;###autoload
(defun organic-contacts-browse-contacts ()
  "Presents a list of contacts to choose from."
  (interactive)
  (organic-contacts--show-contact-list-to-browse organic-contacts--data))

;;;###autoload
(defun organic-contacts-lookup-people (search-term)
  "Accepts a search term and prompts to select from matching contacts."
  (interactive "sEnter search term: ")
  (let* ((matching-contacts (organic-contacts--find-matching-entries organic-contacts--data
                                                                  search-term)))
    (organic-contacts--show-contact-list-to-browse matching-contacts)))

(define-derived-mode organic-contacts-contact-mode
  special-mode
  "organic-contacts-contact"
  :abbrev-table nil
  :syntax-table nil)

(provide 'organic-contacts)

;;; organic-contacts.el ends here
