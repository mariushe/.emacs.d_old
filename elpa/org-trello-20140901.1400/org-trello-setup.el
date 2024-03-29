;;; org-trello-setup.el --- Almost all constants + variables setup for org-trello namespace.
;;; Commentary:
;;; Code:

(require 'org-trello-log)

(defconst *consumer-key* nil
  "Id representing the user.")

(defconst *access-token* nil
  "Read/write access token to use trello on behalf of the user.")

(defconst *ORGTRELLO/MARKER* "orgtrello-marker"
  "A marker used inside the org buffer to synchronize entries.")

(defconst *ORGTRELLO/CARD-LEVEL* 1
  "Card level.")

(defconst *ORGTRELLO/CHECKLIST-LEVEL* 2
  "Checkbox level.")

(defconst *ORGTRELLO/ITEM-LEVEL* 3
  "Item level.")

(defconst *ORGTRELLO/OUTOFBOUNDS-LEVEL* 4
  "Out of bounds level.")

(defconst *ORGTRELLO/ACTION-SYNC* "sync-entity"
  "Possible action regarding the entity synchronization.")

(defconst *ORGTRELLO/ACTION-DELETE* "delete"
  "Possible action regarding the entity deletion.")

(defconst *ORGTRELLO/USER-PREFIX* "orgtrello-user-"
  "Org-trello prefix to define user to a 'org-mode' level.")

(defconst *ORGTRELLO/USERS-ENTRY* "orgtrello-users"
  "Org-trello property entry to store the users assigned to a card.")

(defconst *ORGTRELLO/USER-ME* "orgtrello-user-me"
  "Current user's property id.")

(defconst *ORGTRELLO/USER-LOGGED-IN*    nil
  "Current user logged in.")

(defconst *ORGTRELLO/CARD-COMMENTS* "orgtrello-card-comments"
  "Current card's comments property.")

(defconst *ORGTRELLO/CARD-COMMENTS-DELIMITER* "###"
  "Current card's comments delimiter.")

(defconst *ORGTRELLO/CARD-COMMENTS-DELIMITER-PRINT* "\n\n"
  "Current card's comments delimiter to print.")

(defconst *ORGTRELLO/DO-SHOW-CARD-COMMENTS-AFTER-ADDING* nil
  "Show the comment buffer after adding one comment.")

(defconst *ORGTRELLO/TITLE-BUFFER-INFORMATION* "*org-trello-information*"
  "Title for the org-trello buffers that display information.")

(defconst *ORGTRELLO/DEADLINE-PREFIX*   "DEADLINE:"
  "Deadline (org's equivalent to trello's due date property) prefix.")

(defconst *ORGTRELLO/HTTPS* "https://trello.com"
  "URL https to help in browsing.")

(defconst *ORGTRELLO/ERROR-SYNC-CARD-MISSING-NAME*
  "Cannot synchronize the card - missing mandatory name. Skip it...")

(defconst *ORGTRELLO/ERROR-SYNC-CHECKLIST-SYNC-CARD-FIRST*
  "Cannot synchronize the checklist - the card must be synchronized first. Skip it...")

(defconst *ORGTRELLO/ERROR-SYNC-CHECKLIST-MISSING-NAME*
  "Cannot synchronize the checklist - missing mandatory name. Skip it...")

(defconst *ORGTRELLO/ERROR-SYNC-ITEM-SYNC-CARD-FIRST*
  "Cannot synchronize the item - the card must be synchronized first. Skip it...")

(defconst *ORGTRELLO/ERROR-SYNC-ITEM-SYNC-CHECKLIST-FIRST*
  "Cannot synchronize the item - the checklist must be synchronized first. Skip it...")

(defconst *ORGTRELLO/ERROR-SYNC-ITEM-MISSING-NAME*
  "Cannot synchronize the item - missing mandatory name. Skip it...")

(defconst *ORGTRELLO/TODO* "TODO"
  "Org-mode todo state.")

(defconst *ORGTRELLO/DONE* "DONE"
  "Org-mode done state.")

(defconst *ORGTRELLO/BOARD-ID* "board-id"
  "Org-trello property board-id entry.")

(defconst *ORGTRELLO/BOARD-NAME* "board-name"
  "Org-trello property board-name entry.")

(defvar *ORGTRELLO/ORG-KEYWORD-TRELLO-LIST-NAMES* nil
  "Org-trello property names of the different lists.
This use the standard 'org-todo-keywords property from 'org-mode'.")

(defvar *ORGTRELLO/HMAP-LIST-ORGKEYWORD-ID-NAME*       nil
  "Org-trello hash map containing for each id, the associated name (or org keyword).")

(defvar *ORGTRELLO/HMAP-USERS-ID-NAME* nil
  "Org-trello hash map containing for each user name, the associated id.")

(defvar *ORGTRELLO/HMAP-USERS-NAME-ID* nil
  "Org-trello hash map containing for each user id, the associated name.")

;; make variable buffer-local
(mapc (lambda (var)
        (make-variable-buffer-local var))
      '(*ORGTRELLO/ORG-KEYWORD-TRELLO-LIST-NAMES*
        *ORGTRELLO/HMAP-LIST-ORGKEYWORD-ID-NAME*
        *ORGTRELLO/HMAP-USERS-ID-NAME*
        *ORGTRELLO/HMAP-USERS-NAME-ID*
        *ORGTRELLO/USER-LOGGED-IN*))

(defconst *ORGTRELLO/CONFIG-DIR*
  (concat (getenv "HOME") "/" ".trello"))

(defconst *ORGTRELLO/CONFIG-FILE*
  (concat *ORGTRELLO/CONFIG-DIR* "/config.el"))

(defconst *ORGTRELLO/ID* "orgtrello-id"
  "Key entry used for the trello identifier and the trello marker (the first sync).")

(defvar *ORGTRELLO/MODE-PREFIX-KEYBINDING*          "C-c o"
  "The default prefix keybinding.")

(defvar *PREVIOUS-ORGTRELLO/MODE-PREFIX-KEYBINDING* "C-c o"
  "The memory default prefix keybinding.")

(defconst *ORGTRELLO-BUFFER/INDENT-DESCRIPTION* 2
  "The default card description's indentation column.")

(defvar *org-trello-interactive-command-binding-couples* '() "List of commands and default bindings without the prefix key.")
(setq *org-trello-interactive-command-binding-couples*
      '((org-trello/version                           "v" "Display the current version installed.")
        (org-trello/install-key-and-token             "i" "Install the keys and the access-token.")
        (org-trello/install-board-metadata            "I" "Select the board and attach the todo, doing and done list.")
        (org-trello/update-board-metadata             "u" "Update the buffer's trello board metadata.")
        (org-trello/create-board-and-install-metadata "b" "Create interactively a board and attach the newly created trello board with the current org file.")
        (org-trello/check-setup                       "d" "Check that the setup is ok. If everything is ok, will simply display 'Setup ok!'.")
        (org-trello/delete-setup                      "D" "Clean up the org buffer from all org-trello informations.")
        (org-trello/sync-card                         "c" "Create/Update a complete card.")
        (org-trello/sync-buffer                       "s" "Synchronize the org-mode file to the trello board (org-mode -> trello). With prefix C-u, sync-from-trello (org-mode <- trello).")
        (org-trello/archive-cards                     "A" "Archive all DONE cards.")
        (org-trello/abort-sync                        "g" "Abort synchronization activities.")
        (org-trello/kill-entity                       "k" "Kill the entity (and its arborescence tree) from the trello board and the org buffer.")
        (org-trello/kill-cards                        "K" "Kill all the entities (and their arborescence tree) from the trello board and the org buffer.")
        (org-trello/assign-me                         "a" "Assign oneself to the card. With C-u modifier, unassign oneself from the card.")
        (org-trello/add-card-comments                 "C" "Add a comment to the card. With C-u modifier, display the current card's comments in a pop-up buffer.")
        (org-trello/show-board-labels                 "l" "Display the board's labels in a pop-up buffer.")
        (org-trello/jump-to-trello-card               "j" "Jump to card in browser.")
        (org-trello/jump-to-trello-board              "J" "Open the browser to your current trello board.")
        (org-trello/help-describing-bindings          "h" "This help message.")))

(defvar org-trello-mode-map (make-sparse-keymap)
  "Org-trello's mode map.")

(defun org-trello/compute-url (url-without-base-uri)
  "An helper method to compute the uri to trello from URL-WITHOUT-BASE-URI."
  (concat *ORGTRELLO/HTTPS* url-without-base-uri))

(defun org-trello/require-cl ()
  "Require cl lib."
  (if (version< "24.3" emacs-version)
      (require 'cl-lib)
    (progn ;; need to alias the call
      (require 'cl)
      (defalias 'cl-defun 'defun*)
      (defalias 'cl-destructuring-bind 'destructuring-bind))))

(orgtrello-log/msg *OT/DEBUG* "orgtrello-setup loaded!")

(provide 'org-trello-setup)
;;; org-trello-setup.el ends here
