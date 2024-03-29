;;; org-trello-buffer.el --- Manipulation functions of org-trello buffer
;;; Commentary:
;;; Code:

(require 'org-trello-setup)
(require 'org-trello-log)
(require 'org-trello-hash)
(require 'org-trello-data)
(require 'org-trello-query)
(require 'org-trello-entity)
(require 'org-trello-cbx)
(require 'org-trello-backend)

(org-trello/require-cl)

(defun orgtrello-buffer/org-entry-put! (point property value)
  "Put at POINT the PROPERTY with VALUE.
If the VALUE is nil or empty, remove such PROPERTY."
  (if (or (null value) (string= "" value))
      (orgtrello-buffer/delete-property-from-entry! property)
    (org-entry-put point property value)))

(defun orgtrello-buffer/extract-description-from-current-position! ()
  "Given the current position, extract the text content of current card."
  (let* ((start (orgtrello-entity/card-description-start-point!))
         (end   (orgtrello-entity/card-metadata-end-point!)))
    (when (< start end)
      (->> (buffer-substring-no-properties start end)
        s-lines
        (--map (if (s-equals? "" it) it (substring it *ORGTRELLO-BUFFER/INDENT-DESCRIPTION*)))
        (s-join "\n")))))

(defun orgtrello-buffer/get-card-comments! ()
  "Retrieve the card's comments. Can be nil if not on a card."
  (orgtrello-buffer/org-entry-get (point) *ORGTRELLO/CARD-COMMENTS*))

(defun orgtrello-buffer/put-card-comments! (comments)
  "Retrieve the card's comments. Can be nil if not on a card."
  (orgtrello-buffer/org-entry-put! (point) *ORGTRELLO/CARD-COMMENTS* comments))

(defun orgtrello-buffer/org-get-property (property-key properties)
  "Retrieve the PROPERTY-KEY in PROPERTIES."
  (assoc-default property-key properties))

(defun orgtrello-buffer/org-file-get-property! (property-key)
  "Return the property key present in the org buffer."
  (orgtrello-buffer/org-get-property property-key (orgtrello-buffer/org-file-properties!)))

(defun orgtrello-buffer/board-name! ()
  "Compute the board's name"
  (orgtrello-buffer/org-file-get-property! *ORGTRELLO/BOARD-NAME*))

(defun orgtrello-buffer/board-id! ()
  "Compute the board's id"
  (orgtrello-buffer/org-file-get-property! *ORGTRELLO/BOARD-ID*))

(defun orgtrello-buffer/me! ()
  "Compute the board's current user"
  (orgtrello-buffer/org-file-get-property! *ORGTRELLO/USER-ME*))

(defun orgtrello-buffer/labels! ()
  "Compute the board's current labels and return it as an association list."
  (mapcar (lambda (color) `(,color . ,(orgtrello-buffer/org-file-get-property! color))) '(":red" ":blue" ":orange" ":yellow" ":purple" ":green")))

(defun orgtrello-buffer/pop-up-with-content! (title body-content)
  "Compute a temporary buffer *ORGTRELLO/TITLE-BUFFER-INFORMATION* with the title and body-content."
  (with-temp-buffer-window
   *ORGTRELLO/TITLE-BUFFER-INFORMATION* nil nil
   (progn
     (temp-buffer-resize-mode 1)
     (insert (format "%s:\n\n%s" title body-content)))))

(defun orgtrello-buffer/set-property-comment! (comments)
  "Update comments property."
  (orgtrello-buffer/org-entry-put! nil *ORGTRELLO/CARD-COMMENTS* comments))

(defun orgtrello-buffer/write-item! (item-id entities)
  "Write the item to the org buffer."
  (->> entities
    (gethash item-id)
    (orgtrello-buffer/write-entity! item-id)))

(defun orgtrello-buffer/write-checklist-header! (entity-id entity)
  "Write the checklist data and properties without its structure."
  (orgtrello-buffer/write-entity! entity-id entity))

(defun orgtrello-buffer/write-checklist! (checklist-id entities adjacency)
  "Write the checklist and its structure inside the org buffer."
  (orgtrello-buffer/write-checklist-header! checklist-id (gethash checklist-id entities))
  (--map (orgtrello-buffer/write-item! it entities) (gethash checklist-id adjacency)))

(defun orgtrello-buffer/update-member-ids-property! (entity)
  "Update the users assigned property card entry."
  (--> entity
    (orgtrello-data/entity-member-ids it)
    (orgtrello-buffer/--csv-user-ids-to-csv-user-names it *ORGTRELLO/HMAP-USERS-ID-NAME*)
    (replace-regexp-in-string *ORGTRELLO/USER-PREFIX* "" it)
    (orgtrello-buffer/set-usernames-assigned-property! it)))

(defun orgtrello-buffer/update-property-card-comments! (entity)
  "Update last comments "
  (->> entity
    orgtrello-data/entity-comments
    orgtrello-data/comments-to-list
    orgtrello-buffer/set-property-comment!))

(defun orgtrello-buffer/write-unknown-properties! (unknown-properties)
  "Write the alist UNKNOWN-PROPERTIES inside standard properties org drawer."
  (mapc (lambda (property)
          (let ((key (car property))
                (value (cdr property)))
            (orgtrello-buffer/org-entry-put! (point) key value)))
        unknown-properties))

(defun orgtrello-buffer/--write-card-description! (description)
  "Write at point the current card's DESCRIPTION if present (and indent it)."
  (when description
    (let ((start (point)))
      (insert (format "%s" description))
      (indent-rigidly start (point) *ORGTRELLO-BUFFER/INDENT-DESCRIPTION*))))

(defun orgtrello-buffer/write-card-header! (card-id card)
  "Given a card entity, write its data and properties without its structure."
  (orgtrello-buffer/write-entity! card-id card)
  (orgtrello-buffer/update-member-ids-property! card)
  (orgtrello-buffer/update-property-card-comments! card)
  (orgtrello-buffer/write-unknown-properties! (orgtrello-data/entity-unknown-properties card))
  (orgtrello-buffer/--write-card-description! (orgtrello-data/entity-description card)))

(defun orgtrello-buffer/write-card! (card-id card entities adjacency)
  "Write the card and its structure inside the org buffer."
  (orgtrello-buffer/write-card-header! card-id card)
  (insert "\n")
  (-when-let (checklists (gethash card-id adjacency))
    (--map (orgtrello-buffer/write-checklist! it entities adjacency) checklists)))

(defun orgtrello-buffer/write-entity! (entity-id entity)
  "Write the entity in the buffer to the current position. Move the cursor position."
  (orgtrello-log/msg *OT/INFO* "Synchronizing entity '%s' with id '%s'..." (orgtrello-data/entity-name entity) entity-id)
  (insert (orgtrello-buffer/--compute-entity-to-org-entry entity))
  (when entity-id (orgtrello-buffer/--update-property entity-id (not (orgtrello-data/entity-card-p entity)))))

(defun orgtrello-buffer/clean-region! (region)
  "Given a region, remove everything in this region, including text and overlays"
  (apply 'orgtrello-buffer/remove-overlays! region)
  (apply 'delete-region region))

(defun orgtrello-buffer/--csv-user-ids-to-csv-user-names (csv-users-id users-id-name)
  "Given a CSV-USERS-ID and a USERS-ID-NAME map, return a csv usernames."
  (->> csv-users-id
    orgtrello-data/--users-from
    (--map (gethash it users-id-name))
    orgtrello-data/--users-to))

(defun orgtrello-buffer/--compute-entity-to-org-entry (entity)
  "Given an ENTITY, compute its org representation."
  (funcall
   (cond ((orgtrello-data/entity-card-p entity)      'orgtrello-buffer/--compute-card-to-org-entry)
         ((orgtrello-data/entity-checklist-p entity) 'orgtrello-buffer/--compute-checklist-to-org-entry)
         ((orgtrello-data/entity-item-p entity)      'orgtrello-buffer/--compute-item-to-org-entry))
   entity))

(defun orgtrello-buffer/--compute-due-date (due-date)
  "Compute the format of the DUE-DATE."
  (if due-date (format "%s <%s>\n" *ORGTRELLO/DEADLINE-PREFIX* due-date) ""))

(defun orgtrello-buffer/--private-compute-card-to-org-entry (name status due-date tags)
  "Compute the org format of a card with NAME, STATUS, DUE-DATE and TAGS."
  (let ((prefix-string (format "* %s %s" (if status status *ORGTRELLO/TODO*) name)))
    (format "%s%s\n%s" prefix-string (orgtrello-buffer/--serialize-tags prefix-string tags) (orgtrello-buffer/--compute-due-date due-date))))

(defun orgtrello-buffer/--serialize-tags (prefix-string tags)
  "Given a PREFIX-STRING and TAGS, compute the 'org-mode' serialization string.
If tags is empty, return an empty string.
If PREFIX-STRING's length is superior to 72, return tags.
Otherwise, return the tags with as much space needed to start the tags at position 72."
  (if (or (null tags) (string= "" tags))
      ""
    (let ((l (length prefix-string)))
      (format "%s%s" (if (< 72 l) " " (orgtrello-buffer/--symbol " " (- 72 l))) tags))))

(defun orgtrello-buffer/--compute-card-to-org-entry (card)
  "Given a CARD, compute its 'org-mode' entry equivalence."
  (orgtrello-buffer/--private-compute-card-to-org-entry
   (orgtrello-data/entity-name card)
   (orgtrello-data/entity-keyword card)
   (orgtrello-data/entity-due card)
   (orgtrello-data/entity-tags card)))

(defun orgtrello-buffer/--symbol (sym n)
  "Compute the repetition of a symbol SYM N times as a string."
  (--> n
    (-repeat it sym)
    (s-join "" it)))

(defun orgtrello-buffer/--space (n)
  "Given a level, compute N times the number of spaces for an org checkbox entry."
  (orgtrello-buffer/--symbol " "  n))

(defun orgtrello-buffer/--compute-state-checkbox (state)
  "Compute the STATE of the checkbox."
  (orgtrello-data/--compute-state-generic state '("[X]" "[-]")))

(defun orgtrello-buffer/--compute-level-into-spaces (level)
  "LEVEL 2 is 0 space, otherwise 2 spaces."
  (if (equal level *ORGTRELLO/CHECKLIST-LEVEL*) 0 2))

(defun orgtrello-buffer/--compute-checklist-to-org-checkbox (name &optional level status)
  "Compute checklist with NAME and optional LEVEL and STATUS to the org checkbox format."
  (format "%s- %s %s\n"
          (-> level
            orgtrello-buffer/--compute-level-into-spaces
            orgtrello-buffer/--space)
          (orgtrello-buffer/--compute-state-checkbox status)
          name))

(defun orgtrello-buffer/--compute-item-to-org-checkbox (name &optional level status)
  "Compute item with NAME and optional LEVEL and STATUS to the org checkbox format."
  (format "%s- %s %s\n"
          (-> level
            orgtrello-buffer/--compute-level-into-spaces
            orgtrello-buffer/--space)
          (orgtrello-data/--compute-state-item-checkbox status)
          name))

(defun orgtrello-buffer/--compute-checklist-to-org-entry (checklist &optional orgcheckbox-p)
  "Given a CHECKLIST, compute its 'org-mode' entry equivalence.
The optional ORGCHECKBOX-P is not used."
  (orgtrello-buffer/--compute-checklist-to-org-checkbox (orgtrello-data/entity-name checklist) *ORGTRELLO/CHECKLIST-LEVEL* "incomplete"))

(defun orgtrello-buffer/--compute-item-to-org-entry (item)
  "Given a checklist ITEM, compute its 'org-mode' entry equivalence."
  (orgtrello-buffer/--compute-item-to-org-checkbox (orgtrello-data/entity-name item) *ORGTRELLO/ITEM-LEVEL* (orgtrello-data/entity-keyword item)))

(defun orgtrello-buffer/--put-card-with-adjacency (current-meta entities adjacency)
  "Deal with adding the CURRENT-META in ENTITIES and ADJACENCY."
  (-> current-meta
    (orgtrello-buffer/--put-entities entities)
    (list adjacency)))

(defun orgtrello-buffer/--dispatch-create-entities-map-with-adjacency (entity)
  "Given the ENTITY, return the function to add the entity and adjacency."
  (if (orgtrello-data/entity-card-p entity) 'orgtrello-buffer/--put-card-with-adjacency 'orgtrello-backend/--put-entities-with-adjacency))

(defun orgtrello-buffer/build-org-card-structure! (buffer-name)
  "Build the card structure on the current BUFFER-NAME at current point.
No synchronization is done."
  (->> (orgtrello-entity/compute-card-region!)
    (cons buffer-name)
    (apply 'orgtrello-buffer/build-org-entities!)))

(defun orgtrello-buffer/build-org-entities! (buffer-name &optional region-start region-end)
  "Compute the current entities hash from the BUFFER-NAME.
Return the list of entities map and adjacency map in this order.
If REGION-START and REGION-END are provided, this will work on such defined region."
  (with-current-buffer buffer-name
    (save-excursion
      (save-restriction
        (let ((entities (orgtrello-hash/empty-hash))
              (adjacency (orgtrello-hash/empty-hash)))
          (when (and region-start region-end) (narrow-to-region region-start region-end))
          (orgtrello-buffer/org-map-entities-without-params!
           (lambda ()
             (org-show-subtree) ;; unfold every entries, otherwise https://github.com/org-trello/org-trello/issues/53
             (let* ((full-meta (orgtrello-buffer/entry-get-full-metadata!))
                    (entity    (orgtrello-data/current full-meta)))
               (unless (-> entity orgtrello-data/entity-id orgtrello-data/id-p) ;; if no id, we set one
                 (let ((marker (orgtrello-buffer/--compute-marker-from-entry entity)))
                   (orgtrello-buffer/--set-marker! marker)        ;; set the marker
                   (orgtrello-data/put-entity-id marker entity)   ;; update the entity with its id/marker
                   (orgtrello-data/put-current entity full-meta)));; update the full-meta data with the new entity
               (--> entity
                 (orgtrello-buffer/--dispatch-create-entities-map-with-adjacency it)
                 (funcall it full-meta entities adjacency)))))
          (list entities adjacency))))))

(defun orgtrello-buffer/--put-entities (current-meta entities)
  "Deal with adding a the current entry from CURRENT-META in ENTITIES."
  (-> current-meta
    orgtrello-data/current
    (orgtrello-backend/--add-entity-to-entities entities)))

(defun orgtrello-buffer/--update-property (id orgcheckbox-p)
  "Update the property identifier with ID if depending on ORGCHECKBOX-P.
Move the cursor position."
  (if orgcheckbox-p
      (save-excursion
        (forward-line -1) ;; need to get back one line backward for the checkboxes as their properties is at the same level (otherwise, for headings we do not care)
        (orgtrello-buffer/set-property *ORGTRELLO/ID* id))
    (orgtrello-buffer/set-property *ORGTRELLO/ID* id)))

(defun orgtrello-buffer/--set-marker! (marker)
  "Set a MARKER to get back to later."
  (orgtrello-buffer/set-property *ORGTRELLO/ID* marker))

(defun orgtrello-buffer/set-marker-if-not-present! (entity marker)
  "Set the CURRENT-ENTITY with MARKER to the entry if we never did."
  (unless (string= (orgtrello-data/entity-id entity) marker) ;; if never created before, we need a marker to add inside the file
    (orgtrello-buffer/--set-marker! marker)))

(defun orgtrello-buffer/org-map-entities-without-params! (fn-to-execute)
  "Execute fn-to-execute function for all entities from buffer - fn-to-execute is a function without any parameters."
  (org-map-entries
   (lambda ()
     (funcall fn-to-execute) ;; execute on heading entry
     (orgtrello-cbx/map-checkboxes fn-to-execute))))

(defun orgtrello-buffer/get-usernames-assigned-property! ()
  "Read the org users property from the current entry."
  (org-entry-get nil *ORGTRELLO/USERS-ENTRY*))

(defun orgtrello-buffer/set-usernames-assigned-property! (csv-users)
  "Update users org property."
  (orgtrello-buffer/org-entry-put! nil *ORGTRELLO/USERS-ENTRY* csv-users))

(defun orgtrello-buffer/delete-property-from-entry! (property)
  "Delete a property from the org buffer."
  (org-delete-property property))

(defun orgtrello-buffer/delete-property! (property)
  "Given a property name (checkbox), if found, delete it from the buffer."
  (orgtrello-buffer/delete-property-from-entry! property)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ":PROPERTIES: {.*" nil t)
      (remove-overlays (point-at-bol) (point-at-eol)) ;; the current overlay on this line
      (replace-match "" nil t))))                     ;; then remove the property

(defun orgtrello-buffer/remove-overlays! (&optional start end)
  "Remove every org-trello overlays from the current buffer.
When START/END are specified, use those boundaries.
Otherwise, work on the all buffer."
  (remove-overlays (if start start (point-min)) (if end end (point-max)) 'invisible 'org-trello-cbx-property))

(defun orgtrello-buffer/install-overlays! ()
  "Install overlays throughout the all buffers."
  (orgtrello-buffer/remove-overlays!)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ":PROPERTIES: {.*" nil t)
      (orgtrello-buffer/install-overlay! (match-beginning 0)))))

(defun orgtrello-buffer/indent-card-descriptions! ()
  "Indent the card descriptions rigidly starting at 2."
  (when (eq major-mode 'org-mode)
    (save-excursion
      (org-map-entries (lambda () "Indent the description from the current card if need be."
                         (let ((start (orgtrello-entity/card-description-start-point!))
                               (end   (orgtrello-entity/card-metadata-end-point!)))
                           (save-restriction
                             (narrow-to-region start end)
                             (goto-char (point-min))
                             (unless (<= 2 (org-get-indentation));; if need be
                               (indent-rigidly start end *ORGTRELLO-BUFFER/INDENT-DESCRIPTION*)))))))));; now indent with the rightful indentation

(defun orgtrello-buffer/--convert-orgmode-date-to-trello-date (orgmode-date)
  "Convert the 'org-mode' deadline ORGMODE-DATE into a time adapted for trello."
  (if (and orgmode-date (not (string-match-p "T*Z" orgmode-date)))
      (cl-destructuring-bind (sec min hour day mon year dow dst tz)
          (--map (if it (if (< it 10) (concat "0" (int-to-string it)) (int-to-string it)))
                 (parse-time-string orgmode-date))
        (concat (concat year "-" mon "-" day "T") (if hour (concat hour ":" min ":" sec) "00:00:00") ".000Z"))
    orgmode-date))

(defun orgtrello-buffer/org-entity-metadata! ()
  "Compute the metadata the org-mode way."
  (org-heading-components))

(defun orgtrello-buffer/--extract-metadata! ()
  "Extract the current metadata depending on the org-trello's checklist policy."
  (funcall (if (orgtrello-entity/org-checkbox-p!) 'orgtrello-cbx/org-checkbox-metadata! 'orgtrello-buffer/org-entity-metadata!)))

(defun orgtrello-buffer/extract-identifier! (point)
  "Extract the identifier from POINT."
  (orgtrello-buffer/org-entry-get point *ORGTRELLO/ID*))

(defun orgtrello-buffer/set-property (key value)
  "Either set the property normally at KEY with VALUE.
Deal with org entities and checkbox as well."
  (funcall (if (orgtrello-entity/org-checkbox-p!) 'orgtrello-cbx/org-set-property 'org-set-property) key value))

(defun orgtrello-buffer/org-entry-get (point key)
  "Extract the identifier from the POINT at KEY.
Deal with org entities and checkbox as well."
  (funcall (if (orgtrello-entity/org-checkbox-p!) 'orgtrello-cbx/org-get-property 'org-entry-get) point key))

(defun orgtrello-buffer/--user-ids-assigned-to-current-card ()
  "Compute the user ids assigned to the current card."
  (--> (orgtrello-buffer/get-usernames-assigned-property!)
    (orgtrello-data/--users-from it)
    (--map (gethash (format "%s%s" *ORGTRELLO/USER-PREFIX* it) *ORGTRELLO/HMAP-USERS-NAME-ID*) it)
    (orgtrello-data/--users-to it)))

(defun orgtrello-buffer/metadata! ()
  "Compute the metadata for a given org entry. Also add some metadata identifier/due-data/point/buffer-name/etc..."
  (let ((current-point (point)))
    (->> (orgtrello-buffer/--extract-metadata!)
      (cons (-> current-point (orgtrello-buffer/org-entry-get "DEADLINE") orgtrello-buffer/--convert-orgmode-date-to-trello-date))
      (cons (orgtrello-buffer/extract-identifier! current-point))
      (cons current-point)
      (cons (buffer-name))
      (cons (orgtrello-buffer/--user-ids-assigned-to-current-card))
      (cons (when (orgtrello-entity/card-at-pt!) (orgtrello-buffer/extract-description-from-current-position!)))
      (cons (orgtrello-buffer/org-entry-get current-point *ORGTRELLO/CARD-COMMENTS*))
      (cons (orgtrello-buffer/org-unknown-drawer-properties!))
      orgtrello-buffer/--to-orgtrello-metadata)))

(defun orgtrello-buffer/--filter-out-known-properties (list)
  "Filter out the org-trello known properties from the LIST."
  (--filter (not (or (string-match-p "^orgtrello-.*" (car it))
                     (string= (car it) "CATEGORY"))) list))

(defun orgtrello-buffer/org-unknown-drawer-properties! ()
  "Retrieve the key/value pairs of org-trello unknown drawer properties."
  (->> (org-entry-properties (point) 'standard)
    orgtrello-buffer/--filter-out-known-properties))

(defun orgtrello-buffer/org-up-parent! ()
  "A function to get back to the current entry's parent"
  (funcall (if (orgtrello-entity/org-checkbox-p!) 'orgtrello-cbx/org-up! 'org-up-heading-safe)))

(defun orgtrello-buffer/--parent-metadata! ()
  "Extract the metadata from the current heading's parent."
  (save-excursion
    (orgtrello-buffer/org-up-parent!)
    (orgtrello-buffer/metadata!)))

(defun orgtrello-buffer/--grandparent-metadata! ()
  "Extract the metadata from the current heading's grandparent."
  (save-excursion
    (orgtrello-buffer/org-up-parent!)
    (orgtrello-buffer/org-up-parent!)
    (orgtrello-buffer/metadata!)))

(defun orgtrello-buffer/entry-get-full-metadata! ()
  "Compute metadata needed for entry into a map with keys :current, :parent, :grandparent. Returns nil if the level is superior to 4."
  (save-excursion
    (let* ((current   (orgtrello-buffer/metadata!))
           (level     (orgtrello-data/entity-level current)))
      (when (< level *ORGTRELLO/OUTOFBOUNDS-LEVEL*)
        (let* ((ancestors (cond ((= level *ORGTRELLO/CARD-LEVEL*)      '(nil nil))
                                ((= level *ORGTRELLO/CHECKLIST-LEVEL*) `(,(orgtrello-buffer/--parent-metadata!) nil))
                                ((= level *ORGTRELLO/ITEM-LEVEL*)      `(,(orgtrello-buffer/--parent-metadata!) ,(orgtrello-buffer/--grandparent-metadata!)))))
               (parent      (car ancestors))
               (grandparent (cadr ancestors)))
          (orgtrello-data/put-parent grandparent parent)
          (orgtrello-data/put-parent parent current)
          (orgtrello-data/make-hierarchy current parent grandparent))))))

(defun orgtrello-buffer/--to-orgtrello-metadata (heading-metadata)
  "Given the HEADING-METADATA returned by the function 'org-heading-components.
Make it a hashmap with key :level,  :keyword,  :name and their respective value."
  (cl-destructuring-bind (unknown-properties comments description member-ids buffer-name point id due level _ keyword _ name tags) heading-metadata
    (orgtrello-data/make-hash-org member-ids level (if keyword keyword (car *ORGTRELLO/ORG-KEYWORD-TRELLO-LIST-NAMES*)) name id due point buffer-name description comments tags unknown-properties)))

(defun orgtrello-buffer/filtered-kwds! ()
  "org keywords used (based on org-todo-keywords-1)."
  (let ((keywords org-todo-keywords-1))
    (nreverse (reverse keywords))))

(defun orgtrello-buffer/org-file-properties! ()
  (let ((org-trello-file-properties org-file-properties))
    org-trello-file-properties))

(defun orgtrello-buffer/org-map-entries (fn-to-execute)
  "Execute for each heading the FN-TO-EXECUTE."
  (org-map-entries fn-to-execute))

(defun orgtrello-buffer/end-of-line-point! ()
  "Compute the end of line for an org-trello buffer."
  (let* ((pt (save-excursion (org-end-of-line) (point))))
    (if (orgtrello-entity/org-checkbox-p!)
        (-if-let (s (orgtrello-buffer/compute-overlay-size!))
            (- pt s 1)
          pt)
      pt)))

(defun orgtrello-buffer/end-of-line! ()
  "Move the cursor at the end of the line. For a checkbox, move to the 1- point (because of overlays)."
  (interactive)
  (goto-char (orgtrello-buffer/end-of-line-point!)))

(defun orgtrello-buffer/org-decorator (org-fn)
  "If on org-trello checkbox move to the org end of the line.
In any case, execute ORG-FN."
  (when (orgtrello-entity/org-checkbox-p!)
    (org-end-of-line))
  (funcall org-fn))

(defun orgtrello-buffer/org-return! ()
  "Move the cursor at the real end of the line. Then execute org-return."
  (interactive)
  (orgtrello-buffer/org-decorator 'org-return))

(defun orgtrello-buffer/org-ctrl-c-ret! ()
  "Move the cursor at the end of the line. For a checkbox, move to the 1- point (because of overlays)."
  (interactive)
  (orgtrello-buffer/org-decorator 'org-ctrl-c-ret))

(defun orgtrello-buffer/install-overlay! (start-position)
  "Install org-trello overlay from START-POSITION.
First, it removes the current org-trello overlay on actual line.
Then install the new one."
  ;; remove overlay present on current position
  (orgtrello-buffer/remove-overlays! (point-at-bol) (point-at-eol))
  ;; build an overlay to hide the cbx properties
  (overlay-put (make-overlay start-position (point-at-eol) (current-buffer) t nil)
               'invisible 'org-trello-cbx-property))

(defun orgtrello-buffer/get-overlay-at-pos! ()
  "Retrieve overlay at current position.
Return nil if none."
  (->> (overlays-in (point-at-bol) (point-at-eol))
    (--filter (eq (overlay-get it 'invisible) 'org-trello-cbx-property))
    car))

(defun orgtrello-buffer/compute-overlay-size! ()
  "Compute the overlay size to the current position"
  (-when-let (o (orgtrello-buffer/get-overlay-at-pos!))
    (- (overlay-end o) (overlay-start o))))

(defun orgtrello-buffer/--compute-marker-from-entry (entry)
  "Compute and set the ENTRY marker (either a sha1 or the id of the entry-metadata)."
  (-if-let (current-entry-id (orgtrello-data/entity-id entry))
      current-entry-id
    (orgtrello-buffer/compute-marker (orgtrello-data/entity-buffername entry) (orgtrello-data/entity-name entry) (orgtrello-data/entity-position entry))))

(defun orgtrello-buffer/compute-marker (buffer-name name position)
  "Compute the orgtrello marker which is composed of BUFFER-NAME, NAME and POSITION."
  (->> (list *ORGTRELLO/MARKER* buffer-name name (if (stringp position) position (int-to-string position)))
    (-interpose "-")
    (apply 'concat)
    sha1
    (concat *ORGTRELLO/MARKER* "-")))

(defun orgtrello-buffer/save-buffer (buffer-name)
  "Given a BUFFER-NAME, save it."
  (with-current-buffer buffer-name
    (call-interactively 'save-buffer)))

(orgtrello-log/msg *OT/DEBUG* "orgtrello-buffer loaded!")

(provide 'org-trello-buffer)
;;; org-trello-buffer.el ends here
