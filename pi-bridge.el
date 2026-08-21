;;; pi-bridge.el --- Drive a pi coding-agent session from Emacs -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:
;;
;; A thin bridge to the pi coding agent: one long-lived `pi --mode rpc`
;; process per project, a chat buffer rendering the streamed events, and
;; commands that send prompts carrying current-file/region context.
;;
;; pi edits files on disk; the bridge reverts unmodified project buffers as
;; edits land, so your buffers follow the agent live.  Review/reject via git
;; hunks (diff-hl) as usual.
;;
;; Companion: emacs-context.ts (pi extension) registers an `emacs_context'
;; tool that calls back into Emacs via emacsclient -> `pi-bridge-context',
;; so the agent can see what you are looking at on demand.  Requires
;; (server-start).
;;
;; Setup (Doom, in config.el):
;;
;;   (use-package! pi-bridge
;;     :load-path "/path/to/pi-bridge"
;;     :commands (pi-bridge pi-bridge-send pi-bridge-send-region)
;;     :init
;;     (map! :leader
;;           (:prefix ("j" . "pi")
;;            :desc "pi chat buffer"    "j" #'pi-bridge
;;            :desc "compose prompt"    "c" #'pi-bridge-compose
;;            :desc "send to pi"        "s" #'pi-bridge-send
;;            :desc "send region to pi" "r" #'pi-bridge-send-region
;;            :desc "stage region/file" "m" #'pi-bridge-stage-dwim
;;            :desc "clear staged"      "M" #'pi-bridge-stage-clear
;;            :desc "abort pi"          "a" #'pi-bridge-abort
;;            :desc "kill pi session"   "k" #'pi-bridge-kill)))
;;   (after! emacs (unless (bound-and-true-p server-process) (server-start)))
;;
;;; Code:

(require 'project)
(require 'subr-x)

(defgroup pi-bridge nil
  "Drive a pi coding-agent session from Emacs."
  :group 'tools)

(defcustom pi-bridge-command '("pi" "--mode" "rpc")
  "Base command to start pi in RPC mode."
  :type '(repeat string))

(defcustom pi-bridge-extra-args nil
  "Extra CLI args appended to `pi-bridge-command' (e.g. (\"--model\" \"...\"))."
  :type '(repeat string))

(defcustom pi-bridge-session-id "pi-bridge"
  "Stable per-project pi session id, so conversations survive Emacs restarts.
pi scopes session ids to the project directory, so every project gets its
own \"pi-bridge\" session.  Set to nil to start a fresh session each time."
  :type '(choice string (const nil)))

(defcustom pi-bridge-history-limit 30
  "How many prior messages to replay in the chat buffer when resuming."
  :type 'integer)

(defcustom pi-bridge-show-thinking nil
  "When non-nil, stream the model's thinking into the chat buffer (dimmed)."
  :type 'boolean)

(defcustom pi-bridge-context-lines 40
  "Lines of surrounding code `pi-bridge-context' shows when no region is active."
  :type 'integer)

(defface pi-bridge-prompt-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for echoed user prompts.")

(defface pi-bridge-tool-face
  '((t :inherit font-lock-keyword-face))
  "Face for tool-call lines.")

(defface pi-bridge-dim-face
  '((t :inherit shadow))
  "Face for secondary info (thinking, context notes, separators).")

(defface pi-bridge-error-face
  '((t :inherit error))
  "Face for errors.")

(defvar pi-bridge--procs (make-hash-table :test 'equal)
  "Project root -> pi RPC process.")

(defvar-local pi-bridge--root nil)
(defvar-local pi-bridge--proc nil)
(defvar-local pi-bridge--busy nil)
(defvar-local pi-bridge--model nil)
(defvar-local pi-bridge--session nil)

;;; Project / process plumbing

(defun pi-bridge--root ()
  "Project root for the current buffer (falls back to `default-directory')."
  (or pi-bridge--root
      (when-let ((pr (project-current nil)))
        (expand-file-name (project-root pr)))
      (expand-file-name default-directory)))

(defun pi-bridge--buffer-name (root)
  (format "*pi: %s*" (file-name-nondirectory (directory-file-name root))))

(defun pi-bridge--attach-buffer (proc root)
  "Create and wire a chat buffer for PROC; used at start and reattach."
  (let ((buf (get-buffer-create (pi-bridge--buffer-name root))))
    (process-put proc 'pi-chat buf)
    (with-current-buffer buf
      (unless (derived-mode-p 'pi-bridge-mode) (pi-bridge-mode))
      (setq pi-bridge--root root
            pi-bridge--proc proc
            pi-bridge--busy nil))
    buf))

(defun pi-bridge--live-proc (root)
  (let ((proc (gethash root pi-bridge--procs)))
    (when (and proc (process-live-p proc))
      ;; chat buffer killed out from under the live process: reattach a
      ;; fresh buffer and replay state/history from the running session
      (unless (buffer-live-p (process-get proc 'pi-chat))
        (pi-bridge--attach-buffer proc root)
        (process-put proc 'pi-history-done nil)
        (pi-bridge--send-json proc '(:type "get_state")))
      proc)))

(defun pi-bridge--proc (root &optional create)
  (or (pi-bridge--live-proc root)
      (and create (pi-bridge--start root))))

(defun pi-bridge--start (root)
  "Start `pi --mode rpc' in ROOT; return the process."
  (let* ((default-directory root)
         (buf (get-buffer-create (pi-bridge--buffer-name root)))
         (stderr-buf (get-buffer-create (format " *pi-stderr: %s*" root)))
         (proc (make-process
                :name (format "pi-bridge[%s]" root)
                :command (append pi-bridge-command
                                 (when pi-bridge-session-id
                                   (list "--session-id" pi-bridge-session-id))
                                 pi-bridge-extra-args)
                :connection-type 'pipe
                :noquery t
                :filter #'pi-bridge--filter
                :sentinel #'pi-bridge--sentinel
                :stderr stderr-buf)))
    (process-put proc 'pi-chat buf)
    (process-put proc 'pi-root root)
    (process-put proc 'pi-acc "")
    (pi-bridge--attach-buffer proc root)
    (puthash root proc pi-bridge--procs)
    (pi-bridge--insert buf (format "── pi started in %s ──\n" root)
                       'pi-bridge-dim-face)
    (pi-bridge--send-json proc '(:type "get_state"))
    proc))

(defun pi-bridge--sentinel (proc event)
  (let ((buf (process-get proc 'pi-chat))
        (root (process-get proc 'pi-root)))
    ;; only drop the registry entry if it still points at THIS process — a
    ;; replacement may already have been registered under the same root
    (when (and root (eq (gethash root pi-bridge--procs) proc))
      (remhash root pi-bridge--procs))
    (when (buffer-live-p buf)
      (with-current-buffer buf (setq pi-bridge--busy nil))
      (pi-bridge--insert buf (format "\n── pi process %s──\n" event)
                         'pi-bridge-dim-face))))

(defun pi-bridge--send-json (proc obj)
  (process-send-string proc (concat (json-serialize obj) "\n")))

;;; Rendering

(defun pi-bridge--insert (buf text &optional face)
  "Append TEXT to chat BUF; keep window scrolled if it was at the bottom."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let* ((win (get-buffer-window buf t))
             (follow (or (not win) (>= (window-point win) (1- (point-max)))))
             (inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (insert (if face (propertize text 'face face) text)))
        (when (and win follow)
          (set-window-point win (point-max)))))))

(defun pi-bridge--ensure-bol (buf)
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (unless (or (= (point-max) (point-min))
                  (eq (char-before (point-max)) ?\n))
        (pi-bridge--insert buf "\n")))))

(defun pi-bridge--first-line (s)
  (car (split-string (or s "") "\n")))

(defun pi-bridge--truncate (s n)
  (if (> (length s) n) (concat (substring s 0 n) "…") s))

;;; Event handling

(defun pi-bridge--filter (proc chunk)
  (let* ((acc (concat (process-get proc 'pi-acc) chunk))
         (parts (split-string acc "\n")))
    (process-put proc 'pi-acc (car (last parts)))
    (dolist (line (butlast parts))
      (let ((line (string-trim-right line "\r")))
        (unless (string-empty-p line)
          (condition-case err
              (pi-bridge--handle
               proc
               (json-parse-string line :object-type 'plist
                                  :null-object nil :false-object nil))
            (json-parse-error
             (pi-bridge--insert (process-get proc 'pi-chat)
                                (concat line "\n") 'pi-bridge-dim-face))
            (error
             (pi-bridge--insert (process-get proc 'pi-chat)
                                (format "[pi-bridge] %s\n" (error-message-string err))
                                'pi-bridge-error-face))))))))

(defun pi-bridge--handle (proc ev)
  (let ((buf (process-get proc 'pi-chat))
        (root (process-get proc 'pi-root))
        (type (plist-get ev :type)))
    (pcase type
      ("response" (pi-bridge--handle-response proc buf ev))
      ("agent_start"
       (with-current-buffer buf (setq pi-bridge--busy t))
       (force-mode-line-update t))
      ("agent_settled"
       (with-current-buffer buf (setq pi-bridge--busy nil))
       (force-mode-line-update t)
       (pi-bridge--ensure-bol buf)
       (pi-bridge--insert buf "──\n" 'pi-bridge-dim-face)
       (pi-bridge--revert-project-buffers root))
      ("message_update"
       (pi-bridge--handle-delta buf (plist-get ev :assistantMessageEvent)))
      ("tool_execution_start"
       (pi-bridge--ensure-bol buf)
       (pi-bridge--insert buf (format "⚙ %s " (plist-get ev :toolName))
                          'pi-bridge-tool-face)
       (pi-bridge--insert buf
                          (concat (pi-bridge--truncate
                                   (pi-bridge--tool-summary
                                    (plist-get ev :toolName) (plist-get ev :args))
                                   120)
                                  "\n")
                          'pi-bridge-dim-face))
      ("tool_execution_end"
       (pi-bridge--handle-tool-end buf root ev))
      ("auto_retry_start"
       (pi-bridge--ensure-bol buf)
       (pi-bridge--insert buf
                          (format "retrying (%s/%s): %s\n"
                                  (plist-get ev :attempt) (plist-get ev :maxAttempts)
                                  (pi-bridge--first-line (plist-get ev :errorMessage)))
                          'pi-bridge-dim-face))
      ("compaction_start"
       (pi-bridge--ensure-bol buf)
       (pi-bridge--insert buf "compacting context…\n" 'pi-bridge-dim-face))
      ("extension_error"
       (pi-bridge--ensure-bol buf)
       (pi-bridge--insert buf
                          (format "extension error (%s): %s\n"
                                  (plist-get ev :extensionPath) (plist-get ev :error))
                          'pi-bridge-error-face))
      ("extension_ui_request" (pi-bridge--handle-ui proc buf ev))
      (_ nil))))

(defun pi-bridge--handle-response (proc buf ev)
  (let ((cmd (plist-get ev :command)))
    (cond
     ((not (plist-get ev :success))
      (pi-bridge--ensure-bol buf)
      (pi-bridge--insert buf (format "✗ %s: %s\n" cmd (plist-get ev :error))
                         'pi-bridge-error-face))
     ((equal cmd "get_state")
      (let* ((data (plist-get ev :data))
             (n (or (plist-get data :messageCount) 0)))
        (with-current-buffer buf
          (setq pi-bridge--model (plist-get (plist-get data :model) :id)
                pi-bridge--session (plist-get data :sessionId)))
        (force-mode-line-update t)
        ;; resumed a session with prior messages: replay them once
        (when (and (> n 0) (not (process-get proc 'pi-history-done)))
          (process-put proc 'pi-history-done t)
          (pi-bridge--send-json proc '(:type "get_messages")))))
     ((equal cmd "get_messages")
      (pi-bridge--render-history
       buf (plist-get (plist-get ev :data) :messages))))))

(defun pi-bridge--content-text (content)
  "Concatenated text blocks of a message CONTENT (string or block vector)."
  (cond
   ((stringp content) content)
   ((vectorp content)
    (mapconcat (lambda (b)
                 (when (equal (plist-get b :type) "text") (plist-get b :text)))
               content ""))
   (t "")))

(defun pi-bridge--display-prompt (s)
  "First line of user prompt S, with any [context] preamble stripped."
  (let ((s (or s "")))
    (when (string-prefix-p "[context]" s)
      (when-let ((i (string-search "\n\n" s)))
        (setq s (substring s (+ i 2)))))
    (pi-bridge--truncate (pi-bridge--first-line s) 160)))

(defun pi-bridge--render-history (buf messages)
  "Replay the tail of a resumed session's MESSAGES into chat BUF."
  (let* ((msgs (seq-filter
                (lambda (m) (member (plist-get m :role) '("user" "assistant")))
                (append messages nil)))
         (dropped (max 0 (- (length msgs) pi-bridge-history-limit)))
         (tail (seq-drop msgs dropped)))
    (when tail
      (when (> dropped 0)
        (pi-bridge--insert buf (format "… %d earlier messages not shown\n" dropped)
                           'pi-bridge-dim-face))
      (dolist (m tail)
        (pcase (plist-get m :role)
          ("user"
           (pi-bridge--insert
            buf (format "\n❯ %s\n" (pi-bridge--display-prompt
                                    (pi-bridge--content-text (plist-get m :content))))
            'pi-bridge-prompt-face))
          ("assistant"
           (let ((text (string-trim (pi-bridge--content-text (plist-get m :content)))))
             (unless (string-empty-p text)
               (pi-bridge--insert buf (concat (pi-bridge--truncate text 4000) "\n")))))))
      (pi-bridge--insert buf "── resumed ──\n" 'pi-bridge-dim-face))))

(defun pi-bridge--handle-delta (buf d)
  (pcase (plist-get d :type)
    ("text_start" (pi-bridge--ensure-bol buf))
    ("text_delta" (pi-bridge--insert buf (plist-get d :delta)))
    ("thinking_start"
     (when pi-bridge-show-thinking (pi-bridge--ensure-bol buf)))
    ("thinking_delta"
     (when pi-bridge-show-thinking
       (pi-bridge--insert buf (plist-get d :delta) 'pi-bridge-dim-face)))
    ("thinking_end"
     (when pi-bridge-show-thinking (pi-bridge--ensure-bol buf)))
    (_ nil)))

(defun pi-bridge--tool-summary (name args)
  (or (pcase name
        ("bash" (pi-bridge--first-line (plist-get args :command)))
        ((or "edit" "write" "read" "multi-edit") (plist-get args :path))
        (_ (and args (json-serialize args))))
      ""))

(defun pi-bridge--handle-tool-end (buf root ev)
  (let ((name (plist-get ev :toolName))
        (args (plist-get ev :args)))
    (if (plist-get ev :isError)
        (let* ((content (plist-get (plist-get ev :result) :content))
               (text (and (vectorp content) (> (length content) 0)
                          (plist-get (aref content 0) :text))))
          (pi-bridge--ensure-bol buf)
          (pi-bridge--insert buf
                             (format "✗ %s: %s\n" name
                                     (pi-bridge--truncate (pi-bridge--first-line text) 160))
                             'pi-bridge-error-face))
      ;; Successful edit/write: refresh the visiting buffer right away so the
      ;; change appears in-editor as it lands, not only at end of turn.
      (when (member name '("edit" "write" "multi-edit"))
        (when-let ((path (plist-get args :path)))
          (pi-bridge--revert-file
           (expand-file-name path root)))))))

;;; Extension UI sub-protocol (confirm/select/input dialogs from pi extensions)

(defun pi-bridge--handle-ui (proc buf ev)
  (let ((id (plist-get ev :id))
        (method (plist-get ev :method))
        (title (or (plist-get ev :title) "pi")))
    (pcase method
      ("notify"
       (message "pi: %s" (plist-get ev :message)))
      ("setStatus" nil)
      ("setWidget" nil)
      ("setTitle" nil)
      ("set_editor_text" nil)
      ("confirm"
       (pi-bridge--send-json
        proc
        (condition-case nil
            `(:type "extension_ui_response" :id ,id
              :confirmed ,(if (y-or-n-p (format "%s %s "
                                                title (or (plist-get ev :message) "")))
                              t :false))
          (quit `(:type "extension_ui_response" :id ,id :cancelled t)))))
      ((or "select" "input" "editor")
       (pi-bridge--send-json
        proc
        (condition-case nil
            (let ((value (pcase method
                           ("select" (completing-read
                                      (concat title ": ")
                                      (append (plist-get ev :options) nil)
                                      nil t))
                           (_ (read-string (concat title ": ")
                                           (plist-get ev :prefill))))))
              `(:type "extension_ui_response" :id ,id :value ,value))
          (quit `(:type "extension_ui_response" :id ,id :cancelled t)))))
      (_ (pi-bridge--insert buf (format "[pi ui] %s\n" method) 'pi-bridge-dim-face)))))

;;; Keeping buffers in sync with the agent's disk edits

(defun pi-bridge--revert-file (path)
  (when-let ((b (find-buffer-visiting path)))
    (with-current-buffer b
      (when (and (not (buffer-modified-p))
                 (file-exists-p path)
                 (not (verify-visited-file-modtime (current-buffer))))
        (revert-buffer :ignore-auto :noconfirm :preserve-modes)))))

(defun pi-bridge--revert-project-buffers (root)
  "Revert unmodified buffers under ROOT whose files changed on disk."
  (dolist (b (buffer-list))
    (when-let ((f (buffer-file-name b)))
      (when (file-in-directory-p f root)
        (pi-bridge--revert-file f)))))

(defun pi-bridge--save-project-buffers (root)
  "Silently save modified file buffers under ROOT (agent edits disk state)."
  (save-some-buffers t (lambda ()
                         (and buffer-file-name
                              (file-in-directory-p buffer-file-name root)))))

;;; Context capture (also called remotely by pi's emacs_context tool)

(defun pi-bridge-context ()
  "Describe what the user is looking at, for pi's `emacs_context' tool.
Returns a plain-text summary: current file, cursor, region or surrounding
lines, and other open file buffers."
  (let ((buf (seq-find (lambda (b)
                         (and (buffer-file-name b)
                              (not (string-prefix-p " " (buffer-name b)))))
                       (buffer-list))))
    (if (not buf)
        "No file buffer is open in Emacs."
      (with-current-buffer buf
        (let* ((line (line-number-at-pos (point)))
               (total (line-number-at-pos (point-max)))
               (region
                (when (region-active-p)
                  (format "Selected region (lines %d-%d):\n```\n%s\n```\n"
                          (line-number-at-pos (region-beginning))
                          (line-number-at-pos (region-end))
                          (pi-bridge--truncate
                           (buffer-substring-no-properties
                            (region-beginning) (region-end))
                           4000))))
               (around
                (unless region
                  (let* ((half (/ pi-bridge-context-lines 2))
                         (beg (save-excursion
                                (forward-line (- half)) (line-beginning-position)))
                         (end (save-excursion
                                (forward-line half) (line-end-position))))
                    (format "Code around the cursor (lines %d-%d):\n```\n%s\n```\n"
                            (line-number-at-pos beg) (line-number-at-pos end)
                            (buffer-substring-no-properties beg end)))))
               (others
                (delq nil
                      (mapcar (lambda (b)
                                (let ((f (buffer-file-name b)))
                                  (and f (not (eq b buf)) f)))
                              (buffer-list)))))
          (concat
           (format "Current file: %s (cursor at line %d of %d)%s\n"
                   buffer-file-name line total
                   (if (buffer-modified-p) " [HAS UNSAVED CHANGES]" ""))
           (or region around)
           (when others
             (concat "Other open files (most recent first):\n"
                     (mapconcat (lambda (f) (concat "  " f))
                                (seq-take others 15) "\n")
                     "\n"))))))))

(defun pi-bridge--context-block ()
  "Context header prepended to prompts sent from a file buffer.
Returns (MESSAGE-PART . ECHO-NOTE), or nil when not visiting a file."
  (when buffer-file-name
    (let* ((root (pi-bridge--root))
           (rel (file-relative-name buffer-file-name root))
           (line (line-number-at-pos (point))))
      (if (use-region-p)
          (let ((l1 (line-number-at-pos (region-beginning)))
                (l2 (line-number-at-pos (region-end))))
            (cons (format "[context] I am in %s, looking at lines %d-%d:\n```\n%s\n```\n\n"
                          rel l1 l2
                          (buffer-substring-no-properties
                           (region-beginning) (region-end)))
                  (format "[%s:%d-%d]" rel l1 l2)))
        (cons (format "[context] I am in %s, cursor on line %d.\n\n" rel line)
              (format "[%s:%d]" rel line))))))

;;; Commands

(defun pi-bridge--dispatch (root message echo-main echo-note)
  "Save ROOT's buffers, echo the prompt, and send MESSAGE to pi."
  (let* ((proc (pi-bridge--proc root t))
         (buf (process-get proc 'pi-chat))
         (busy (with-current-buffer buf pi-bridge--busy)))
    (pi-bridge--save-project-buffers root)
    (pi-bridge--ensure-bol buf)
    (pi-bridge--insert buf (format "\n❯ %s" echo-main) 'pi-bridge-prompt-face)
    (when echo-note
      (pi-bridge--insert buf (format "  %s" echo-note) 'pi-bridge-dim-face))
    (pi-bridge--insert buf "\n\n")
    (pi-bridge--send-json
     proc
     (if busy
         `(:type "prompt" :message ,message :streamingBehavior "steer")
       `(:type "prompt" :message ,message)))
    (unless (get-buffer-window buf t)
      (display-buffer buf))))

;;;###autoload
(defun pi-bridge ()
  "Open (starting if needed) the pi chat buffer for the current project."
  (interactive)
  (let* ((root (pi-bridge--root))
         (proc (pi-bridge--proc root t)))
    (pop-to-buffer (process-get proc 'pi-chat))))

;;;###autoload
(defun pi-bridge-send ()
  "Send a quick one-line prompt to the project's pi session.
From a file buffer, the prompt carries file/cursor context (and the region,
when active).  From the chat buffer, it is sent bare.  An empty prompt
opens the multi-line compose buffer instead (see `pi-bridge-compose')."
  (interactive)
  (let* ((ctx (unless (derived-mode-p 'pi-bridge-mode) (pi-bridge--context-block)))
         (root (pi-bridge--root))
         (instruction (read-string "pi ❯ (empty = compose) ")))
    (if (string-empty-p (string-trim instruction))
        (pi-bridge-compose)
      (pi-bridge--dispatch root
                           (concat (car ctx) instruction)
                           instruction (cdr ctx)))))

;;;###autoload
(defun pi-bridge-send-region (beg end)
  "Send the active region to the project's pi session.
Prompts for an instruction; when one is given, the region is sent as fenced
context beneath it.  When left empty, the region text itself IS the prompt
\(so you can write a prompt in the buffer — e.g. as a comment — select it,
and send it directly)."
  (interactive "r")
  (unless (use-region-p) (user-error "No active region"))
  (let* ((root (pi-bridge--root))
         (rel (if buffer-file-name
                  (file-relative-name buffer-file-name root)
                (buffer-name)))
         (l1 (line-number-at-pos beg))
         (l2 (line-number-at-pos end))
         (text (buffer-substring-no-properties beg end))
         (instruction (read-string
                       (format "pi ❯ [%s:%d-%d] (empty = region is the prompt) "
                               rel l1 l2))))
    (if (string-empty-p (string-trim instruction))
        (pi-bridge--dispatch root text
                             (pi-bridge--truncate (pi-bridge--first-line text) 120)
                             (format "[%s:%d-%d]" rel l1 l2))
      (pi-bridge--dispatch
       root
       (format "[context] I am in %s, looking at lines %d-%d:\n```\n%s\n```\n\n%s"
               rel l1 l2 text instruction)
       instruction (format "[%s:%d-%d]" rel l1 l2)))))

;;; Compose buffer (multi-line prompts)

(defvar-local pi-bridge--compose-root nil)
(defvar-local pi-bridge--compose-context nil)
(defvar-local pi-bridge--compose-use-context t)

(defun pi-bridge--compose-header ()
  (concat "C-c C-c send · C-c C-k cancel"
          (when pi-bridge--compose-context
            (format " · C-c C-t context [%s]"
                    (if pi-bridge--compose-use-context
                        (cdr pi-bridge--compose-context)
                      "off")))))

(define-derived-mode pi-bridge-compose-mode text-mode "pi-compose"
  "Compose a multi-line prompt for pi."
  (setq header-line-format '(:eval (pi-bridge--compose-header)))
  (visual-line-mode 1))

(define-key pi-bridge-compose-mode-map (kbd "C-c C-c") #'pi-bridge-compose-send)
(define-key pi-bridge-compose-mode-map (kbd "C-c C-k") #'pi-bridge-compose-cancel)
(define-key pi-bridge-compose-mode-map (kbd "C-c C-t") #'pi-bridge-compose-toggle-context)

;;;###autoload
(defun pi-bridge-compose ()
  "Open a multi-line compose buffer for the project's pi session.
Invoked from a file buffer it captures file/cursor/region context, shown in
the header line (C-c C-t toggles it off).  C-c C-c sends, C-c C-k cancels."
  (interactive)
  (let ((root (pi-bridge--root))
        (ctx (unless (derived-mode-p 'pi-bridge-mode) (pi-bridge--context-block)))
        (buf (get-buffer-create "*pi compose*")))
    (with-current-buffer buf
      (pi-bridge-compose-mode)
      (setq pi-bridge--compose-root root
            pi-bridge--compose-context ctx
            pi-bridge--compose-use-context t))
    (pop-to-buffer buf '((display-buffer-below-selected)
                         (window-height . 8)))
    (when (fboundp 'evil-insert-state) (evil-insert-state))))

(defun pi-bridge-compose-toggle-context ()
  "Toggle whether the captured file/region context is sent with the prompt."
  (interactive)
  (setq pi-bridge--compose-use-context (not pi-bridge--compose-use-context))
  (force-mode-line-update))

(defun pi-bridge-compose-send ()
  "Send the compose buffer's content and close it."
  (interactive)
  (let ((text (string-trim (buffer-substring-no-properties (point-min) (point-max))))
        (root pi-bridge--compose-root)
        (ctx (and pi-bridge--compose-use-context pi-bridge--compose-context)))
    (when (string-empty-p text) (user-error "Empty prompt"))
    (erase-buffer)
    (quit-window)
    (pi-bridge--dispatch root
                         (concat (car ctx) text)
                         (pi-bridge--truncate (pi-bridge--first-line text) 120)
                         (cdr ctx))))

(defun pi-bridge-compose-cancel ()
  "Close the compose buffer without sending (content is kept)."
  (interactive)
  (quit-window))

;;; Context staging (for chatting with pi in a terminal TUI)

(defun pi-bridge--stage-file (root)
  (expand-file-name ".pi/emacs-stage.md" root))

;;;###autoload
(defun pi-bridge-stage-dwim ()
  "Stage the region (or, with none active, the current file) as context.
Staged items accumulate in .pi/emacs-stage.md and are attached to the next
message you send to pi — in a terminal `pi' TUI (via the emacs-stage
extension) or through this bridge.  Stage several, then go type your
message."
  (interactive)
  (let* ((root (pi-bridge--root))
         (stage (pi-bridge--stage-file root))
         (rel (and buffer-file-name (file-relative-name buffer-file-name root)))
         (region (use-region-p))
         (entry (cond
                 (region
                  (format "From %s lines %d-%d:\n```\n%s\n```\n\n"
                          (or rel (buffer-name))
                          (line-number-at-pos (region-beginning))
                          (line-number-at-pos (region-end))
                          (buffer-substring-no-properties
                           (region-beginning) (region-end))))
                 (rel (format "Read %s for context.\n\n" rel))
                 (t (user-error "Nothing to stage (no file, no region)")))))
    (make-directory (file-name-directory stage) t)
    (unless (file-exists-p stage)
      (write-region "[staged context from emacs]\n\n" nil stage nil 'silent))
    (write-region entry nil stage 'append 'silent)
    (deactivate-mark)
    (message "pi: staged %s"
             (if region
                 (format "%s:%d-%d" (or rel (buffer-name))
                         (line-number-at-pos (region-beginning))
                         (line-number-at-pos (region-end)))
               rel))))

;;;###autoload
(defun pi-bridge-stage-clear ()
  "Discard all staged context for this project."
  (interactive)
  (let ((stage (pi-bridge--stage-file (pi-bridge--root))))
    (if (file-exists-p stage)
        (progn (delete-file stage) (message "pi: staged context cleared"))
      (message "pi: nothing staged"))))

;;;###autoload
(defun pi-bridge-stage-show ()
  "Visit the staged-context file for this project."
  (interactive)
  (let ((stage (pi-bridge--stage-file (pi-bridge--root))))
    (unless (file-exists-p stage) (user-error "Nothing staged"))
    (find-file-other-window stage)))

;;;###autoload
(defun pi-bridge-clear ()
  "Clear the chat buffer.  The pi session itself is untouched."
  (interactive)
  (unless (derived-mode-p 'pi-bridge-mode)
    (user-error "Not in a pi chat buffer"))
  (let ((inhibit-read-only t))
    (erase-buffer)))

;;;###autoload
(defun pi-bridge-abort ()
  "Abort the current pi run for this project."
  (interactive)
  (if-let ((proc (pi-bridge--live-proc (pi-bridge--root))))
      (pi-bridge--send-json proc '(:type "abort"))
    (user-error "No pi session for this project")))

;;;###autoload
(defun pi-bridge-kill ()
  "Kill the pi process for this project (session file remains on disk)."
  (interactive)
  (if-let ((proc (pi-bridge--live-proc (pi-bridge--root))))
      (delete-process proc)
    (user-error "No pi session for this project")))

;;; Chat buffer major mode

(defun pi-bridge--header ()
  (concat (if pi-bridge--busy "● " "○ ")
          (or pi-bridge--model "pi")
          (when pi-bridge--session (format "  ·  session %s" pi-bridge--session))))

(define-derived-mode pi-bridge-mode special-mode "pi"
  "Chat buffer for a pi coding-agent session."
  (setq header-line-format '(:eval (pi-bridge--header)))
  (visual-line-mode 1))

(define-key pi-bridge-mode-map (kbd "C-c C-s") #'pi-bridge-send)
(define-key pi-bridge-mode-map (kbd "C-c C-k") #'pi-bridge-abort)
(define-key pi-bridge-mode-map (kbd "s") #'pi-bridge-send)
(define-key pi-bridge-mode-map (kbd "c") #'pi-bridge-compose)
(define-key pi-bridge-mode-map (kbd "RET") #'pi-bridge-compose)
(define-key pi-bridge-mode-map (kbd "C") #'pi-bridge-clear)
(define-key pi-bridge-mode-map (kbd "a") #'pi-bridge-abort)

(provide 'pi-bridge)
;;; pi-bridge.el ends here
