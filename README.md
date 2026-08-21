# pi-bridge

![pi-bridge](img.png)

Use the [pi coding agent](https://github.com/earendil-works/pi) from inside
Emacs. One `pi --mode rpc` process per project, a chat buffer showing the
session, and prompts that carry your current file, cursor, and region. pi
edits files on disk as usual; the bridge keeps your buffers in sync as the
edits land.

There's also a small pi extension, `emacs-context.ts`, which gives the agent
an `emacs_context` tool: it can ask Emacs what you're currently looking at
(file, cursor line, active region or surrounding code, unsaved changes,
other open files).

## Requirements

- Emacs 29.1+ with the Emacs server running (`server-start`)
- pi on your PATH

## Setup

Clone somewhere:

```sh
git clone https://github.com/haywoood/pi-bridge ~/code/pi-bridge
```

Load it from your Emacs config. Doom:

```elisp
(use-package! pi-bridge
  :load-path "~/code/pi-bridge"
  :commands (pi-bridge pi-bridge-send pi-bridge-send-region
             pi-bridge-abort pi-bridge-kill)
  :init
  (map! :leader
        (:prefix ("j" . "pi")
         :desc "pi chat buffer"    "j" #'pi-bridge
         :desc "send to pi"        "s" #'pi-bridge-send
         :desc "send region to pi" "r" #'pi-bridge-send-region
         :desc "abort pi"          "a" #'pi-bridge-abort
         :desc "kill pi session"   "k" #'pi-bridge-kill)))

;; the emacs_context tool needs the server + pi-bridge loaded
(add-hook 'doom-first-input-hook
          (defun +pi-bridge-init-h ()
            (require 'pi-bridge)
            (require 'server)
            (unless (server-running-p)
              (server-start))))
```

Vanilla Emacs works the same way with `add-to-list 'load-path`, `require`,
and your own keybindings.

Install the pi extension globally so every project's pi gets the
`emacs_context` tool:

```sh
mkdir -p ~/.pi/agent/extensions
ln -s ~/code/pi-bridge/emacs-context.ts ~/.pi/agent/extensions/
```

## Use

Open a file in a project and run `pi-bridge-send`. The prompt is sent to a
pi process started in the project root (so skills, extensions, and context
files load normally), along with which file and line you're on — and the
region, if one is active. `pi-bridge` opens the chat buffer; `pi-bridge-send-region`
sends the selected code fenced in the prompt.

Notes:

- Modified project buffers are saved before each send, and buffers are
  reverted as the agent's edits land, so buffers and disk stay in sync.
- Edits are ordinary working-tree changes: review them with your usual git
  tooling (diff-hl hunks work well). Prefer reverting hunks over `undo`,
  which would desync the buffer from disk.
- Sending while pi is mid-turn queues the message as steering.
- Killing the bridge keeps the session file; pi can resume it later.

## Options

- `pi-bridge-command`, `pi-bridge-extra-args` — how pi is launched
  (e.g. `("--model" "...")`)
- `pi-bridge-show-thinking` — stream thinking into the chat buffer (default off)
- `pi-bridge-context-lines` — how much code around the cursor
  `emacs_context` reports (default 40)

## License

MIT
