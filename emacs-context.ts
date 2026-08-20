/**
 * emacs_context tool for pi — lets the agent see what the user is looking
 * at in Emacs right now.
 *
 * Calls back into a running Emacs (server-start required) via
 * `emacsclient -e (pi-bridge-context)`; pi-bridge.el (same directory)
 * defines that function: current file, cursor line, active region or
 * surrounding code, unsaved-change status, other open files.
 *
 * Install globally so every project's pi gets it:
 *   ln -s /path/to/pi-bridge/emacs-context.ts ~/.pi/agent/extensions/
 *
 * Degrades gracefully: if Emacs isn't reachable the tool returns an
 * explanatory message instead of failing the turn.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { Type } from "typebox";
import { execFile } from "node:child_process";
import { promisify } from "node:util";

const execFileAsync = promisify(execFile);

/**
 * emacsclient -e prints the evaluated result with elisp `prin1` semantics:
 * a string comes back wrapped in double quotes with `\` and `"`
 * backslash-escaped (newlines are printed literally). Undo that.
 */
function unescapeElispString(raw: string): string | null {
  const s = raw.trim();
  if (s.length < 2 || !s.startsWith('"') || !s.endsWith('"')) return null;
  let out = "";
  for (let i = 1; i < s.length - 1; i++) {
    const c = s[i];
    if (c === "\\" && i + 1 < s.length - 1) {
      const next = s[i + 1];
      if (next === "n") out += "\n";
      else if (next === "t") out += "\t";
      else out += next; // \\ , \" and anything else: take the char verbatim
      i++;
    } else {
      out += c;
    }
  }
  return out;
}

export default function (pi: ExtensionAPI) {
  pi.registerTool({
    name: "emacs_context",
    label: "Emacs context",
    description:
      "See what the user is currently looking at in their Emacs: current file, " +
      "cursor position, selected region (or code surrounding the cursor), " +
      "unsaved-change status, and other open files. Call this whenever the user " +
      "refers to 'this file', 'this function', 'here', or when you need to know " +
      "where they are working before reading or editing anything.",
    parameters: Type.Object({}),
    async execute() {
      try {
        const { stdout } = await execFileAsync(
          "emacsclient",
          ["-e", "(pi-bridge-context)"],
          { timeout: 5000 },
        );
        const text = unescapeElispString(stdout) ?? stdout.trim();
        return { content: [{ type: "text", text }], details: {} };
      } catch (err: any) {
        const detail =
          err?.stderr?.toString()?.trim() || err?.message || "unknown error";
        return {
          content: [
            {
              type: "text",
              text:
                `emacs_context unavailable (${detail}). Emacs must be running ` +
                `with (server-start) and pi-bridge.el loaded. Proceed without it.`,
            },
          ],
          details: {},
        };
      }
    },
  });
}
