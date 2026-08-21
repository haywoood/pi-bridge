/**
 * emacs-stage extension for pi — context staged from Emacs, attached to
 * your next message.
 *
 * Emacs (pi-bridge.el `pi-bridge-stage-dwim') appends regions/file refs to
 * <project>/.pi/emacs-stage.md. This extension shows a widget above the
 * editor while anything is staged, and on your next input prepends the
 * staged content to the message and clears the file. `/stage-clear`
 * discards without sending.
 *
 * Applies wherever pi has a UI (TUI and RPC bridge); one-shot `pi -p`
 * invocations leave staged context untouched.
 *
 * Install globally, next to emacs-context.ts:
 *   ln -s /path/to/pi-bridge/emacs-stage.ts ~/.pi/agent/extensions/
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import * as fs from "node:fs";
import * as path from "node:path";

function stagePath(cwd: string): string {
  return path.join(cwd, ".pi", "emacs-stage.md");
}

function readStage(cwd: string): string | null {
  try {
    const s = fs.readFileSync(stagePath(cwd), "utf-8");
    return s.trim() ? s : null;
  } catch {
    return null;
  }
}

function clearStage(cwd: string): void {
  try {
    fs.unlinkSync(stagePath(cwd));
  } catch {
    /* already gone */
  }
}

function countItems(text: string): number {
  const n = text
    .split("\n")
    .filter((l) => /^(From .+ lines \d+|Read .+ for context)/.test(l)).length;
  return Math.max(n, 1);
}

export default function (pi: ExtensionAPI) {
  let lastShown: string | null = null;

  const updateWidget = (ctx: any) => {
    const text = readStage(ctx.cwd);
    if (text === lastShown) return;
    lastShown = text;
    if (text) {
      const n = countItems(text);
      ctx.ui.setWidget("emacs-stage", [
        `❯ emacs: ${n} staged item${n === 1 ? "" : "s"} — attached to your next message (/stage-clear to drop)`,
      ]);
    } else {
      ctx.ui.setWidget("emacs-stage", undefined);
    }
  };

  pi.on("session_start", async (_event, ctx) => {
    updateWidget(ctx);
    const timer = setInterval(() => updateWidget(ctx), 2000);
    (timer as any).unref?.();
  });

  pi.on("input", async (event, ctx) => {
    if (!ctx.hasUI) return { action: "continue" };
    if (event.source === "extension") return { action: "continue" };
    const staged = readStage(ctx.cwd);
    if (!staged) return { action: "continue" };
    clearStage(ctx.cwd);
    lastShown = null;
    ctx.ui.setWidget("emacs-stage", undefined);
    return {
      action: "transform",
      text: `${staged.trimEnd()}\n\n${event.text}`,
    };
  });

  pi.registerCommand("stage-clear", {
    description: "Discard context staged from Emacs",
    handler: async (_args, ctx) => {
      clearStage(ctx.cwd);
      lastShown = null;
      ctx.ui.setWidget("emacs-stage", undefined);
      ctx.ui.notify("staged context cleared", "info");
    },
  });
}
