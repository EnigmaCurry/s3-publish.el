---
name: s3-publish
description: Publish files to the web through the user's running Emacs (s3-publish.el), and list or delete what has been published. Org and Markdown files become styled HTML pages. Use when the user asks to publish, upload, share or host a file or document on S3/their bucket, to get a link to a file, to see what they have published, or to take something down.
allowed-tools: Bash(${CLAUDE_SKILL_DIR}/s3p:*)
---

Publishes through **s3-publish.el in the user's running Emacs**, using
their S3 profiles and credentials. Everything goes through the wrapper
next to this file, which calls `emacsclient`:

- `${CLAUDE_SKILL_DIR}/s3p` — run `s3p --help` for every command

It prints the Lisp values Emacs returns (plists like
`(:url "https://..." :keys (...) :job 7 :status running)`). Read them as
data.

## How publishing works — what you need to know

- **You publish files, never buffers.** `s3p publish` reads the file as
  it is on disk, in a buffer of its own. It never opens, reads, saves or
  modifies a buffer the user has open, even one visiting the same file,
  and never touches their kill ring. To publish text you wrote, write it
  to a file first.
- **The URL comes from the file's absolute path** (and the profile):
  `md5(profile salt + path)` → `dkhou-ari53-xxlhx-2ix51-7qti3.ext`. Same
  path + same profile = same URL, so **republishing a path replaces it**,
  and a different path is a different URL. So:
  - To update something already published, publish the same path again.
  - Put new content at a new path unless the user wants it replaced, and
    keep files you publish where they will stay (not a path you will
    reuse for something else).
- **Org (`.org`) and Markdown (`.md`, `.markdown`, ...) are documents:**
  they publish **two objects** with one base key — the HTML page
  (`BASE.html`, the URL to share) and the source (`BASE.org` / `BASE.md`,
  linked from the page). Local images they reference are embedded in the
  page (or uploaded beside it if large). Every other file is published
  as-is.
- **Public vs private profiles.** A public profile gives an `https://`
  URL anyone can open, and lists the file in the bucket's RSS feed and
  HTML index. A private one gives an `s3://` URI (or a signed URL if the
  user configured `s3-publish-signed-url-expiry`); use `s3p sign` to make
  a shareable link that expires.

## Choosing a profile — ask when unsure

The agent may use **any** of the user's profiles, and every command
takes one by name; there is no default. Run `s3p profiles` first. Then:

- Use the profile the user named, or one the context makes clear
  (e.g. they said "private", and only one profile is private; or you
  already used one for this file in this conversation).
- **Otherwise ask the user** (AskUserQuestion), listing the profiles with
  their bucket and public/private. You may mention which one is
  `:last-used`, but never pick it silently.

Public publishing puts content on the open web. If the content looks
sensitive (credentials, personal data, anything internal) and the user
did not clearly ask for it to be public, confirm before publishing.

## Commands

```
s3p check                          # Emacs reachable, API loaded?
s3p profiles
s3p publish FILE PROFILE [TITLE]   # TITLE: for the feed/index entry
s3p wait JOB [SECONDS]             # after publish, until done/failed
s3p status [JOB]
s3p list PROFILE [--bucket]
s3p delete TARGET PROFILE          # TARGET: URL, s3:// URI, or file path
s3p sign TARGET DURATION PROFILE   # DURATION: 3600, 30m, 12h, 7d, 2w
```

### Publishing

1. `s3p publish /abs/path/report.org PROFILE` returns `:url`, `:keys`
   and `:job`. Uploads usually run in the background, so `:status` is
   `running` or `queued`.
2. **Always** `s3p wait JOB` before reporting. Exit 0 = done; exit 1 =
   failed (the output has `:error` — report it, do not claim success);
   exit 3 = superseded by a newer upload of the same file (wait for that
   job instead, its number is in `:superseded-by`); exit 4 = timed out
   (report the job is still running).
3. Give the user the `:url`. For a document, that is the HTML page.

### Listing

`s3p list PROFILE` lists from the profile's RSS feed (titles, URLs,
dates) when it keeps one; otherwise — or with `--bucket` — it lists every
object in the bucket (key, URL, size, date), including `index.html`,
`index.xml` and uploaded images. A document shows up in a bucket listing
as two keys sharing a base: `BASE.html` and `BASE.org`/`BASE.md`.

### Deleting

`s3p delete TARGET PROFILE` deletes **every object sharing the target's
key** — for a document both the HTML page and its source, whichever of
the two (or the source file's path) you pass — and removes its feed and
index entry. It returns the keys it deleted (`nil` if there was nothing).
Images uploaded beside a page are kept, as other pages may use them.

Deleting is permanent and breaks links people may have. **Confirm with
the user before deleting**, naming what will go (for a document, both
URLs). After deleting, report the keys returned.

## Problems

- `s3p check` fails with "cannot reach Emacs": the user's Emacs server is
  not running (`M-x server-start`). Tell them; do not start Emacs
  yourself.
- "has no s3-publish API": the running Emacs has an older s3-publish
  loaded. Tell the user to load a version with `s3-publish-api-*`.
- An auth-source/GPG passphrase prompt may appear in the user's Emacs
  the first time credentials are read; `s3p` waits for it. If a command
  seems stuck, tell the user to check their Emacs.
- Full output of background uploads is in Emacs's `*s3-publish-log*`
  buffer.
