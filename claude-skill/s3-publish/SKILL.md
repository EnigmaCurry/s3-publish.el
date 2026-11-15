---
name: s3-publish
description: Publish files to the web with s3-publish.el (S3 / DigitalOcean Spaces), and list or delete what has been published. Org and Markdown files become styled HTML pages. Use when the user asks to publish, upload, share or host a file or document on S3/their bucket, to get a link to a file, to see what they have published, or to take something down.
allowed-tools: Bash(${CLAUDE_SKILL_DIR}/s3p:*)
---

Publishes with **s3-publish.el**, using the user's own S3 profiles and
credentials. Everything goes through the wrapper next to this file:

- `${CLAUDE_SKILL_DIR}/s3p` — run `s3p --help` for every command

Each command runs in **its own fresh `emacs --batch`** with none of the
user's configuration: it reads only their s3-publish settings (profiles
etc.) from their Emacs custom file, and their credentials from
auth-source. Their running Emacs is never involved or touched. Every
command finishes its work before it exits, and prints **JSON** on
stdout. A failure exits non-zero with `{"error": "..."}`.

## How publishing works — what you need to know

- **You publish files.** To publish text you wrote, write it to a file
  first.
- **The URL comes from the file's absolute path** (and the profile):
  `md5(profile salt + path)` → `dkhou-ari53-xxlhx-2ix51-7qti3.ext`. Same
  path + same profile = same URL, so **republishing a path replaces it**,
  and a different path is a different URL. So:
  - To update something already published, publish the same path again
    with the same profile.
  - Put new content at a new path unless the user wants it replaced, and
    keep published files where they will stay.
- **Org (`.org`) and Markdown (`.md`, `.markdown`, ...) are documents:**
  they publish **two objects** with one base key — the HTML page
  (`BASE.html`, the URL to share) and the source (`BASE.org` / `BASE.md`,
  linked from the page). Local images they reference are embedded in the
  page (or uploaded beside it if large). Every other file is published
  as-is.
- **Public vs private profiles.** A public profile gives an `https://`
  URL anyone can open; if it keeps a feed, the file is also listed in
  the bucket's RSS feed and HTML index. A private one gives an `s3://`
  URI (or a signed URL, if the user configured that); use `s3p sign` for
  a shareable link that expires.

## Code in Org documents — off unless the user says so

By default **no code is run**: source blocks are published as written,
without results, and nothing is tangled. Some of the user's Org
documents are literate programs that expect to be tangled and evaluated
when exported (look for `:eval yes`, `:exports results`/`both`, or
`#+S3_PUBLISH_TANGLE: t`). Publishing those without running code gives
a page missing its results.

`--run-code` tangles the document first (when it sets
`#+S3_PUBLISH_TANGLE: t`, overwriting the tangled files) and evaluates
its blocks, without asking. **That runs arbitrary programs on the
user's machine. Only use `--run-code` when the user has explicitly said
to run the document's code** — when a document looks like it needs it,
ask (AskUserQuestion), saying what it will run. It can take as long as
the code does.

## Choosing a profile — ask when unsure

The agent may use **any** of the user's profiles, and every command
takes one by name; there is no default. Run `s3p profiles` first. Then:

- Use the profile the user named, or one the context makes clear —
  e.g. to republish a file, the profile it is already published with
  (`s3p list PROFILE` shows each profile's files).
- **Otherwise ask the user** (AskUserQuestion), listing the profiles with
  their bucket and whether they are public. You may mention which one is
  `last-used`, but never pick it silently.

Public publishing puts content on the open web. If the content looks
sensitive (credentials, personal data, anything internal) and the user
did not clearly ask for it to be public, confirm before publishing.

## Commands

```
s3p check                                      # settings, library, s3cmd found?
s3p profiles
s3p publish FILE PROFILE [--title T] [--run-code]
s3p list PROFILE [--bucket]
s3p delete TARGET PROFILE                      # TARGET: URL, s3:// URI or file path
s3p sign TARGET DURATION PROFILE               # DURATION: 3600, 30m, 12h, 7d, 2w
```

### Publishing

`s3p publish /path/report.org PROFILE` returns
`{"url": ..., "keys": [...]}` once everything is uploaded. Give the user
the `url` — for a document, the HTML page. On failure report the
`error`; do not claim success.

### Listing

`s3p list PROFILE` lists from the profile's RSS feed (title, url, key,
date) when it keeps one; otherwise — or with `--bucket` — every object
in the bucket (key, url, size, date), including `index.html`,
`index.xml` and uploaded images. A document shows up in a bucket listing
as two keys sharing a base: `BASE.html` and `BASE.org`/`BASE.md`.

### Deleting

`s3p delete TARGET PROFILE` deletes **every object sharing the target's
key** — for a document both the HTML page and its source, whichever of
the two (or the source file's path) you pass — and removes its feed and
index entry. It returns `{"deleted": [keys]}`, empty if there was
nothing. Images uploaded beside a page are kept, as other pages may use
them.

Deleting is permanent and breaks links people may have. **Confirm with
the user before deleting**, naming what will go (for a document, both
URLs). Afterwards, report the keys deleted.

## Problems

- `s3p check` reports where settings were read from. If profiles are
  missing, the user's settings live somewhere else: they can point
  `S3P_SETTINGS_FILE` at the file holding their
  `(custom-set-variables '(s3-publish-profiles ...))`.
- "No credentials found": the user's auth-source has no entry for
  `s3-publish-PROFILE`. If their auth file is GPG-encrypted, gpg-agent
  must be able to decrypt it without a prompt.
