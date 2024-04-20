---
name: Mu4e Bug Report
about: Create a report to help us improve
title: "[mu4e bug]"
labels: bug, mu4e, new
assignees: ''
---

**Describe the bug**

Give the bug a good title.

Please provide a clear and concise description of what you expected to happen
and what actually happened, and follow the steps below.

**How to Reproduce**

Include the exact steps of what you were doing (commands executed etc.). Include
any relevant logs and outputs:

- Best start from `emacs -Q`, and load a minimal `mu4e` setup; describe the steps
  that lead up to the bug.
- Does the problem happen each time? Sometimes?
- If this is about a specific (kind of) message, attach an example message.
  (Open the message, press `.` (`mu4e-view-raw-message`), then `C-x C-w` and
  attach. Anonymize as needed, all that matters is that the issue still
  reproduces.

**Environment**

Please describe the versions of OS, Emacs, mu/mu4e etc. you are using.

**Checklist**

- [ ] you are running either an 1.10.x/1.12.x release or `master` (otherwise please upgrade)
- [ ] you can reproduce the problem without 3rd party extensions (including Doom/Evil, various extensions etc.)
- [ ] you have read all of the above

Please make sure you all items in the checklist are set/met before filing the ticket.

Thank you!
