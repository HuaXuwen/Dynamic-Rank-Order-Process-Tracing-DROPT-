# DROPT: Dynamic Rank-Order Process Tracing
 
Ranking is everywhere — top-25 sports polls, best-restaurant lists, a friend's top-five anything — and it looks deceptively simple: each item is just a position in an ordered list. But that single position usually reflects a trade-off across many underlying attributes, and the standard way rankings are collected reveals almost nothing about *how* a person arrived at their order.
 
**DROPT (Dynamic Rank-Order Process Tracing)** is a lightweight, JavaScript-based method for recording how people build a ranking, not just the final result. As respondents drag and drop items into position in an online survey, DROPT logs the process in the background: which item was moved at each step, the timestamps for when a move started and ended, and the full order after every adjustment. From these traces you can derive item-level measures such as how often an item was touched, how early it was first moved, and how far it traveled before settling into place.
 
Across our studies, this process data reveals a consistent **"3F" pattern**: people move the items scoring highest on the task-relevant attribute more **frequently**, **first**, and **farther** up the list — behavior consistent with a selection-sort strategy. We also show that these drag-and-drop patterns are associated with underlying preferences and can improve the prediction of later choices beyond what the final ranking alone provides.
 
DROPT is designed to be easy to adopt. It needs no specialized software beyond a survey platform (we use Qualtrics) and R, records both mouse and touchscreen interactions, and can be added to any rank-order question. This page collects everything you need to run it yourself.
 
## What's here
 
- **The paper** — the full manuscript, including a step-by-step tutorial (Study 1B) for implementing DROPT in Qualtrics and analyzing the resulting data in R. *(Preprint / under review — see note below.)*
- **Qualtrics template (`.qsf`)** — a ready-to-upload survey with the DROPT rank-order question and embedded-data fields already configured.
- **JavaScript logger** — the drag-and-drop recording script to paste into a rank-order question.
- **R scripts** — a documented five-step tutorial script, plus a compact `process_dropt_sequence()` function that returns an analysis-ready, item-level dataset in a single call.
- **Video walkthroughs** — short screencasts showing setup and analysis from end to end.
## Getting started
 
1. Upload the `.qsf` template to your Qualtrics account, or add the JavaScript to your own rank-order question.
2. Create the four embedded-data fields, then run a test response to confirm the logs populate.
3. Export your data and process it in R with the tutorial script or the one-call `process_dropt_sequence()` function.
## Citation
 
> Hua, X.\*, Trieb, A.\*, Ludwig, J., Sugerman, E. R., & Johnson, E. J. (2026). *DROPT: Dynamic Rank-Order Process Tracing.* Manuscript under review.
>
> \* Equal contribution.
 
## A note on the manuscript
 
This is a preprint / author's version and has **not yet been peer reviewed**; the findings are provisional and may change. Please cite accordingly.
 
<!-- Team: edit this note as you prefer. Springer Nature (BRM's publisher) permits sharing the
author's version on a project or institutional page at any point during review, so a restrictive
"do not share without permission" label is optional rather than required. Keep it if you'd rather
hold the manuscript close; otherwise the lighter preprint framing above is policy-aligned. -->
 
## Contact
 
Questions or feedback: Arian Trieb (trieb@wharton.upenn.edu) or Xuwen Hua (xuwenhua@stanford.edu).
