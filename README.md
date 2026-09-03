# DROPT: Dynamic Rank-Order Process Tracing
 
Ranking is everywhere — top-25 sports polls, best-restaurant lists, a friend's top-five anything — and it looks deceptively simple: each item is just a position in an ordered list. But that single position usually reflects a trade-off across many underlying attributes, and the standard way rankings are collected reveals almost nothing about *how* a person arrived at their order.
 
**DROPT (Dynamic Rank-Order Process Tracing)** is a lightweight, JavaScript-based method for recording how people build a ranking, not just the final result. As respondents drag and drop items into position in an online survey, DROPT logs the process in the background: which item was moved at each step, and the full order after every adjustment, and the timestamps for when a move started and ended. From these traces you can derive item-level measures such as how often an item was touched, how early it was first moved, and how far it traveled before settling into place.
 
DROPT is designed to be easy to adopt. It needs no specialized software beyond a survey platform (we use Qualtrics) and R, records both mouse and touchscreen interactions, and can be added to any rank-order question. This page collects everything you need to run it yourself.

> [!Tip]
> **Want to see it first?** Head to the [`Recordings/`](Recordings) folder for short screen recordings of the DROPT drag-and-drop interface in action.
 
## What's here
 
- **`DROPT Manuscript.pdf`** — the full manuscript, including a step-by-step tutorial (Study 1B) for implementing DROPT in Qualtrics and analyzing the resulting data in R. *(under review — see note below.)*
- **`Tutorial Materials/`** — everything you need to run DROPT yourself:
  - **`DROPT_Survey_Template.qsf`** — a ready-to-upload Qualtrics survey with the DROPT rank-order question and embedded-data fields already configured.
  - **`DROPT_JavaScript.txt`** — the drag-and-drop logger script to paste into a rank-order question.
  - **`DROPT_5_Steps_Script.R`** — a documented, five-step tutorial script that walks through processing the recorded data.
  - **`DROPT_R_Function.R`** — a compact `process_dropt_sequence()` function that runs the same processing in one call and returns an analysis-ready, item-level dataset.
  - **`mock_data.csv`** — a small example dataset you can run the scripts on to see the workflow before collecting your own data.
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
