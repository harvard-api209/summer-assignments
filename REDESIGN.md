# API-209 Summer Assignment Redesign Spec

## Direction

The summer assignment experience should feel like a guided course player, not a
generic documentation site. The student should always know:

- where they are in the path;
- what to do next;
- where the work happens;
- which file matters; and
- how to get unstuck.

The redesign applies to both the website and the Posit Cloud assignment
materials. The website provides orientation and support. Posit Cloud is where
students complete the R Markdown work. Canvas is where completed `.Rmd` files
are submitted.

## Design Model

Use a hybrid of:

- Duolingo-style path clarity;
- DataCamp-style practice loops; and
- CourseKata-style scaffolded lessons and notebooks.

The tone should be warm, modern, lightly playful, and academically serious. Do
not use mascots, streaks, points, confetti, badges, or animation-dependent
learning flow.

## Website Principles

- Home is a course dashboard first and a reference page second.
- The first screen should emphasize the next action and the full assignment
  path.
- The path should be static and clickable, not an official progress tracker.
- Support pages should remain available, but they should not compete with the
  primary path.
- Use micro-interactions only: hover states, focus states, and gentle movement.
- Respect `prefers-reduced-motion` when adding animation.

## Home Prototype

The Home page should include:

1. A compact hero with the core promise and next action.
2. A course map showing the five-step path:
   - Getting Started
   - Warm-up
   - Assignment 1
   - Parts 2-5
   - Submit on Canvas
3. A current action panel that points students to Getting Started first.
4. A platform map:
   - Website = instructions and support
   - Posit Cloud = assignment work
   - Canvas = final submission
5. A support strip.
6. Preserved but compressed course principles, data, AI, and assignment roadmap
   content below the dashboard.

## Posit Assignment Rhythm

R Markdown files should use a consistent high-level rhythm, with scaffolding
that fades over time:

1. Goal
2. Why this matters for API-209 / MPA/ID
3. Concept
4. Try it
5. Check your output
6. Explain in words
7. Knit checkpoint

Part 1 should be the most scaffolded. Parts 2-4 should keep the same rhythm but
ask students to take more responsibility for checking filters, denominators, and
missing values. Part 5 should become a short memo-style capstone with fewer
hints and more judgment.

## Assignment 1 Prototype

Assignment 1 should add:

- a bridge from the website and warm-up into Posit Cloud;
- learning goals;
- a short explanation of why fundamentals matter for API-209 and MPA/ID;
- repeated concept / try / check / explain / knit sections;
- a clear submission note: submit the completed `.Rmd`, not knitted HTML unless
  asked;
- an AI-use note focused on debugging and verification.

The prototype should preserve the existing learning objectives and basic
content. It should improve rhythm and clarity, not inflate difficulty.
