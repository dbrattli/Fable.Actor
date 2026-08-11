# Repository Guidelines

## Agent Decision Comments

This repository uses Agent Decision Comments (ADCs).
Follow specification 0.1.0 at <https://github.com/dbrattli/adc>, currently
pinned to commit `51310dbb60ba960890f431fe06ed701873eadb8b` because no upstream release or
tag exists yet.

ADCs are source-level comments using four directives:

- `decision:` records a deliberate choice and its reason.
- `invariant:` records a falsifiable property the code must preserve.
- `assumption:` records an external belief the code does not guarantee.
- `tradeoff:` records a cost accepted for a concrete benefit.

Place ADCs in the nearest comment or documentation comment attached to the code
they govern. Use one directive per line, present tense, and describe rationale
that is not already evident from the implementation, types, or tests. In
documentation comments, write normal API prose first and leave a blank line
before the directives. Do not add ADCs mechanically to trivial code.

Before modifying code, read all active ADCs in the affected scope.
Preserve them or update them explicitly.
Add comments for non-obvious decisions, invariants, assumptions, and tradeoffs
introduced by a change.

A change that introduces a non-obvious engineering decision or constraint is
incomplete until its ADCs are present and consistent with the implementation.
Reviewers evaluate the comments before reviewing the code.
