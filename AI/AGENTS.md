# Global Development Guidelines

You are an AI assistant. Always follow these rules across **all projects**.

## Communication
- **Language:** Always respond in English.
- **Clarity:** Be concise and factual. Avoid unnecessary preambles ("You're right, here is..." -> just answer).
- **Proactive:** If you lack context (e.g., file paths, dependencies), ask for clarification immediately.

## Coding Principles (language-agnostic)
- **KISS (Keep It Simple):** Prefer simple, readable solutions over complex ones.
- **DRY (Don't Repeat Yourself):** Abstract common logic into reusable functions/modules.
- **Idiomatic Code:** Follow the conventions and best practices of the specific language you are writing in.
- **Error Handling:** Never silently ignore errors. Always handle or explicitly propagate them.
- **Testing:** When writing or modifying code, suggest or include relevant tests.

## Git & Version Control
- **Confirm before destructive actions:** Never run `git push --force`, `git reset --hard`, or `git clean -fd` without explicit confirmation.
- **Commit messages:** Follow Conventional Commits (e.g., `feat: add login`, `fix: resolve null pointer`, `docs: update README`).
- **Small commits:** Prefer atomic, focused commits over large, mixed changes.

## Security & Sensitive Data
- **No hardcoded secrets:** Never suggest or commit API keys, passwords, or tokens.
- **Environment variables:** Use `.env` files or environment variables for configuration.
- **Review before commit:** If you suspect a file contains secrets, warn the user.

## Project Context Detection
- **Priority order:**
  1. Local `AGENTS.md` or `CLAUDE.md` at project root (overrides these global rules for that project).
  2. This global file as fallback.
- **Respect project-specific tooling:** If a project uses `pnpm`, `cargo`, `uv`, or `make`, use those commands instead of generic alternatives.

## Responses Structure
- **Code blocks:** Always specify the language (e.g., ` ```python `, ` ```rust `, ` ```bash `).
- **Explanations:** Keep inline comments to a minimum unless the logic is non-obvious.
- **UI/UX:** When proposing CLI commands, show the command and its expected output separately.

## Final Rule for Conflicts
If any rule above conflicts with a local `AGENTS.md` or `opencode.json`, **the local configuration wins**.
