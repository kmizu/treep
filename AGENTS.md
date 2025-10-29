# Repository Guidelines

## Project Structure & Module Organization
- Use a simple, predictable layout:
  - `src/` – application/library code
  - `tests/` – automated tests mirror `src/`
  - `scripts/` – one-off dev/CI scripts
  - `assets/` – static files (images, sample data)
  - `docs/` – docs and design notes
- Keep modules small and focused. Prefer `src/pkg_name/` over flat files.

## Build, Test, and Development Commands
- Build: `sbt compile`
- Run CLI:
  - `sbt "run new"` – サンプル `samples/hello.treep` を生成
  - `sbt "run build"` – `*.treep` を解析→マクロ展開まで検証（生成物なし）
  - `sbt "run run"` – 素朴インタプリタで実行
- Format/Lint: 現状なし（Scala 側は標準オプションのみ）。
- Test: `sbt test`（MUnit）。
- 必須チェック: 変更を加えたら毎回 `sbt compile` → `sbt test` を実行し、ビルドとテストが通ることを確認すること。

## Coding Style & Naming Conventions
- Indentation: 2 spaces (JS/TS, JSON, YAML), 4 spaces (Python).
- Naming:
  - Python: `snake_case` for functions/vars, `PascalCase` for classes, `lower_snake.py` files.
  - JS/TS: `camelCase` for functions/vars, `PascalCase` for classes/components, `kebab-case.ts[x]` files.
- Formatting/Linting (enable if stack applies):
  - JS/TS: Prettier + ESLint (`npm run lint:fix`).
  - Python: Black + Ruff (`ruff format . && ruff check .`).

## Testing Guidelines
- Mirror `src/` in `tests/`. Example: `src/utils/date.py` → `tests/utils/test_date.py`.
- Naming: `test_*.py` (pytest), `*.spec.ts(x)` (Jest/Vitest), `*_test.go` (Go).
- Aim for ≥80% coverage on changed code. Keep tests deterministic and isolated.
- Quick run examples: `pytest -q`, `npm test --silent`, `go test ./...`.

## Commit & Pull Request Guidelines
- Use Conventional Commits: `feat:`, `fix:`, `chore:`, `docs:`, `refactor:`, `test:`.
- Commits: imperative mood, ≤72 char subject, body explains why + impact.
- PRs: clear description, link issues (`Closes #123`), include screenshots for UI, note breaking changes.
- Keep PRs small and focused; include `AGENTS.md` updates when behavior or workflow changes.

## Security & Configuration
- Do not commit secrets. Use `.env` with `.env.example` and document required vars.
- Respect `.gitignore` (e.g., `node_modules/`, `.venv/`, `.env`, `dist/`).

## Agent-Specific Instructions
- 応答は「はんなり北摂弁」で行うこと。
- 人格設定: 「優しい性格の幼馴染」として、穏やかで思いやりある口調、背中を押す一言、過度に馴れ馴れしくならない礼儀を保つ。
- Follow this file’s conventions for any edits.
- Prefer minimal, targeted changes; update docs/tests alongside code.
- Before large refactors, open an issue or draft PR for alignment.
