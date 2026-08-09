# shakespeare2

This is primarily a showcase piece exemplifying two more substantial projects, simultaneously:

- **[web-skeleton](https://github.com/tjohncan/web-skeleton)** —
  a web framework in Common Lisp. **Hard build dependency** —
  shakespeare2's ASDF system declares `:depends-on ("web-skeleton")` and won't compile without it.
  This repo is a thin application on top: routing, WebSocket streaming,
  static files, non-blocking fetch, and crypto primitives —
  all lean on the framework for the HTTP layer.

- **[auth-server](https://github.com/tjohncan/auth-server)** —
  an OAuth2 authorization server written in C.
  **Not a build dependency**, not part of the Docker Compose setup,
  and not required at runtime at all:
  shakespeare2's auth module is feature-gated (`SHAKESPEARE2_AUTH=true`).
  When auth is on, shakespeare2 acts as a *confidential client*
  (tokens live server-side; the browser only sees an opaque session cookie)
  against any RFC-compliant authorization-code + PKCE provider.
  The endpoint paths are env-configurable and the defaults simply match auth-server.
  The localhost admin relay (`src/admin.lisp`) does target auth-server's specific `/api/rs/*`
  resource-server API, but would work against any provider that happens to expose the same routes.

The app itself is just-for-fun. Type an inspiration phrase, get back a short poem
streamed token-by-token from a local Ollama model
wearing a Shakespeare persona defined in `SPIRIT.md`.

The two parent projects are the pieces worth reading or reusing in their own right.
This repo exists to demonstrate them working (together) end-to-end.

## Integration surface

From **auth-server**: `GET /authorize`, `POST /token`, `POST /revoke`,
and the user-provisioning set `POST /api/rs/users`,
`POST /api/rs/users/lookup`, `POST /api/rs/client-users`,
`DELETE /api/rs/client-users`, `POST /api/rs/client-users/list`.

From **web-skeleton**: WebSocket streaming (`ws-send` per token),
`http-fetch` async callback (for the server-side OAuth2 token exchange),
`defer-to-fetch` (for the admin relay), `http-fetch-stream` (for the Ollama
NDJSON stream), the JSON reader and writer (`json-parse`, `json-get`,
`make-json-object`, `json-object-p`), `get-cookie`, and the
`base64url-encode` / `sha256` / `constant-time-equal` crypto primitives.

## Layout

- `src/` — the Lisp application
  - `handler.lisp` — HTTP routing, WebSocket protocol, server entry point
  - `ollama.lisp` — streaming client for Ollama's `/api/generate`
  - `config.lisp` — all runtime knobs (env-driven)
  - `auth.lisp`, `admin.lisp` — OAuth2 client flow + localhost admin API,
    compiled in when `SHAKESPEARE2_AUTH=true`
- `static/` — single-page frontend (vanilla JS, typewriter streaming)
- `nginx/` — reverse-proxy configs for plain-HTTP and direct-TLS deploys
- `Dockerfile`, `docker-compose*.yml`, `Makefile` — build + deploy
- `run.lisp`, `build.lisp` — dev REPL and production entries (both load `bootstrap.lisp` for the shared preamble)
- `SPIRIT.md` — the system prompt defining the poet's voice
- `.env.example` — runtime knobs with commentary
