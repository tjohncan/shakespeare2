# Build context must be the parent directory of shakespeare2/ so that
# both web-skeleton/ and shakespeare2/ are accessible.
# Use docker-compose (which sets context: ../) rather than plain docker build.

# ---------------------------------------------------------------------------
# Stage 1 — build the SBCL binary
# ---------------------------------------------------------------------------
FROM debian:trixie-slim AS builder

# libssl-dev is needed when SHAKESPEARE2_AUTH=true so that the build step can
# load web-skeleton-tls (outbound HTTPS to the auth server). It's cheap to
# install unconditionally and keeps the build matrix simple.
RUN apt-get update && apt-get install -y --no-install-recommends \
        sbcl \
        libssl3 \
        ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build

# Copy both sibling repos (context is their shared parent).
COPY web-skeleton/ web-skeleton/
COPY shakespeare2/ shakespeare2/

WORKDIR /build/shakespeare2

# AUTH=true bakes OAuth2 support into the binary via CL *features*.
ARG AUTH=false
RUN SHAKESPEARE2_AUTH=${AUTH} sbcl --non-interactive --load build.lisp

# ---------------------------------------------------------------------------
# Stage 2 — minimal runtime image
# ---------------------------------------------------------------------------
FROM debian:trixie-slim

# libssl3 for outbound HTTPS (auth server); ca-certificates so TLS trusts
# standard public CAs; tini for proper PID-1 signal handling.
RUN apt-get update && apt-get install -y --no-install-recommends \
        libssl3 \
        ca-certificates \
        tini \
    && rm -rf /var/lib/apt/lists/* \
    && groupadd --system --gid 1000 app \
    && useradd  --system --uid 1000 --gid app --home /app --shell /usr/sbin/nologin app

WORKDIR /app

COPY --from=builder --chown=app:app /build/shakespeare2/shakespeare2 /app/shakespeare2

# Static files and default SPIRIT.md (SPIRIT.md can be overridden via volume).
COPY --chown=app:app shakespeare2/static/   /app/static/
COPY --chown=app:app shakespeare2/SPIRIT.md /app/SPIRIT.md

USER app
EXPOSE 8080

# The app listens on /healthz — use it as the container-level healthcheck.
# /dev/tcp is a bash-only feature (dash doesn't support it), so invoke bash
# explicitly — debian:trixie-slim ships bash at /bin/bash.
HEALTHCHECK --interval=30s --timeout=3s --start-period=10s --retries=3 \
  CMD bash -c 'exec 3<>/dev/tcp/127.0.0.1/${PORT:-8080} \
               && printf "GET /healthz HTTP/1.0\r\nHost: localhost\r\n\r\n" >&3 \
               && head -n 1 <&3 | grep -q "200 OK"'

ENTRYPOINT ["/usr/bin/tini", "--"]
CMD ["/app/shakespeare2"]
