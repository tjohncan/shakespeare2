.PHONY: up down logs logs-all rebuild pull-model shell ps \
        nginx-prod-config deploy-tls up-tls down-tls rebuild-tls \
        renew-cert

# Production overlay: include both compose files when DOMAIN is set.
PROD_FILES := -f docker-compose.yml -f docker-compose.prod.yml

# --- Localhost / ALB-fronted deploys ---------------------------------------

up:
	docker compose up -d

down:
	docker compose down

# App logs only — most useful while developing the CL side.
logs:
	docker compose logs -f app

# Every container's logs interleaved — useful for startup debugging.
logs-all:
	docker compose logs -f

# Rebuild after code changes (Dockerfile, src/, static/). No service arg —
# scoping to `app` would skip nginx, since nginx depends on app, not the
# other way round.
rebuild:
	docker compose up -d --build

# Force a model re-pull (normally handled automatically by ollama-bootstrap).
# --profile is named rather than left to compose's auto-activation, so this
# behaves the same whether or not COMPOSE_PROFILES is set: asking to pull a
# model is unambiguous about wanting the model.
pull-model:
	docker compose --profile ollama run --rm ollama-bootstrap

# Drop a shell in the app container for poking around.
shell:
	docker compose exec app sh

ps:
	docker compose ps

# --- Direct-TLS deploys (nginx + certbot on this host) --------------------
# These targets require DOMAIN (and ACME_EMAIL on first run) in .env or on
# the command line, e.g. `make deploy-tls DOMAIN=foo.example.com ACME_EMAIL=x@y`.

# Generate nginx.prod.conf from the template. Re-run whenever DOMAIN changes.
nginx-prod-config:
	@[ -n "$(DOMAIN)" ] || (echo "DOMAIN must be set" >&2; exit 1)
	DOMAIN=$(DOMAIN) envsubst '$${DOMAIN}' \
	  < nginx/nginx.prod.conf.template \
	  > nginx/nginx.prod.conf
	@echo "wrote nginx/nginx.prod.conf for DOMAIN=$(DOMAIN)"

# First-time deploy on a fresh host. Assumes DNS A record already points
# here. Obtains an initial cert in standalone mode (port 80 must be free),
# then brings the full stack up.
deploy-tls: nginx-prod-config
	@[ -n "$(DOMAIN)" ] || (echo "DOMAIN must be set" >&2; exit 1)
	@[ -n "$(ACME_EMAIL)" ] || (echo "ACME_EMAIL must be set" >&2; exit 1)
	docker run --rm -p 80:80 \
	  -v /etc/letsencrypt:/etc/letsencrypt \
	  certbot/certbot certonly --standalone \
	  -d $(DOMAIN) --email $(ACME_EMAIL) \
	  --agree-tos --non-interactive
	docker compose $(PROD_FILES) up -d

# Bring the TLS stack up/down without re-issuing a cert (use after the
# initial deploy-tls). Assumes nginx/nginx.prod.conf already exists.
up-tls:
	docker compose $(PROD_FILES) up -d

down-tls:
	docker compose $(PROD_FILES) down

rebuild-tls:
	docker compose $(PROD_FILES) up -d --build

# Force an immediate cert renewal + nginx reload. Normally the certbot
# container loops on this every 12h on its own.
renew-cert:
	docker compose $(PROD_FILES) run --rm certbot \
	  renew --webroot -w /var/www/certbot
	docker compose $(PROD_FILES) exec nginx nginx -s reload
