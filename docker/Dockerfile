# syntax=docker/dockerfile:1
FROM debian:bookworm-slim

# 1. Install Emacs, Pygments, and essentials
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      emacs-nox \
      ca-certificates && \
    rm -rf /var/lib/apt/lists/*

# 2. Install Org from GNU ELPA
RUN emacs --batch \
    --eval="(require 'package)" \
    --eval="(setq package-archives \
      '((\"gnu\" . \"https://elpa.gnu.org/packages/\")))" \
    --eval="(package-initialize)" \
    --eval="(package-refresh-contents)" \
    --eval="(package-install 'org)" \
    --eval="(kill-emacs)"

# 3. Copy configuration and render script
RUN mkdir -p /root/.emacs.d
COPY init.el /root/.emacs.d/init.el
COPY render.sh /usr/local/bin/render
RUN chmod +x /usr/local/bin/render
COPY zenburn.css /usr/local/share/org-css/

# 4. Set working dir and entrypoint
WORKDIR /workspace
ENTRYPOINT ["/usr/local/bin/render"]
