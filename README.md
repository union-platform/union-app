<!--
 - SPDX-FileCopyrightText: 2021 Union
 -
 - SPDX-License-Identifier: AGPL-3.0-or-later
 -->

# Union

[![License: AGPL-3.0](https://img.shields.io/badge/License-AGPL%203.0-blue.svg)](https://opensource.org/licenses/AGPL-3.0)
[![Licenses CI](https://github.com/union-platform/union-app/actions/workflows/licenses.yml/badge.svg)](https://github.com/union-platform/union-app/actions/workflows/licenses.yml)
[![Haskell CI](https://github.com/union-platform/union-app/actions/workflows/backend.yml/badge.svg)](https://github.com/union-platform/union-app/actions/workflows/backend.yml)
[![Deploy](https://github.com/union-platform/union-app/actions/workflows/deployment.yml/badge.svg)](https://github.com/union-platform/union-app/actions/workflows/deployment.yml)


Union is a platform where you can find your labor of love, join a team, create
something important or crazy; this is a place where you can be yourself.

## How to use [↑](#union)

This repo contains all needed to server Union web application. For more details
you can read [backend readme](./backend/README.md) and [fronted readme](./frontend/README.md).

For easier usage Union is packed into Docker, so you can just navigate to
[docker folder](./docker) and run `docker compose up -d` there. It will spin up
all required infrastructure - backend (e.g. server), database, web-server and
frontend (TODO).

Note:
You can get error that there is missing volumes - this is because it's specified
that we use external storage for some data. We need id to keep changes after
docker restart.
To fix this you need to create required volumes before first run with command
`docker volume create NAME`. You can find volume names in [compose file](./docker/compose.yml)
(see `volumes` section with `external: true`).

## Issue tracker [↑](#union)

We use GitHub issues as our issue tracker.
You can login using your GitHub account to leave a comment or create a new issue.

## For Contributors [↑](#union)

Please see [CONTRIBUTING.md](./CONTRIBUTING.md) for more information.
