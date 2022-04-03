#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2022 Union
#
# SPDX-License-Identifier: AGPL-3.0-or-later

# Setup temporary DB for testing.
# Dependencies:
# * Postgres server
# * pg_tmp (http://eradman.com/ephemeralpg/)

# pg_tmp shutdowns and cleans up automatically after a period of inactivity,
# so we don't need to clean up manually.
echo "$(pg_tmp)&options=-c%20client_min_messages\%3DWARNING"
