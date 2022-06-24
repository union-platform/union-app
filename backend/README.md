<!--
 - SPDX-FileCopyrightText: 2021-2022 Union
 -
 - SPDX-License-Identifier: AGPL-3.0-or-later
 -->

# Union-server

To run Union you can run `union-server run`. It will launch with default
hardcoded config, if you want to provide custom config - use `-c PATH_TO_CONFIG`
option.

To see all available options run `union-server -h`.

## Documentation [↑](#Union-server)

Union serves Swagger UI with API documentation, to see it - launch backend
instance and go to `/docs`.

## Migrations [↑](#Union-server)

Union server applies all migrations at startup; migrations:

* stored in [migrations](./migrations) directory
* provided as `.sql` files
* has name convention: `[datetime]_[description].sql`
* once provided, should never be changed (new changes must be provided via new
  migrations)
* migrations has to scope consistent changes (if you need to create several
  tables related to one feature it's fine to keep it in single migration, but
  in case you provide new schema and change existing - you should split it)

Also, we define some requirements for migrations syntax:

* all sql statements should be in lowercase
* all names should be in snake case
* all field names should be escaped with `` ` `` character

You can use [prettier-sql](https://github.com/inferrinizzard/prettier-sql) with
following settings:
```
{
  ...,
  "Prettier-SQL.uppercaseKeywords": false,
  "Prettier-SQL.tabSizeOverride": 2`,
  ...
}
```

To work with migrations you can use Union executable. For more information run
`union-server migrations --help`; currently we support:

* `create` - to create empty migration file with proper name
* `list` - to show all applied migrations
* `prune` - to clean database (note, that your user must be `public` schema
  owner for that action)
