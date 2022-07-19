-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later
create table
  public.interest (
    "interest_id" bigserial primary key
  , "name" varchar(32) unique not null
  );

create table
  public.interest_map (
    "interest_id" bigserial references interest on delete cascade
  , "account_id" bigserial references account on delete cascade
  , unique ("interest_id", "account_id")
  );
