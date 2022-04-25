-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later
create table
  public.account (
    "account_id" bigserial primary key,
    "phone" varchar(20) unique not null,
    "created_at" timestamp not null default current_timestamp,
    "activated_at" timestamp default null
  );

create type c_scope as enum ('SignIn');

create table
  public.confirmation (
    "account_id" bigserial,
    "scope" c_scope not null,
    "code" varchar(6) not null,
    "created_at" timestamp not null default current_timestamp,
    "expired_at" timestamp not null,
    "used" boolean not null default false,
    constraint fk_confirmation_account foreign key("account_id") references account("account_id")
    on delete cascade
  );

create table
  public.auth_log (
    "account_id" bigserial,
    "code" varchar(6) not null,
    "ip" varchar(64) not null,
    "client" varchar(256),
    "created_at" timestamp not null default current_timestamp,
    constraint fk_auth_log_account foreign key("account_id") references account("account_id")
    on delete cascade
  );
