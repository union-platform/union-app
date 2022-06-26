-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later
create table
  public.profile (
    "account_id" bigserial references account on delete cascade
  , "name" varchar(32) not null
  );
