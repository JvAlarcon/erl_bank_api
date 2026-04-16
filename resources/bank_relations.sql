DROP DATABASE IF EXISTS erl_bank;
CREATE DATABASE erl_bank;

\c erl_bank;

BEGIN;

-------------------------------------------------
-- Entities relations
-------------------------------------------------
 
CREATE TABLE customers (
  document_number TEXT NOT NULL UNIQUE,
  name TEXT NOT NULL,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  id TEXT NOT NULL UNIQUE GENERATED ALWAYS AS (
    encode(sha256(CONCAT_WS('|', document_number, created_at)::bytea), 'hex')
  ) VIRTUAL,
  PRIMARY KEY(id)
-- The ideia behind this id is to have a surrogate key with a meaning
-- It's not a number to be a number, but a number that relates to field essential to entity
-- This idea becomes clear if you know about essence and accidents
);

-- Type entity, a simple surrogated key does the job here
CREATE TABLE account_types (
  id SERIAL PRIMARY KEY,
  label TEXT NOT NULL UNIQUE
);

CREATE TABLE accounts (
  id SERIAL UNIQUE,
  id_customer INTEGER NOT NULL REFERENCES customers(identity_document) ON DELETE CASCADE,
  id_type INTEGER NOT NULL REFERENCES account_types(id) ON DELETE CASCADE,
  balance INTEGER NOT NULL DEFAULT 0 CONSTRAINT accounts_balance_positive CHECK (balance >= 0),
  id TEXT NOT NULL UNIQUE GENERATED ALWAYS AS (
    encode(sha256(CONCAT_WS('|', id_customer, id_type)::bytea), 'hex')
  ) VIRTUAL,
  PRIMARY KEY(id)
);

CREATE TABLE transaction_status (
  id SERIAL PRIMARY KEY,
  label TEXT NOT NULL UNIQUE
);

CREATE TABLE transactions (
  id_status INTEGER NOT NULL REFERENCES transactions_status(id) ON DELETE CASCADE,
  id_account INTEGER NOT NULL REFERENCES accounts(id) ON DELETE CASCADE,
  started_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  trace_id UUID NOT NULL UNIQUE DEFAULT uuidv7(),
  id TEXT NOT NULL UNIQUE GENERATED ALWAYS AS (
    encode(sha256(CONCAT_WS('|', id_account, started_at, trace_id)::bytea), 'hex')
  ) VIRTUAL,
  PRIMARY KEY(id)
);

CREATE TABLE transfer_kind (
  label VARCHAR(20) NOT NULL,
  PRIMARY KEY(label)
);

CREATE TABLE transfers (
  id_transaction INTEGER NOT NULL UNIQUE REFERENCES transactions(id),
  trace_id UUID NOT NULL UNIQUE REFERENCES transactions(trace_id),
  from_account INTEGER NOT NULL,
  kind VARCHAR(20) NOT NULL REFERENCES transfer_kind(label),
  executed_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  id TEXT NOT NULL UNIQUE GENERATED ALWAYS AS (
    encode(sha256(CONCAT_WS('|', id_transaction, kind)::bytea), 'hex')
  ) VIRTUAL,
  PRIMARY KEY(id)
);

-- Entity son of transfers
-- It will only be inserted here if kind if equal to withdrawal
-- The others son's entities will have the same behavior
CREATE TABLE withdrawals (
  id_transfer TEXT NOT NULL UNIQUE,
  amount INTEGER NOT NULL CONSTRAINT withdrawals_amount_positive CHECK(amount >= 0),
  kind VARCHAR(20) NOT NULL CHECK(kind = 'withdrawal'),
  FOREIGN KEY (kind) REFERENCES transfer_kind(label),
  FOREIGN KEY (id_transfer, kind) REFERENCES transfers(id, kind),
  PRIMARY KEY(id_transfer)
);

CREATE TABLE deposits (
  id_transfer TEXT NOT NULL UNIQUE,
  amount INTEGER NOT NULL CONSTRAINT deposits_amount_positive CHECK(amount >= 0),
  kind VARCHAR(20) NOT NULL CHECK(kind = 'deposit'),
  FOREIGN KEY (kind) REFERENCES transfer_kind(label),
  FOREIGN KEY (id_transfer, kind) REFERENCES transfers(id, kind),
  PRIMARY KEY(id_transfer)
);

CREATE TABLE pix (
  id_transfer TEXT NOT NULL UNIQUE,
  amount INTEGER NOT NULL CONSTRAINT pix_amount_positive CHECK(amount >= 0),
  target_key TEXT NOT NULL,
  kind VARCHAR(20) NOT NULL CHECK(kind = 'pix'),
  FOREIGN KEY (kind) REFERENCES transfer_kind(label),
  FOREIGN KEY (id_transfer, kind) REFERENCES transfers(id, kind),
  PRIMARY KEY (id_transfer)
);

INSERT INTO account_types(label)
VALUES
    ('checking'), -- Default everyday account
    ('savings'), -- Money storage account
    ('internal'); -- Used for internal tests

INSERT INTO transaction_status(label)
VALUES
    ('pending'),
    ('processing'),
    ('finished');

INSERT INTO transfer_kind(label)
VALUES
    ('withdrawal'),
    ('deposit'),
    ('pix');

-------------------------------------------------
-- Views
-------------------------------------------------
-- Create a view for customer account data
-- Create a view for trasactions statement
-- Don't forget to search for updatable views



-- Create CRUD operations to be called by erlang application

COMMIT;
