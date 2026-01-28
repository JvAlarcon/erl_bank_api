DROP DATABASE IF EXISTS erl_bank;
CREATE DATABASE erl_bank;

\c erl_bank;

BEGIN;
CREATE TABLE customers (
    id SERIAL UNIQUE,
    name VARCHAR(60) NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY(id, created_at)
);

CREATE TABLE account_types (
   id SERIAL PRIMARY KEY,
   description TEXT NOT NULL UNIQUE
);

CREATE TABLE accounts (
    id SERIAL UNIQUE,
    id_type INTEGER NOT NULL REFERENCES account_types(id) ON DELETE CASCADE,
    id_customer INTEGER NOT NULL REFERENCES customers(id) ON DELETE CASCADE,
    balance INTEGER NOT NULL DEFAULT 0 CONSTRAINT accounts_balance_positive CHECK (balance >= 0),
    PRIMARY KEY(id, id_type, id_customer)
);

CREATE TABLE deposits (
  id SERIAL UNIQUE,
  id_account INTEGER NOT NULL REFERENCES accounts(id) ON DELETE CASCADE,
  executed_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  amount INTEGER NOT NULL CONSTRAINT CHECK(amount >= 0),
  PRIMARY KEY(id, id_account, executed_at)
);

CREATE TABLE withdrawals (
  id SERIAL UNIQUE,
  id_account INTEGER NOT NULL REFERENCES accounts(id) ON DELETE CASCADE,
  executed_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  amount INTEGER NOT NULL CONSTRAINT CHECK(amount >= 0),
  PRIMARY KEY(id, id_account, executed_at)
);

CREATE TABLE transfer (
  id SERIAL UNIQUE,
  id_account_from INTEGER NOT NULL REFERENCES accounts(id) ON DELETE CASCADE,
  id_account_to INTEGER NOT NULL REFERENCES account(id) ON DELETE CASCADE,
  executed_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  amount INTEGER NOT NULL CONSTRAINT CHECK(amount >= 0)
);


INSERT INTO account_types(description)
VALUES
    ('checking'), -- Default everyday account
    ('savings'), -- Money storage account
    ('internal'); -- Used for internal tests


-------------------------------------------------
-- Customer and Account creation
-------------------------------------------------
CREATE OR REPLACE FUNCTION insert_in_customers(name VARCHAR(60))
LANGUAGE plpgsql
RETURNS INTEGER AS $$
DECLARE
  new_customer_id INTEGER;
BEGIN
    INSERT INTO customers(name)
    VALUES(name)
    RETURNING id INTO new_customer_id;

    RETURN new_customer_id;
END; $$;

CREATE OR REPLACE FUNCTION update_in_customers(id_customer INTEGER, name VARCHAR(60))
LANGUAGE plpgsql
RETURNS BOOLEAN AS $$
BEGIN
    UPDATE customers AS c
    SET c.name = name
    WHERE c.id = id_customer;

    IF NOT FOUND THEN
       RAISE EXCEPTION 'Account % not found', id_account;
    END IF;

    RETURN FOUND;
END; $$;

CREATE OR REPLACE FUNCTION insert_in_accounts(id_customer INTEGER, account_type INTEGER)
LANGUAGE plpgsql
RETURN INTEGER AS $$
DECLARE
  new_account_id INTEGER;
BEGIN
    INSERT INTO accounts(id_customer, id_type)
    VALUES(id_customer, account_type)
    RETURNING id INTO new_account_id;

    RETURN new_account_id;
END; $$;

CREATE OR REPLACE PROCEDURE create_customer_and_account(name VARCHAR(60), account_type INTEGER)
LANGUAGE plpgsql AS $$
DECLARE
  new_customer_id INTEGER;
  new_account_id INTEGER;
BEGIN
    new_customer_id := SELECT insert_in_customers(name);
    new_account_id := SELECT insert_in_accounts(new_customer_id, account_type);
END; $$;

CREATE OR REPLACE PROCEDURE create_account(customer_id INTEGER, account_type INTEGER)
LANGUAGE plpgsql AS $$
DECLARE
  new_account_id INTEGER;
BEGIN
    new_account_id := SELECT insert_in_accounts(customer_id, account_type);
END; $$;



COMMIT;
