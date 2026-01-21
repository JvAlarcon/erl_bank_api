DROP DATABASE IF EXISTS erl_bank;
CREATE DATABASE erl_bank;

\c erl_bank;

BEGIN;
CREATE TABLE customers (
    id SERIAL UNIQUE,
    cpf VARCHAR(14) NOT NULL UNIQUE,
    name VARCHAR(60) NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY(id, cpf, created_at)
);

CREATE TABLE account_types (
   id SERIAL PRIMARY KEY,
   description TEXT NOT NULL UNIQUE
);

CREATE TABLE accounts (
    id SERIAL UNIQUE,
    id_type INTEGER NOT NULL REFERENCES account_types(id) ON DELETE CASCADE,
    id_customer INTEGER NOT NULL REFERENCES customers(id) ON DELETE CASCADE,
    balance NUMERIC(10,2) NOT NULL DEFAULT 0 CONSTRAINT accounts_balance_positive CHECK (balance >= 0),
    PRIMARY KEY(id, id_type, id_customer)
);

CREATE TABLE balance_operations (
    id SERIAL PRIMARY KEY,
    description TEXT NOT NULL UNIQUE
);

CREATE TABLE statement_history (
    id_account_origin INTEGER NOT NULL REFERENCES accounts(id) ON DELETE CASCADE,
    id_account_target INTEGER NOT NULL REFERENCES accounts(id) ON DELETE CASCADE,
    id_balance_operation INTEGER NOT NULL REFERENCES balance_operations(id) ON DELETE CASCADE,
    executed_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY(id_account_origin, id_balance_operation, id_account_target, executed_at)
);

INSERT INTO account_types(description)
VALUES
    ('checking'), -- Default everyday account
    ('savings'), -- Money storage account
    ('internal'); -- Used for internal tests

INSERT INTO balance_operations(description)
VALUES
    ('deposit'),
    ('withdraw'),
    ('transfer');

-------------------------------------------------
-- Utility functions to be used by procedures
-------------------------------------------------
CREATE OR REPLACE FUNCTION insert_in_customers(cpf VARCHAR(14), name VARCHAR(60))
LANGUAGE plpgsql
RETURNS INTEGER AS $$
DECLARE
  new_customer_id INTEGER;
BEGIN
    INSERT INTO customers(cpf, name)
    VALUES(cpf, name)
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

-- Insert into accounts

CREATE OR REPLACE FUNCTION deposit_in_accounts(id_account int, amount numeric(10,2))
LANGUAGE plpgsql
RETURNS boolean AS $$
BEGIN
    IF amount <= 0 THEN
        RAISE EXCEPTION 'Deposit amount must be greater than 0';
    END IF;

    UPDATE accounts AS a
    SET a.balance = a.balance + amount
    WHERE a.id = id_account;

    IF NOT FOUND THEN
       RAISE EXCEPTION 'Account % not found', id_account;
    END IF;

    RETURN FOUND;
END; $$;

CREATE OR REPLACE FUNCTION withdraw_from_accounts(id_account int, amount numeric(10,2))
LANGUAGE plpgsql
RETURNS boolean AS $$
DECLARE
  current_balance numeric(10,2);
BEGIN
    IF amount <= 0 THEN
        RAISE EXCEPTION 'Withdraw amount must be greater than 0';
    END IF;

    current_balance := SELECT balance INTO current_balance FROM accounts WHERE id = id_account;

    UPDATE accounts AS a
    SET a.balance = current_balance - amount
    WHERE a.id = id_account;

    IF NOT FOUND THEN
       RAISE EXCEPTION 'Account % not found', id_account;
    END IF;

    RETURN FOUND;
EXCEPTION
    WHEN accounts_balance_positive THEN
        RAISE EXCEPTION 'Insufficient funds!';
    WHEN OTHER THEN
        RAISE;
END; $$;

COMMIT;
