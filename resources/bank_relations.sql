DROP DATABASE IF EXISTS erl_bank;
CREATE DATABASE erl_bank;

\c erl_bank;

BEGIN;

-------------------------------------------------
-- Entities relations
-------------------------------------------------

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
  amount INTEGER NOT NULL CONSTRAINT deposits_amount_positive CHECK(amount >= 0),
  PRIMARY KEY(id, id_account, executed_at)
);

CREATE TABLE withdrawals (
  id SERIAL UNIQUE,
  id_account INTEGER NOT NULL REFERENCES accounts(id) ON DELETE CASCADE,
  executed_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  amount INTEGER NOT NULL CONSTRAINT withdrawals_amount_positive CHECK(amount >= 0),
  PRIMARY KEY(id, id_account, executed_at)
);

CREATE TABLE transfer (
  id SERIAL UNIQUE,
  id_account_from INTEGER NOT NULL REFERENCES accounts(id) ON DELETE CASCADE,
  id_account_to INTEGER NOT NULL REFERENCES accounts(id) ON DELETE CASCADE,
  executed_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  amount INTEGER NOT NULL CONSTRAINT transfers_amount_positive CHECK(amount >= 0)
);


INSERT INTO account_types(description)
VALUES
    ('checking'), -- Default everyday account
    ('savings'), -- Money storage account
    ('internal'); -- Used for internal tests

-------------------------------------------------
-- Views
-------------------------------------------------
CREATE OR REPLACE VIEW accounts_details AS (
    SELECT ac.id, c.name, c.updated_at, act.description
    FROM customers AS c
    INNER JOIN accounts AS ac ON c.id = ac.id_account
    INNER JOIN acccount_types AS act ON act.id = ac.id_type
    ORDER BY ac.id
);

CREATE OR REPLACE FUNCTION get_account_details(id_account INTEGER)
RETURNS accounts_details
LANGUAGE plpgsql AS $$
BEGIN
    RETURN QUERY SELECT * FROM accounts_details WHERE ac.id = id_account;
END; $$;

CREATE OR REPLACE FUNCTION list_accounts_details(row_limit INTEGER)
RETURNS SETOF accounts_details
LANGUAGE plpgsql AS $$
BEGIN
    RETURN QUERY SELECT * FROM accounts_details LIMIT row_limit;
END; $$;

CREATE OR REPLACE VIEW accounts_statement AS (
    SELECT
        a.id AS id_account, a.balance AS balance,
	d.amount AS deposit_amount, d.executed_at AS deposit_executed_at,
	w.amount AS withdrawal_amount, w.executed_at AS withdrawal_executed_at,
	t.amount AS transfer_amount, t.executed_at AS transfer_executed_at 
    FROM accounts AS a
    INNER JOIN deposits AS d ON d.id_account = a.id
    INNER JOIN withdrawals AS w ON w.id_account = a.id
    INNER JOIN transfers AS t ON
        t.id_account_from = a.id OR
        t.id_account_to = a.id
    ORDER BY a.id;
);


-------------------------------------------------
-- Customer and Account creation
-------------------------------------------------
CREATE OR REPLACE FUNCTION insert_in_customers(p_name VARCHAR(60))
RETURNS INTEGER
LANGUAGE plpgsql AS $$
DECLARE
  new_customer_id INTEGER;
BEGIN
    INSERT INTO customers(name)
    VALUES(p_name)
    RETURNING id INTO new_customer_id;

    RETURN new_customer_id;
END; $$;

CREATE OR REPLACE FUNCTION update_in_customers(id_customer INTEGER, name VARCHAR(60))
RETURNS BOOLEAN
LANGUAGE plpgsql AS $$
BEGIN
    UPDATE customers AS c
    SET c.name = name
    WHERE c.id = id_customer;

    IF NOT FOUND THEN
       RAISE EXCEPTION 'Customer % not found', id_customer;
    END IF;

    RETURN FOUND;
END; $$;

CREATE OR REPLACE FUNCTION insert_in_accounts(id_customer INTEGER, account_type INTEGER)
RETURNS INTEGER
LANGUAGE plpgsql AS $$
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
    new_customer_id := insert_in_customers(name);
    SELECT insert_in_accounts(new_customer_id, account_type);
    COMMIT;
END; $$;

CREATE OR REPLACE PROCEDURE create_account(id_customer INTEGER, account_type INTEGER)
LANGUAGE plpgsql AS $$
BEGIN
    SELECT insert_in_accounts(id_customer, account_type);
    COMMIT;
END; $$;

-------------------------------------------------
-- Deposit operation
-------------------------------------------------
CREATE OR REPLACE FUNCTION insert_in_deposits(id_account INTEGER, amount INTEGER)
RETURNS INTEGER
LANGUAGE plpgsql AS $$
DECLARE
  new_deposit_id INTEGER;
BEGIN
    UPDATE accounts AS a
    SET a.balance = a.balance + amount
    WHERE a.id = id_account;

    IF NOT FOUND THEN
        RAISE EXCEPTION 'Account % not found', id_account;
    END IF;

    INSERT INTO deposits(id_account, amount)
    VALUES (id_account, amount)
    RETURNING id INTO new_deposit_id;
    
    RETURN new_deposit_id;
EXCEPTION
    WHEN check_violation THEN
        IF SQLERRM LIKE '%deposits_amount_positive%' THEN
            RAISE EXCEPTION 'Deposit amount must be greater than 0';
	ELSE
	    RAISE;
	END IF;
    WHEN OTHERS THEN
        RAISE;
END; $$;

CREATE OR REPLACE PROCEDURE deposit(id_account INTEGER, amount INTEGER)
LANGUAGE plpgsql AS $$
BEGIN
    SELECT insert_in_deposits(id_account, amount);
    COMMIT;
END; $$;

-------------------------------------------------
-- Withdrawal operation
-------------------------------------------------
CREATE OR REPLACE FUNCTION insert_in_withdrawals(id_account INTEGER, amount INTEGER)
RETURNS INTEGER
LANGUAGE plpgsql AS $$
DECLARE
  new_withdrawal_id INTEGER;
BEGIN
    UPDATE accounts AS a
    SET a.balance = a.balance - amount
    WHERE a.id = id_account;

    IF NOT FOUND THEN
        RAISE EXCEPTION 'Account % not found', id_account;
    END IF;

    INSERT INTO withdrawals(id_account, amount)
    VALUES (id_account, amount)
    RETURNING id INTO new_withdrawal_id;

    RETURN new_withdrawal_id;
EXCEPTION
    WHEN check_violation THEN
        IF SQLERRM LIKE '%withdrawals_amount_positive%' THEN
	    RAISE 'Withdraw amount must be greater than 0';
	ELSIF SQLERRM LIKE '%accounts_balance_positive%' THEN
	    RAISE 'Account % has insufficient funds!', id_account;
	ELSE
	    RAISE;
	END IF;
    WHEN OTHERS THEN
        RAISE;
END; $$;

CREATE OR REPLACE PROCEDURE withdraw(id_account INTEGER, amount INTEGER)
LANGUAGE plpgsql AS $$
BEGIN
    SELECT insert_in_withdrawals(id_account, amount);
    COMMIT;
END; $$;

-------------------------------------------------
-- Transfer operation
-------------------------------------------------
CREATE OR REPLACE FUNCTION insert_in_transfers(id_account_from INTEGER, id_account_to INTEGER, amount INTEGER)
RETURNS INTEGER
LANGUAGE plpgsql AS $$
DECLARE
  new_transfer_id INTEGER;
BEGIN
    UPDATE accounts AS a
    SET a.balance = a.balance - amount
    WHERE a.id = id_account_from;

    IF NOT FOUND THEN
        RAISE EXCEPTION 'Account % not found', id_account_from;
    END IF;

    UPDATE accounts AS a
    SET a.balance = a.balance + amount
    WHERE a.id = id_account_to;

    IF NOT FOUND THEN
        RAISE EXCEPTION 'Account % not found', id_account_to;
    END IF;

    INSERT INTO transfer(id_account_from, id_account_to, amount)
    VALUES (id_account_from, id_account_to, amount)
    RETURNING id INTO new_transfer_id;

    RETURN new_transfer_id;
EXCEPTION
    WHEN check_violation THEN
        IF SQLERRM LIKE '%transfers_amount_positive%' THEN
	    RAISE 'Transfer amount must be greater than 0';
	ELSIF SQLERRM LIKE '%accounts_balance_positive%' THEN
	    RAISE 'Account % has insufficient funds!', id_account_from;
	ELSE
	    RAISE;
	END IF;
    WHEN OTHERS THEN
        RAISE;
END; $$;

CREATE OR REPLACE PROCEDURE transfer(id_account_from INTEGER, id_account_to INTEGER, amount INTEGER)
LANGUAGE plpgsql AS $$
BEGIN
    SELECT insert_in_transfers(id_account_from, id_account_to, amount);
    COMMIT;
END; $$;
    
COMMIT;
