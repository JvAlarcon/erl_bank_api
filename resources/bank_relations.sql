SELECT 'CREATE DATABASE erl_bank'
WHERE NOT EXISTS (SELECT FROM pg_database WHERE datname = 'erl_bank')\gexec;

\c erl_bank;

BEGIN;
CREATE TABLE customers (
    id SERIAL,
    cpf VARCHAR(14) NOT NULL UNIQUE,
    name VARCHAR(60) NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY(id, cpf, created_at)
);

CREATE account_types (
   id SERIAL PRIMARY KEY,
   description TEXT NOT NULL UNIQUE
);

CREATE TABLE accounts (
    id SERIAL,
    type INTERGER NOT NULL REFERENCES account_types(id) ON DELETE CASCADE,
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
    PRIMARY KEY(id_account_origin, id_balance_operation, id_account_target, created_at)
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

-- Utilities functions to be handled in a transaction
CREATE FUNCTION deposit(id_account int, amount numeric(10,2))
DECLARE
  balance_operation int;
RETURNS VOID AS $$
    BEGIN
        balance_operation := SELECT id FROM balance_operations WHERE description = 'deposit';
      
	IF amount <= 0 THEN
           RAISE EXCEPTION 'Deposit amount must be greater than 0';
      	END IF;
      
	UPDATE accounts AS a
      	SET a.balance = a.balance + amount
      	WHERE a.id = id_account;

      	IF NOT FOUND THEN
      	   RAISE EXCEPTION 'Account % not found', id_account;
      	END IF;

      	INSERT INTO statement_history(id_account_origin, id_account_target, id_balance_operation, executed_at)
      	VALUES (id_account, id_account, balance_operation, now());
    END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION withdraw(id_account int, amount numeric(10,2))
DECLARE
  balance_operation int;
  current_balance numeric(10,2);
RETURNS VOID AS $$
    BEGIN
	balance_operation := SELECT id FROM balance_operations WHERE description = 'withdraw';

	IF amount <= 0 THEN
	   RAISE EXCEPTION 'Withdraw amount must be greater than 0';
	END IF;

	current_balance := SELECT balance INTO current_balance FROM accounts WHERE id = id_account;

	UPDATE accounts AS a
	SET a.balance = current_balance - amount
	WHERE a.id = id_account;

	INSERT INTO statement_history(id_account_origin, id_account_target, id_balance_operation, executed_at)
      	VALUES (id_account, id_account, balance_operation, now());
    EXCEPTION
        WHEN accounts_balance_positive THEN
	     RAISE EXCEPTION 'Insufficient funds!';
        WHEN OTHERS THEN
	     RAISE;
    END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION transfer(id_accoun_origin int, id_account_target int, amount numeric (10,2))
DECLARE
  balance_operation int;
RETURNS VOID AS $$
    BEGIN
	balance_operation := SELECT id FROM balance_operations WHERE description = 'transfer';

	-- Tira de uma conta
	-- Deposita em outra
	-- Efetivamente, pode chamar o withdraw e depois o deposit, mas sem a parte da operação
	-- Talvez seja melhor criar duas funções auxiliares para serem usadas aqui
    END;
