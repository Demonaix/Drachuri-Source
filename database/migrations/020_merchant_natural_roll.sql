BEGIN;

ALTER TABLE merchant_quotes
  ADD COLUMN IF NOT EXISTS natural_roll INTEGER CHECK (natural_roll BETWEEN 1 AND 20);

ALTER TABLE merchant_transactions
  ADD COLUMN IF NOT EXISTS natural_roll INTEGER CHECK (natural_roll BETWEEN 1 AND 20);

COMMIT;
