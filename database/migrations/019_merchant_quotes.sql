BEGIN;

ALTER TABLE merchant_invitations
  ADD COLUMN IF NOT EXISTS haggle_rejections INTEGER NOT NULL DEFAULT 0 CHECK (haggle_rejections >= 0);

CREATE TABLE merchant_quotes (
  id BIGSERIAL PRIMARY KEY,
  merchant_id BIGINT NOT NULL REFERENCES merchants(id) ON DELETE CASCADE,
  character_id TEXT NOT NULL,
  direction TEXT NOT NULL CHECK (direction IN ('buy','sell')),
  stock_id BIGINT REFERENCES merchant_stock(id) ON DELETE SET NULL,
  player_item_id TEXT,
  item_name TEXT NOT NULL,
  base_value NUMERIC NOT NULL,
  final_price NUMERIC NOT NULL CHECK (final_price >= 0),
  haggle_roll INTEGER NOT NULL,
  haggle_dc INTEGER NOT NULL,
  haggle_success BOOLEAN NOT NULL,
  status TEXT NOT NULL DEFAULT 'pending' CHECK (status IN ('pending','accepted','rejected','expired')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  resolved_at TIMESTAMPTZ
);

CREATE INDEX merchant_quotes_pending_idx ON merchant_quotes(character_id,status,created_at DESC);

COMMIT;
