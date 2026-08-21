BEGIN;

CREATE TABLE merchants (
  id BIGSERIAL PRIMARY KEY,
  session_id BIGINT NOT NULL REFERENCES game_sessions(id) ON DELETE CASCADE,
  name TEXT NOT NULL,
  wealth_class TEXT NOT NULL CHECK (wealth_class IN ('poor','moderate','rich')),
  specialty TEXT NOT NULL CHECK (specialty IN ('general','food','weapons','armour','hunter')),
  temperament TEXT NOT NULL CHECK (temperament IN ('hard','fair','generous')),
  gold NUMERIC NOT NULL DEFAULT 0 CHECK (gold >= 0),
  status TEXT NOT NULL DEFAULT 'open' CHECK (status IN ('open','closed')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE merchant_stock (
  id BIGSERIAL PRIMARY KEY,
  merchant_id BIGINT NOT NULL REFERENCES merchants(id) ON DELETE CASCADE,
  catalogue_id TEXT,
  item_json JSONB NOT NULL,
  quantity INTEGER NOT NULL DEFAULT 1 CHECK (quantity >= 0),
  base_value NUMERIC NOT NULL DEFAULT 0 CHECK (base_value >= 0),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE merchant_invitations (
  merchant_id BIGINT NOT NULL REFERENCES merchants(id) ON DELETE CASCADE,
  character_id TEXT NOT NULL,
  status TEXT NOT NULL DEFAULT 'pending' CHECK (status IN ('pending','opened','dismissed')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (merchant_id,character_id)
);

CREATE TABLE merchant_transactions (
  id BIGSERIAL PRIMARY KEY,
  merchant_id BIGINT NOT NULL REFERENCES merchants(id) ON DELETE CASCADE,
  character_id TEXT NOT NULL,
  direction TEXT NOT NULL CHECK (direction IN ('buy','sell')),
  item_name TEXT NOT NULL,
  quantity INTEGER NOT NULL DEFAULT 1 CHECK (quantity > 0),
  base_value NUMERIC NOT NULL,
  final_price NUMERIC NOT NULL CHECK (final_price >= 0),
  haggle_roll INTEGER NOT NULL,
  haggle_dc INTEGER NOT NULL,
  haggle_success BOOLEAN NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX merchant_session_status_idx ON merchants(session_id,status,created_at DESC);
CREATE INDEX merchant_invitation_character_idx ON merchant_invitations(character_id,status,created_at DESC);
CREATE INDEX merchant_stock_merchant_idx ON merchant_stock(merchant_id,quantity);

COMMIT;
