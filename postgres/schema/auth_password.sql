CREATE TABLE IF NOT EXISTS auth_password 
( user_id INTEGER PRIMARY KEY, 
  hashed_password VARCHAR(60) NOT NULL, 
  created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP)
