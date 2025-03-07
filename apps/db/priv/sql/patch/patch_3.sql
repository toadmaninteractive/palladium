-- Patch SQL
-- Revision: 2 -> 3

-- Add rev, title, dbs, created_at and updated_at fields to projects table
ALTER TABLE projects ADD COLUMN rev integer NOT NULL DEFAULT 1;
ALTER TABLE projects ADD COLUMN title varchar NOT NULL CHECK (TRIM(title) <> '');
ALTER TABLE projects ADD COLUMN dbs jsonb NOT NULL DEFAULT '[]';
ALTER TABLE projects ADD COLUMN created_at timestamptz NOT NULL DEFAULT current_timestamp;
ALTER TABLE projects ADD COLUMN updated_at timestamptz NOT NULL DEFAULT current_timestamp;

-- Create related indexes
CREATE UNIQUE INDEX proj_title_ult_index ON projects (LOWER(TRIM(title)));
CREATE INDEX proj_dbs_gin_index ON projects USING gin (dbs);
CREATE INDEX proj_created_at_index ON projects (created_at);
CREATE INDEX proj_updated_at_index ON projects (updated_at);
