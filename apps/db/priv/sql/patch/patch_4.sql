-- Patch SQL
-- Revision: 3 -> 4

-- Add name field to personnel table
ALTER TABLE personnel ADD COLUMN name varchar DEFAULT NULL;
