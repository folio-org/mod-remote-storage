CREATE TABLE IF NOT EXISTS item_notes
(
    item_id             UUID,
    note_type           VARCHAR(256),
    note                TEXT,
    staff_only          BOOLEAN
);
