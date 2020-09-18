-- requires: create-tags
-- requires: create-posts

BEGIN;

CREATE TABLE IF NOT EXISTS post_tags (
  post_id INT REFERENCES posts(id) ON DELETE CASCADE,
  tag_name TEXT REFERENCES tags(name) ON DELETE CASCADE,
  UNIQUE (post_id, tag_name)
);

COMMIT;
