The apostrophe in "Don't" opens up a string even though it's inside a SQL line comment.

```
-- Don't
SELECT * from users WHERE id = 3;
-- Don't
```

See bug http://code.google.com/p/support/issues/detail?id=44 and project http://code.google.com/p/google-code-prettify/.