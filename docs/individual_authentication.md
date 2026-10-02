# Individual authentication configuration

Phase 1B supports individual adjudication accounts without committing names,
email addresses or credentials to the public repository.

## Deployment variables

### `LEM_INITIAL_USERS_JSON`

A JSON array of user records:

```json
[
  {
    "user_id": "usr-example-admin",
    "email": "admin@example.org",
    "display_name": "Example Admin",
    "role": "administrator",
    "active": true
  }
]
```

For this project, configure three users in the deployment environment:

- Neal Haddaway — administrator
- Sini Savilaakso — reviewer
- Matthew Grainger — reviewer

Their actual email addresses should remain in deployment configuration rather
than in the public repository.

### `LEM_USER_ACCESS_KEY_HASHES_JSON`

A JSON object keyed by `user_id`. Each value is the SHA-256 hash of that
user's high-entropy personal access key:

```json
{
  "usr-example-admin": "<64-character SHA-256 hash>"
}
```

Access keys themselves must not be committed to GitHub or stored in Google
Sheets.

## Transitional behaviour

If individual-user configuration is present, Shiny requires email + personal
access key and stores the authenticated `user_id` with adjudication actions.

If individual-user configuration is not present, the existing shared-key login
continues to work temporarily. This avoids locking out the working deployment
before the three real accounts have been configured.

The legacy shared-key fallback should be removed only after individual login has
been configured and tested successfully in the deployed app.
