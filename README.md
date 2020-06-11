# HY-SQL-backend

[![CircleCI](https://circleci.com/gh/hy-sql/hy-sql-backend.svg?style=svg)](https://circleci.com/gh/hy-sql/hy-sql-backend)

[![codecov](https://codecov.io/gh/hy-sql/hy-sql-backend/branch/master/graph/badge.svg)](https://codecov.io/gh/hy-sql/hy-sql-backend)

https://hy-sql.herokuapp.com/

Backend offers one stateless API-endpoint for parsing and executing SQL-queries.

[API-documentation](https://github.com/hy-sql/project-info/blob/master/documents/rest-api.md)

## Development

Install dependencies: `npm install`

Run app in development mode: `npm run dev`. This launches server with nodemon to `localhost:3001`.

Run tests: `npm run test`

Run ESlint: `npm run lint`

Examples of API usage are found at `_requests/`. HTTP POST -requests support Visual Studio REST Client.
