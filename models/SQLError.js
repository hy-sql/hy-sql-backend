/**
 * Custom error class to distinguish custom SQL error from others
 */
class SQLError extends Error {
    constructor(args) {
        super(args)
        this.name = 'SQLError'
    }
}

module.exports = SQLError
