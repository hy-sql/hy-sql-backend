class SQLError extends Error {
    constructor(args) {
        super(args)
        this.type = 'SQLError'
    }
}

module.exports = SQLError
