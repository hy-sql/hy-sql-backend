class SQLError extends Error {
    constructor(args) {
        super(args)
        this.name = 'SQLError'
    }
}

module.exports = SQLError
