class State {
    constructor(tables) {
        this.tablelist = tables
    }

    get tables() {
        return this.tablelist
    }

    createTable(newTable) {
        this.tablelist.push(newTable)
    }

    insertIntoTable(tableIndex, newRow) {
        this.tablelist[tableIndex].rows.push(newRow)
    }
}

module.exports = State
