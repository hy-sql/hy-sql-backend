class State {
    constructor(tables) {
        this._tables = tables
    }

    set tables(tables) {
        this._tables = tables
    }

    get tables() {
        return this._tables
    }

    /**
     * Inserts new table to tablelist.
     * @param {*} newTable Table object
     */
    createTable(newTable) {
        this._tables.set(newTable.name, newTable)
    }

    getTableByName(tableName) {
        return this._tables.get(tableName)
    }

    tableExists(tableName) {
        return this._tables.has(tableName)
    }

    /*
     * Inserts new row in given table.
     */
    insertIntoTable(tableName, newRow) {
        this._tables.get(tableName).rows.push(newRow)
    }

    /*
     * Updates rows in given table. Input is table's
     * index and object that contains new rows for the table.
     */
    updateRows(tableName, newRows) {
        this._tables.get(tableName).rows = newRows
    }

    /*
     * Deletes rows in given table by replacing the table rows with the rows remaining
     * after deletion. Input is table's
     * index and object that contains new rows for the table.
     */
    deleteFromTable(tableName, remainingRows) {
        this._tables.get(tableName).rows = remainingRows
    }
}

module.exports = State
