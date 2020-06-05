class State {
    constructor(tables) {
        this.tablelist = tables
    }

    get tables() {
        return this.tablelist
    }

    /**
     * Inserts new table to tablelist.
     * @param {*} newTable Table object
     */
    createTable(newTable) {
        this.tablelist.push(newTable)
    }

    /*
     * Inserts new row in given table.
     */
    insertIntoTable(tableIndex, newRow) {
        this.tablelist[tableIndex].rows.push(newRow)
    }

    /*
     * Updates rows in given table. Input is table's
     * index and object that contains new rows for the table.
     */
    updateRows(tableIndex, newRows) {
        this.tablelist[tableIndex].rows = newRows
    }

    /*
     * Deletes rows in given table by replacing the table rows with the rows remaining
     * after deletion. Input is table's
     * index and object that contains new rows for the table.
     */
    deleteFromTable(tableIndex, remainingRows) {
        this.tablelist[tableIndex].rows = remainingRows
    }
}

module.exports = State
