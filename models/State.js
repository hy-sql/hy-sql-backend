const _ = require('lodash')

class State {
    /**
     * Constructor for state. Sets the given Map as value of tables.
     * @param {Map} tables
     */
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
     * Inserts new table to the map of tables.
     * @param {object} newTable Table object
     */
    createTable(newTable) {
        this._tables.set(newTable.name, newTable)
    }

    /**
     * Retrieves a table object by name from the map of tables.
     * @param {String} tableName name of the wanted table
     */
    getTableByName(tableName) {
        return _.cloneDeep(this._tables.get(tableName))
    }

    /**
     * Checks whether a table with given name exists in the map of tables.
     * @param {String} tableName name to search for
     */
    tableExists(tableName) {
        return this._tables.has(tableName)
    }

    /**
     * Adds the given row object to existing rows in the table that has
     * the given name in the map of tables.
     * @param {String} tableName the name of the table to add to
     * @param {object} newRow the row object to be added
     */
    insertIntoTable(tableName, newRow) {
        const table = this.getTableByName(tableName)
        const rows = table.rows

        const newRows = rows.concat(newRow)
        const newTable = { ...table, rows: newRows }

        this._tables.set(newTable.name, newTable)
    }

    /**
     * Replaces the rows with the given rows in the table that has the given name.
     * @param {*} tableName
     * @param {*} newRows
     */
    replaceRows(tableName, newRows) {
        const table = this.getTableByName(tableName)
        const newTable = { ...table, rows: newRows }

        this._tables.set(newTable.name, newTable)
    }
}

module.exports = State
