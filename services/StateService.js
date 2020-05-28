const _ = require('lodash')

class StateService {
    constructor(state) {
        this.state = state
    }

    updateState(parsedCommand) {
        switch (parsedCommand.name) {
            case 'CREATE TABLE':
                return this.createTable(parsedCommand)
            case 'INSERT INTO':
                return this.insertIntoTable(parsedCommand)
            case 'SELECT *':
                return this.selectAllFromTable(parsedCommand)
            case 'SELECT':
                return this.selectFromTable(parsedCommand)
            default:
                break
        }
    }

    createTable(command) {
        let error = this.checkCreateTableErrors(command)
        if (error) return { error: error }

        const newTable = {
            name: command.tableName,
            columns: command.columns,
            rows: [],
        }

        this.state.createTable(newTable)

        return { result: `Table ${newTable.name} created successfully` }
    }

    insertIntoTable(command) {
        const error = this.checkIfTableExists(command.tableName)
        if (error) return { error: error }

        const tableIndex = this.state.tables.findIndex(
            (element) => element.name === command.tableName
        )

        const newRow = {
            id: this.state.tables[tableIndex].rows.length + 1,
        }

        for (let i = 0; i < command.columns.length; i++) {
            const columnName = command.columns[i].name
            const value = command.values[i]

            const columnIndex = this.state.tables[tableIndex].columns.findIndex(
                (e) => e.name === columnName
            )
            const columnType = this.state.tables[tableIndex].columns[
                columnIndex
            ].type

            if (columnType !== value.type)
                return {
                    error: `Wrong datatype: expected ${columnType} but was ${value.type}`,
                }

            newRow[columnName] = value.value
        }

        this.state.insertIntoTable(tableIndex, newRow)

        return {
            result: `INSERT INTO ${command.tableName} -query was executed succesfully`,
        }
    }

    selectAllFromTable(command) {
        const error = this.checkIfTableExists(command.tableName)
        if (error) return { error: error }

        const tableIndex = this.state.tables.findIndex(
            (table) => table.name === command.tableName
        )

        let rows = this.state.tables[tableIndex].rows

        if (command.orderBy) {
            rows =
                command.orderBy.order &&
                command.orderBy.order.toUpperCase() === 'DESC'
                    ? _.orderBy(rows, ['hinta'], ['desc'])
                    : _.orderBy(rows, ['hinta'], ['asc'])
        }

        return {
            result: `SELECT * FROM ${command.tableName} -query was executed succesfully`,
            rows,
        }
    }

    selectFromTable(command) {
        const error = this.checkIfTableExists(command.tableName)
        if (error) return { error: error }

        return `${command.name} FROM ${command.tableName} -query was executed successfully`
    }

    checkCreateTableErrors(command) {
        const tableIndex = this.state.tables.findIndex(
            (e) => e.name === command.tableName
        )

        if (tableIndex !== -1)
            return `Table ${command.tableName} already exists`

        const nameArray = command.columns.map((e) => e.name)

        const duplicates = this.findDuplicates(nameArray)

        if (duplicates.length !== 0)
            return duplicates.map((e) => `duplicate column ${e}: ${e}`)
    }

    checkIfTableExists(tableName) {
        const tableIndex = this.state.tables.findIndex(
            (e) => e.name === tableName
        )
        if (tableIndex === -1) return `No such table ${tableName}`
    }

    findDuplicates(arr) {
        return arr.filter((item, index) => arr.indexOf(item) !== index)
    }
}

module.exports = StateService
