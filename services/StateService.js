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
        const error = this.checkIfTableExists(command)
        if (error) return { error: error }

        const tableIndex = this.state.tables.findIndex(
            (element) => element.name === command.tableName
        )

        const newRow = {
            id: this.state.tables[tableIndex].rows.length + 1,
        }

        for (let i = 0; i < command.columns.length; i++) {
            const column = command.columns[i].name
            const value = command.values[i].value
            newRow[column] = value
        }

        this.state.insertIntoTable(tableIndex, newRow)

        return {
            result: `INSERT INTO ${command.tableName} -query was executed succesfully`,
        }
    }

    selectAllFromTable(command) {
        const error = this.checkIfTableExists(command)
        if (error) return { error: error }

        const tableIndex = this.state.tables.findIndex(
            (table) => table.name === command.tableName
        )

        const rows = { rows: this.state.tables[tableIndex].rows }
        console.log(rows)

        return {
            result: `SELECT * FROM ${command.tableName} -query was executed succesfully`,
            rows,
        }
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

    checkIfTableExists(command) {
        const tableIndex = this.state.tables.findIndex(
            (e) => e.name === command.tableName
        )
        if (tableIndex === -1) return `No such table ${command.tableName}`
    }

    findDuplicates(arr) {
        return arr.filter((item, index) => arr.indexOf(item) !== index)
    }
}

module.exports = StateService