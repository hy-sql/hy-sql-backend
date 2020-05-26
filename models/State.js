class State {
    constructor(tables) {
        this.tablelist = tables
    }

    get tables() {
        return this.tablelist
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
        this.tablelist.push(newTable)

        return { result: `Table ${newTable.name} created successfully` }
    }

    checkCreateTableErrors(command) {
        const tableIndex = this.tablelist.findIndex(
            (e) => e.name === command.tableName
        )
        console.log(`TABLE INDEX ${tableIndex}`)
        if (tableIndex !== -1)
            return `Table ${command.tableName} already exists`
    }

    insertIntoTable(command) {
        const error = this.checkIfTableExists(command)
        if (error) return { error: error }

        const tableIndex = this.tablelist.findIndex(
            (element) => element.name === command.tableName
        )
        let newtablelist = [...this.tablelist]
        const newRow = {
            id: newtablelist[tableIndex].rows.length + 1,
        }
        for (let i = 0; i < command.columns.length; i++) {
            const column = command.columns[i].name
            const value = command.values[i].value
            newRow[column] = value
        }
        newtablelist[tableIndex].rows.push(newRow)
        this.tablelist = newtablelist
        return {
            result: `INSERT INTO ${command.tableName} -query was executed succesfully`,
        }
    }

    checkIfTableExists(command) {
        const tableIndex = this.tablelist.findIndex(
            (e) => e.name === command.tableName
        )
        if (tableIndex === -1) return `No such table ${command.tableName}`
    }

    selectAllFromTable(command) {
        const error = this.checkIfTableExists(command)
        if (error) return { error: error }

        const tableIndex = this.tablelist.findIndex(
            (table) => table.name === command.tableName
        )
        return {
            result: `SELECT * FROM ${command.tableName} -query was executed succesfully`,
            rows: this.tablelist[tableIndex].rows,
        }
    }
}

module.exports = State
