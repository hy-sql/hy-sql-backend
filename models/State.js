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
        let error
        const newTable = {
            name: command.tableName,
            columns: command.columns,
            rows: [],
        }
        this.tablelist.push(newTable)
        if (!error) {
            return {
                result: `Table ${newTable.name} created successfully`,
            }
        } else {
            return {
                error: error,
            }
        }
    }

    insertIntoTable(command) {
        let error
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
        if (!error) {
            return {
                result: `INSERT INTO ${command.tableName} -query was executed succesfully`,
            }
        } else {
            return { error: error }
        }
    }

    selectAllFromTable(command) {
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
