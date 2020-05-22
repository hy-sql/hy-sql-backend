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
                this.createTable(parsedCommand)
                break
            case 'INSERT INTO':
                this.insertIntoTable(parsedCommand)
                break
            case 'SELECT *':
                this.selectAllFromTable(parsedCommand)
                break
            default:
                console.log('sth went wrong')
                break
        }
    }

    createTable(command) {
        const newTable = {
            name: command.tableName,
            columns: command.columns,
            rows: [],
        }
        this.tablelist.push(newTable)
    }

    insertIntoTable(command) {
        const tableIndex = this.tablelist.findIndex(
            (element) => element.name === command.tableName
        )
        let newtablelist = [...this.tablelist]
        const newRow = {
            id: newtablelist[tableIndex].rows.length + 1,
        }
        for (let i = 0; i < command.columns.length; i++) {
            const column = command.columns[i]
            const value = command.values[i]
            newRow[column] = value
        }
        newtablelist[tableIndex].rows.push(newRow)
        this.tablelist = newtablelist
    }

    selectAllFromTable(command) {
        const tableIndex = this.tablelist.findIndex(
            (table) => table.name === command.tableName
        )
        return this.tablelist[tableIndex].rows
    }
}

module.exports = State
