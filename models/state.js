class State {
    constructor(tables) {
        this.tablelist = tables
    }

    get tables() {
        return this.tablelist
    }

    createTable(command) {
        if (command.name !== 'CREATE TABLE') {
            return 'Wrong command type'
        }
        const newObject = {
            name: command.tableName,
            columns: command.columns,
            rows: [],
        }
        this.tablelist.push(newObject)
    }

    insertIntoTable(command) {
        if (command.name !== 'INSERT INTO') {
            return 'Wrong command type'
        }
        const tableIndex = this.tablelist.findIndex(
            (element) => element.name === command.tableName
        )
        let newtablelist = [...this.tablelist]
        //TODO update wanted columns
        const newRowObject = {
            id: newtablelist[tableIndex].rows.length + 1,
        }
        newtablelist[tableIndex].rows.push(newRowObject)
        this.tablelist = newtablelist
    }

    selectAllFromTable(command) {
        if (command.name !== 'SELECT *') {
            return 'Wrong command type'
        }
        const tableIndex = this.tablelist.findIndex(
            (element) => element.name === command.tableName
        )
        return this.tablelist[tableIndex].rows
    }
}

module.exports = State
