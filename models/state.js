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
        console.log(command)
        return 'TODO'
    }

    selectAllFromTable(command) {
        console.log(command)
        return 'TODO'
    }
}

module.exports = State
