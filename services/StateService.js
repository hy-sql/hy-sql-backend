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
                return this.selectColumnsFromTable(parsedCommand)
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

        const table = this.findTable(command.tableName)
        const tableIndex = this.findTableIndex(command.tableName)

        const newRow = {
            id: table.rows.length + 1,
        }

        for (let i = 0; i < command.columns.length; i++) {
            const columnName = command.columns[i].name
            const value = command.values[i]

            const columnIndex = table.columns.findIndex(
                (e) => e.name === columnName
            )
            const columnType = table.columns[columnIndex].type

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

        const table = this.findTable(command.tableName)

        let rows = table.rows

        if (command.where) {
            const filter = this.createFilter(command.where)
            rows = _.filter(rows, filter)

            if (command.orderBy) {
                rows =
                    command.orderBy.order &&
                    command.orderBy.order.toUpperCase() === 'DESC'
                        ? _.orderBy(rows, ['hinta'], ['desc'])
                        : _.orderBy(rows, ['hinta'], ['asc'])
            }
        } else if (command.orderBy) {
            rows =
                command.orderBy.order &&
                command.orderBy.order.toUpperCase() === 'DESC'
                    ? _.orderBy(rows, ['hinta'], ['desc'])
                    : _.orderBy(rows, ['hinta'], ['asc'])
        }

        const result = command.where
            ? `SELECT * FROM ${command.tableName} WHERE ${command.where.columnName}${command.where.sign}${command.where.value} -query executed succesfully`
            : `SELECT * FROM ${command.tableName} -query was executed succesfully`

        return {
            result,
            rows,
        }
    }

    createFilter(whereObject) {
        const column = whereObject.columnName
        const value = whereObject.value
        const sign = whereObject.sign

        switch (sign) {
            case '>':
                return (item) => {
                    return item[column] > value
                }
            case '<':
                return (item) => {
                    return item[column] < value
                }
            case '>=':
                return (item) => {
                    return item[column] >= value
                }
            case '<=':
                return (item) => {
                    return item[column] <= value
                }
            default:
                return { [column]: value }
        }
    }

    /*This is for SELECT column_1, column_2 FROM -queries*/
    selectColumnsFromTable(command) {
        const error = this.checkIfTableExists(command.tableName)
        if (error) return { error: error }

        const table = this.findTable(command.tableName)
        let rows = table.rows

        console.log(command.orderBy)

        if (command.where) {
            const filter = this.createFilter(command.where)
            rows = _.filter(rows, filter)

            if (command.orderBy) {
                rows =
                    command.orderBy.order &&
                    command.orderBy.order.toUpperCase() === 'DESC'
                        ? _.orderBy(
                              rows,
                              [command.orderBy.columnName],
                              ['desc']
                          )
                        : _.orderBy(rows, [command.orderBy.columnName], ['asc'])
            }
        } else if (command.orderBy) {
            rows =
                command.orderBy.order &&
                command.orderBy.order.toUpperCase() === 'DESC'
                    ? _.orderBy(rows, [command.orderBy.columnName], ['desc'])
                    : _.orderBy(rows, [command.orderBy.columnName], ['asc'])
        }

        let rowsToReturn = []
        rows.forEach((row) => {
            rowsToReturn.push(this.pickColumnsFromRow(command.columns, row))
        })

        const columnsStr = command.columns.map((e) => e.name).join(', ')
        const result = command.where
            ? `${command.name} ${columnsStr} FROM ${command.tableName} WHERE ${command.where.columnName}${command.where.sign}${command.where.value} -query executed succesfully`
            : `${command.name} ${columnsStr} FROM ${command.tableName} -query was executed successfully`
        return {
            result,
            rows: rowsToReturn,
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

    checkIfTableExists(tableName) {
        const tableIndex = this.state.tables.findIndex(
            (e) => e.name === tableName
        )
        if (tableIndex === -1) return `No such table ${tableName}`
    }

    findDuplicates(arr) {
        return arr.filter((item, index) => arr.indexOf(item) !== index)
    }

    findTable(tableName) {
        const tableIndex = this.state.tables.findIndex(
            (table) => table.name === tableName
        )
        return this.state.tables[tableIndex]
    }

    findTableIndex(tableName) {
        return this.state.tables.findIndex(
            (element) => element.name === tableName
        )
    }

    pickColumnsFromRow(columns, row) {
        let filteredRow = {}
        columns.forEach((column) => {
            filteredRow[column.name] = row[column.name]
        })
        return filteredRow
    }
}

module.exports = StateService
