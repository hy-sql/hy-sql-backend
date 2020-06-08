const _ = require('lodash')
const { calculateExpression } = require('../utils/calculateExpression')
const {
    parseColumnsFromExpression,
} = require('../utils/parseColumnsFromExpression')
const { parseColumnFromFunction } = require('../utils/parseColumnFromFunction')
const {
    executeStringFunction,
    executeAggregateFunction,
} = require('../utils/executeFunction')

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
            case 'SELECT ADVANCED':
                return this.selectAdvanced(parsedCommand)
            case 'UPDATE':
                return this.updateTable(parsedCommand)
            case 'DELETE':
                return this.deleteFromTable(parsedCommand)
            default:
                break
        }
    }

    /*
     *For executing UPDATE ... or UPDATE ... WHERE; queries
     *Input is parsed command object and output either result or error object
     */
    updateTable(command) {
        let error = this.checkIfTableExists(command.tableName)
        if (error) return { error: error }

        const table = this.findTable(command.tableName)

        /* Check if trying to update wrong type of data to column */
        command.columns.forEach((pair) => {
            const column = _.find(table.columns, { name: pair.columnName })
            if (column.type !== pair.valueType)
                error = `Wrong datatype: expected ${column.type} but was ${pair.valueType}`
        })
        if (error) return { error: error }

        let newRows = []
        let rowsToUpdate = table.rows

        /*
         * If command object has property 'where' it's filtered in order
         * to update only the rows that are spesified in query
         */
        if (command.where) {
            const filter = this.createFilter(command.where)
            rowsToUpdate = _.filter(rowsToUpdate, filter)
            let notChangedRows = _.difference(table.rows, rowsToUpdate)
            notChangedRows.forEach((row) => newRows.push(row))
        }

        rowsToUpdate.forEach((row) => {
            newRows.push(this.updateRow(row, command.columns, table.columns))
        })

        const tableIndex = this.findTableIndex(command.tableName)

        newRows = _.sortBy(newRows, 'id')
        this.state.updateRows(tableIndex, newRows)

        const result = `Rows in table ${command.tableName} updated`

        return { result }
    }

    /**
     * Help function for updateTable().
     * Updates wanted columns in given row.
     * @param {*} row Row object
     * @param {*} columnsToUpdate Object that contains columnName and value which will be updated
     */
    updateRow(row, columnsToUpdate) {
        for (let i = 0; i < columnsToUpdate.length; i++) {
            const columnName = columnsToUpdate[i].columnName
            const value = columnsToUpdate[i].value
            row[columnName] = value
        }
        return row
    }

    /*
     *For executing DELETE FROM Table_name; or DELETE FROM Table_name WHERE...; queries
     *Expected input is a parsed DELETE-command object. Output is an object either containing
     the key result or error and respectively the value result string or error string respectively.
     */
    deleteFromTable(command) {
        const error = this.checkIfTableExists(command.tableName)
        if (error) return { error: error }

        const table = this.findTable(command.tableName)
        const tableIndex = this.findTableIndex(command.tableName)
        let rows = table.rows

        if (command.where) {
            const filter = this.createOppositeFilter(command.where)
            rows = _.filter(rows, filter)
        } else {
            rows = []
        }

        this.state.deleteFromTable(tableIndex, rows)

        const result = command.where
            ? `Requested rows from table ${command.tableName} deleted succesfully`
            : `All rows from table ${command.tableName} deleted succesfully`

        return {
            result,
        }
    }

    selectAdvanced(command) {
        const error = this.checkIfTableExists(command.tableName)
        if (error) return { error: error }

        const table = this.findTable(command.tableName)
        let existingRows = table.rows

        const rows = this.createAdvancedRows(command, existingRows)

        const result = `SELECT ${command.fields
            .map((c) => c.value)
            .join(', ')} FROM ${command.tableName} -query executed succesfully`

        return {
            result,
            rows,
        }
    }

    createAdvancedRows(command, existingRows) {
        if (command.fields[0].type === 'aggregateFunction') {
            return this.createAggregateFunctionRow(
                command.fields[0],
                existingRows
            )
        }

        return this.createFunctionRow(command, existingRows)
    }

    createFunctionRow(command, existingRows) {
        return existingRows.reduce((rowsToReturn, row) => {
            const newRow = {}

            command.fields.forEach((field) => {
                if (field.type === 'column') {
                    newRow[field.value] = row[field.value]
                } else if (field.type === 'expression') {
                    const context = {}
                    parseColumnsFromExpression(field.value).map((column) => {
                        context[column] = row[column]
                    })
                    newRow[field.value] = calculateExpression(
                        field.value,
                        context
                    )
                } else if (field.type === 'stringFunction') {
                    const columnToOperateOn = parseColumnFromFunction(field)
                    const columnValue = row[columnToOperateOn]
                    newRow[field.value] = executeStringFunction(
                        field,
                        columnValue
                    )
                }
            })
            rowsToReturn.push(newRow)

            return rowsToReturn
        }, [])
    }

    createAggregateFunctionRow(functionField, existingRows) {
        return [
            {
                [functionField.value]: executeAggregateFunction(
                    functionField,
                    existingRows
                ),
            },
        ]
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
            case '<>':
                return (item) => {
                    // eslint-disable-next-line
                    return item[column] != value
                }
            default:
                return { [column]: value }
        }
    }

    /*Creates filter that gives the opposite results than filter created with createFilter()*/
    createOppositeFilter(whereObject) {
        const column = whereObject.columnName
        const value = whereObject.value
        const sign = whereObject.sign

        switch (sign) {
            case '>':
                return (item) => {
                    return item[column] <= value
                }
            case '<':
                return (item) => {
                    return item[column] >= value
                }
            case '>=':
                return (item) => {
                    return item[column] < value
                }
            case '<=':
                return (item) => {
                    return item[column] > value
                }
            default:
                return (item) => {
                    return item[column] !== value
                }
        }
    }

    /*This is for SELECT column_1, column_2 FROM -queries*/
    selectColumnsFromTable(command) {
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
