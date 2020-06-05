const _ = require('lodash')
const { calculateExpression } = require('../utils/calculateExpression')
const {
    executeStringFunction,
    executeAggregateFunction,
} = require('../utils/functions')

class StateService {
    constructor(state) {
        this.state = state
    }

    updateState(command) {
        switch (command.name) {
            case 'CREATE TABLE':
                return this.createTable(command)
            case 'INSERT INTO':
                return this.insertIntoTable(command)
            case 'SELECT *':
                return this.selectAllFromTable(command)
            case 'SELECT':
                return this.selectColumnsFromTable(command)
            case 'SELECT ADVANCED':
                return this.selectAdvanced(command)
            default:
                break
        }
    }

    selectAdvanced(command) {
        const error = this.checkIfTableExists(command.tableName)
        if (error) return { error: error }

        const table = this.findTable(command.tableName)
        let existingRows = table.rows
        let rows

        rows = this.createAdvancedRows(command, existingRows)

        if (command.where) {
            const filteredRows = this.filterRows(command.where.conditions, rows)
            rows = _.chain(filteredRows).flattenDeep(filteredRows).uniq()
        }

        const result = `SELECT ${command.fields
            .map((c) => c.value)
            .join(', ')} FROM ${command.tableName} -query executed succesfully`

        return {
            result,
            rows,
        }
    }

    createAdvancedRows(command, existingRows) {
        if (command.fields[0].type === 'all') {
            return existingRows
        }

        if (command.fields[0].type === 'aggregateFunction') {
            return this.createAggregateFunctionRow(
                command.fields[0],
                existingRows
            )
        }

        return this.createQueriedRows(command.fields, existingRows)
    }

    filterRows(conditions, existingRows) {
        const filteredRows = conditions.reduce((rowsToReturn, condition) => {
            return _.filter(existingRows, (row) =>
                this.createAdvancedFilter(row, condition)
            )
        }, existingRows)

        return filteredRows
    }

    createQueriedRows(queries, existingRows) {
        return existingRows.reduce((rowsToReturn, row) => {
            const newRow = {}

            queries.forEach((query) => {
                query
                if (query.type === 'column') {
                    newRow[query.value] = row[query.value]
                } else if (query.type === 'expression') {
                    const context = {}

                    query.columns.map((column) => {
                        column
                        context[column] = row[column]
                    })

                    newRow[query.value] = calculateExpression(
                        query.value,
                        context
                    )
                } else if (query.type === 'stringFunction') {
                    newRow[query.value] = executeStringFunction(query, row)
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

    createAdvancedFilter(row, condition) {
        console.log('HELLO FILTER')
        switch (condition.operator) {
            case '=':
                return (
                    this.evaluateCondition(condition.left, row) ===
                    this.evaluateCondition(condition.right, row)
                )
            case '>':
                return (
                    this.evaluateCondition(condition.left, row) >
                    this.evaluateCondition(condition.right, row)
                )
            case '<':
                return (
                    this.evaluateCondition(condition.left, row) <
                    this.evaluateCondition(condition.right, row)
                )
            case '>=':
                return (
                    this.evaluateCondition(condition.left, row) >=
                    this.evaluateCondition(condition.right, row)
                )
            case '<=':
                return (
                    this.evaluateCondition(condition.left, row) <=
                    this.evaluateCondition(condition.right, row)
                )
            case '<>':
                return (
                    this.evaluateCondition(condition.left, row) !==
                    this.evaluateCondition(condition.right, row)
                )
        }
    }

    evaluateCondition(condition, row) {
        const context = {}
        switch (condition.type) {
            case 'expression':
                condition.columns.map((column) => {
                    column
                    context[column] = row[column]
                })
                return (row[condition.columns[0]] = calculateExpression(
                    condition.value,
                    context
                ))
            case 'stringFunction':
                return executeStringFunction(condition, row)
            case 'string':
                return condition.value
            case 'integer':
                return condition.value
            case 'column':
                return row[condition.value]
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
