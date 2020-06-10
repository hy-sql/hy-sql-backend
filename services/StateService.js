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
            case 'SELECT':
                return this.selectFrom(command)
            case 'UPDATE':
                return this.updateTable(command)
            case 'DELETE':
                return this.deleteFromTable(command)
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

        this.state.insertIntoTable(command.tableName, newRow)

        return {
            result: `INSERT INTO ${command.tableName} -queries[i] was executed succesfully`,
        }
    }

    selectFrom(command) {
        const error = this.checkIfTableExists(command.tableName)
        if (error) return { error: error }

        const table = this.findTable(command.tableName)
        let rows = table.rows

        if (command.where) {
            const filteredAndRows =
                command.where.conditions.AND.length > 0
                    ? this.filterAndRows(command.where.conditions.AND, rows)
                    : rows

            const filteredOrRows =
                command.where.conditions.OR.length > 0
                    ? this.filterOrRows(command.where.conditions.OR, rows)
                    : rows

            const andRows = _.chain(filteredAndRows)
                .flattenDeep()
                .uniq()
                .value()
            const orRows = _.chain(filteredOrRows).flattenDeep().uniq().value()

            rows = _.intersection(andRows, orRows)
        }

        rows = this.createAdvancedRows(command, rows)

        if (command.orderBy) {
            rows =
                command.orderBy.order &&
                command.orderBy.order.toUpperCase() === 'DESC'
                    ? _.orderBy(rows, [command.orderBy.columnName], ['desc'])
                    : _.orderBy(rows, [command.orderBy.columnName], ['asc'])
        }

        const result = `SELECT ${command.fields
            .map((c) => c.value)
            .join(', ')} FROM ${
            command.tableName
        } -queries[i] executed successfully`

        return {
            result,
            rows,
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
         * to update only the rows that are spesified in queries[i]
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

        newRows = _.sortBy(newRows, 'id')
        this.state.updateRows(command.tableName, newRows)

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
        let rows = table.rows

        if (command.where) {
            const filter = this.createOppositeFilter(command.where)
            rows = _.filter(rows, filter)
        } else {
            rows = []
        }

        this.state.deleteFromTable(command.tableName, rows)

        const result = command.where
            ? `Requested rows from table ${command.tableName} deleted succesfully`
            : `All rows from table ${command.tableName} deleted succesfully`

        return {
            result,
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

    filterAndRows(conditions, existingRows) {
        const filteredRows = Object.values(conditions).reduce(
            (rowsToReturn, condition) => {
                if (condition.OR) {
                    const filteredOr = this.filterOrRows(
                        condition.OR,
                        rowsToReturn
                    )

                    return _.intersection(existingRows, filteredOr)
                }
                return _.filter(rowsToReturn, (row) =>
                    this.createAdvancedFilter(row, condition)
                )
            },
            existingRows
        )

        return filteredRows
    }

    filterOrRows(conditions, existingRows) {
        const filteredRows = Object.values(conditions).reduce(
            (rowsToReturn, condition) => {
                let filtered

                if (condition.AND) {
                    filtered = this.filterAndRows(condition.AND, existingRows)
                } else if (condition.OR) {
                    filtered = this.filterOrRows(condition.OR, existingRows)
                } else {
                    filtered = _.filter(existingRows, (row) =>
                        this.createAdvancedFilter(row, condition)
                    )
                }

                return rowsToReturn.concat(filtered)
            },
            []
        )

        return filteredRows
    }

    createQueriedRows(queries, existingRows) {
        return existingRows.reduce((rowsToReturn, row) => {
            const newRow = {}

            for (let i = 0; i < queries.length; i++) {
                if (queries[i].type === 'column') {
                    newRow[queries[i].value] = row[queries[i].value]
                } else if (
                    queries[i].type === 'expression' &&
                    this.containsAggregateFunction(queries[i].value)
                ) {
                    const evaluated = this.evaluateAggregateExpression(
                        queries[i].value,
                        existingRows
                    )
                    newRow[queries[i].stringValue] = evaluated
                    return [{ [queries[i].stringValue]: evaluated }]
                } else if (queries[i].type === 'expression') {
                    const expressionResult = this.evaluateExpression(
                        queries[i].value,
                        row,
                        existingRows
                    )

                    newRow[queries[i].stringValue] = expressionResult
                } else if (queries[i].type === 'stringFunction') {
                    newRow[queries[i].value] = executeStringFunction(
                        queries[i],
                        row
                    )
                }
            }
            rowsToReturn.push(newRow)

            return rowsToReturn
        }, [])
    }

    containsAggregateFunction(expression) {
        return expression.some((e) => e.type === 'aggregateFunction')
    }

    evaluateAggregateExpression(expressionArray, rows) {
        const evaluatedExpression = expressionArray.map((e) =>
            this.evaluateAggregateExpressionPart(e, rows)
        )

        return calculateExpression(evaluatedExpression)
    }

    evaluateExpression(expressionArray, row, rows) {
        const mappedExpression = expressionArray
            .map((e) => this.evaluateExpressionPart(e, row, rows))
            .join('')

        return calculateExpression(mappedExpression)
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
        switch (condition.operator) {
            case '=':
                return (
                    // eslint-disable-next-line
                    this.evaluateExpressionPart(condition.left, row) ==
                    this.evaluateExpressionPart(condition.right, row)
                )
            case '>':
                return (
                    this.evaluateExpressionPart(condition.left, row) >
                    this.evaluateExpressionPart(condition.right, row)
                )
            case '<':
                return (
                    this.evaluateExpressionPart(condition.left, row) <
                    this.evaluateExpressionPart(condition.right, row)
                )
            case '>=':
                return (
                    this.evaluateExpressionPart(condition.left, row) >=
                    this.evaluateExpressionPart(condition.right, row)
                )
            case '<=':
                return (
                    this.evaluateExpressionPart(condition.left, row) <=
                    this.evaluateExpressionPart(condition.right, row)
                )
            case '<>':
                return (
                    // eslint-disable-next-line
                    this.evaluateExpressionPart(condition.left, row) !=
                    this.evaluateExpressionPart(condition.right, row)
                )
        }
    }

    evaluateAggregateExpressionPart(expressionPart, rows) {
        switch (expressionPart.type) {
            case 'stringFunction':
                return executeStringFunction(expressionPart, rows[0])
            case 'aggregateFunction':
                return executeAggregateFunction(expressionPart, rows)
            case 'column':
                return rows[0].column
            default:
                return expressionPart.value
        }
    }

    evaluateExpressionPart(expressionPart, row) {
        switch (expressionPart.type) {
            case 'expression': {
                const expressionResult = this.evaluateExpression(
                    expressionPart.value,
                    row
                )
                return expressionResult
            }
            case 'stringFunction':
                return executeStringFunction(expressionPart, row)
            case 'aggregateFunction':
                return executeAggregateFunction(expressionPart)
            case 'string':
                return expressionPart.value
            case 'integer':
                return expressionPart.value
            case 'column':
                return row[expressionPart.value]
            case 'operator':
                return expressionPart.value
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
                    // eslint-disable-next-line eqeqeq
                    return item[column] != value
                }
            default:
                return (item) => {
                    // eslint-disable-next-line eqeqeq
                    return item[column] == value
                }
        }
    }

    /*Creates filter that gives the opposite results than filter created with createFilter()*/
    createOppositeFilter(whereObject) {
        const column = whereObject.columnName
        const value = whereObject.value
        const sign = whereObject.sign

        switch (sign) {
            case '<>':
                return (item) => {
                    // eslint-disable-next-line
                    return item[column] == value
                }
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
                    // eslint-disable-next-line
                    return item[column] != value
                }
        }
    }

    checkCreateTableErrors(command) {
        console.log(command)
        if (this.state.tableExists(command.tableName)) {
            console.log('HELLO')
            return `Table ${command.tableName} already exists`
        }

        const nameArray = command.columns.map((e) => e.name)

        const duplicates = this.findDuplicates(nameArray)

        if (duplicates.length !== 0)
            return duplicates.map((e) => `duplicate column ${e}: ${e}`)
    }

    checkIfTableExists(tableName) {
        if (!this.state.tableExists(tableName))
            return `No such table ${tableName}`
    }

    findDuplicates(arr) {
        return arr.filter((item, index) => arr.indexOf(item) !== index)
    }

    findTable(tableName) {
        return this.state.getTableByName(tableName)
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
