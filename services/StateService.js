const _ = require('lodash')
const {
    executeStringFunction,
    executeAggregateFunction,
    executeSelectDistinct,
} = require('./components/functions')
const {
    evaluateExpression,
    evaluateAggregateExpression,
    containsAggregateFunction,
} = require('./components/expressionTools')
const { createFilter } = require('./components/filterTools')

class StateService {
    constructor(state) {
        this.state = state
    }

    /**
     * Handles updating state based on the given command.
     * Returns:
     *   - if the update was successful either a result object of form { result: result } or
     *      { result: result, rows: [] } for SELECT commands
     *   - id the commans is not executable successfully an error object of form { error: error }
     *     is returned
     * @param {object} command command object
     */
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

    /**
     * Handles updating the state according to a CREATE TABLE command.
     * Returns error object if the table name given in command already exists or
     * if some of the columns to be created have duplicate names.
     * Returns result object if the table was added to existing tables successfully.
     * @param {object} command CREATE TABLE command object
     */
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

    /**
     * Handles updating the state according to a INSERT INTO command.
     * Returns error object if the table name given in command does not exist
     * or when attempting to add values of wrong data type into columns.
     * Returns result object if the row was added to the table successfully.
     * @param {object} command INSERT INTO command object
     */
    insertIntoTable(command) {
        const error = this.checkIfTableExists(command.tableName)
        if (error) return { error: error }

        const table = this.findTable(command.tableName)

        const highest_id =
            table.rows.length === 0 ? 0 : _.maxBy(table.rows, 'id').id
        const newRow = {
            id: highest_id + 1,
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
            result: `INSERT INTO ${command.tableName} -query was executed succesfully`,
        }
    }

    /**
     * Handles SELECT commands.
     * Returns error object if the table name given in command does not exist
     * or the commans is not executable (for example MAX is called for a nonexitent
     * column).
     * Returns result object of form { result: result, rows: [] } if successful.
     * @param {object} command SELECT command object
     */
    selectFrom(command) {
        const error = this.checkIfTableExists(command.tableName)
        if (error) return { error: error }

        const table = this.findTable(command.tableName)
        let rows = table.rows

        if (command.where) {
            rows = this.filterRows(command.where.conditions, rows)
        }

        rows = this.createAdvancedRows(command, rows)
        if (rows.error) return rows

        if (command.orderBy) {
            rows = this.orderRowsBy(command.orderBy.fields, rows)
        }

        let result
        if (command.fields[0].type === 'distinct') {
            result = `SELECT DISTINCT ${command.fields[0].value
                .map((c) => (c.type === 'expression' ? c.stringValue : c.value))
                .join(', ')} FROM ${
                command.tableName
            } -query executed successfully`
        } else {
            result = `SELECT ${command.fields
                .map((c) => c.value)
                .join(', ')} FROM ${
                command.tableName
            } -query executed successfully`
        }
        return {
            result,
            rows,
        }
    }

    /**
     * Handles updating the state according to a UPDATE command.
     * Returns error object if the table name given in command does not exist
     * or when attempting to add values of wrong data type into columns.
     * Returns result object if the rows of the table were updated successfully.
     * @param {object} command UPDATE command object
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

        /* If command object has property 'where' it's filtered in order
         * to update only the rows that are spesified in queries[i]
         */
        if (command.where) {
            rowsToUpdate = this.filterRows(
                command.where.conditions,
                rowsToUpdate
            )
            let notChangedRows = _.difference(table.rows, rowsToUpdate)
            notChangedRows.forEach((row) => newRows.push(row))
        }

        rowsToUpdate.forEach((row) => {
            newRows.push(this.updateRow(row, command.columns, table.columns))
        })

        newRows = _.sortBy(newRows, 'id')
        this.state.replaceRows(command.tableName, newRows)

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

    /**
     * Handles updating the state according to a DELETE command.
     * Returns error object if the table name given in command does not exist.
     * Returns result object if the required rows were successfully deleted from table.
     * @param {object} command DELETE command object
     */
    deleteFromTable(command) {
        const error = this.checkIfTableExists(command.tableName)
        if (error) return { error: error }

        const table = this.findTable(command.tableName)
        let rows = table.rows

        if (command.where) {
            const rowsToDelete = this.filterRows(command.where.conditions, rows)
            rows = _.difference(rows, rowsToDelete)
        } else {
            rows = []
        }

        this.state.replaceRows(command.tableName, rows)

        const result = command.where
            ? `Requested rows from table ${command.tableName} deleted succesfully`
            : `All rows from table ${command.tableName} deleted succesfully`

        return {
            result,
        }
    }

    /**
     * Handles filtering of the given rows according to the given conditions.
     * @param {object} conditions conditions to filter by
     * @param {object[]} rows rows to filter
     * @returns {object[]} rows matching the conditions
     */
    filterRows(conditions, rows) {
        const filteredAndRows =
            conditions.AND.length > 0
                ? this.filterAndRows(conditions.AND, rows)
                : rows

        const filteredOrRows =
            conditions.OR.length > 0
                ? this.filterOrRows(conditions.OR, rows)
                : rows

        const andRows = _.chain(filteredAndRows).flattenDeep().uniq().value()
        const orRows = _.chain(filteredOrRows).flattenDeep().uniq().value()

        return _.intersection(andRows, orRows)
    }

    /**
     * Handles re-ordering the given rows according to given field information
     * @param {object[]} fields array of field objects
     * @param {object[]} rows array of row objects
     */
    orderRowsBy(fields, rows) {
        const arrayOfColumnNames = fields.map((f) => f.value)
        const arrayOfOrderingKeywords = fields.map((f) => f.order.value)

        const orderedRows = _.orderBy(
            rows,
            arrayOfColumnNames,
            arrayOfOrderingKeywords
        )

        return orderedRows
    }

    /**
     * Handles creating result rows for SELECT command.
     * @param {object} command SELECT command object
     * @param {object[]} existingRows array of row objects
     */
    createAdvancedRows(command, existingRows) {
        if (command.fields[0].type === 'all') {
            return existingRows
        }

        if (command.fields[0].type === 'distinct') {
            const queriedColumns = command.fields[0].value
            const rows = this.createQueriedRows(queriedColumns, existingRows)
            return executeSelectDistinct(rows)
        }

        if (
            command.fields[0].type === 'aggregateFunction' &&
            command.fields.length === 1
        ) {
            const functionResult = this.createAggregateFunctionRow(
                command.fields[0],
                existingRows
            )
            return functionResult.error
                ? functionResult
                : [
                      this.createAggregateFunctionRow(
                          command.fields[0],
                          existingRows
                      ),
                  ]
        }

        return this.createQueriedRows(command.fields, existingRows)
    }

    /**
     * Handles filtering of the given rows according to the given conditions
     * for filterRows().
     * @param {object} conditions conditions to filter by
     * @param {object[]} existingRows rows to filter
     * @returns {object[]} rows matching the conditions
     */
    filterAndRows(conditions, existingRows) {
        const filteredRows = Object.values(conditions).reduce(
            (rowsToReturn, condition) => {
                if (condition.AND && condition.OR) {
                    const filteredAnd = this.filterAndRows(
                        condition.AND,
                        existingRows
                    )

                    const filteredOr = this.filterOrRows(
                        condition.OR,
                        rowsToReturn
                    )

                    return _.intersection(filteredAnd, filteredOr)
                }

                return _.filter(rowsToReturn, (row) =>
                    createFilter(row, condition)
                )
            },
            existingRows
        )

        return filteredRows
    }

    /**
     * Handles filtering of the given rows according to the given conditions
     * for filterRows().
     * @param {object} conditions conditions to filter by
     * @param {object[]} existingRows rows to filter
     * @returns {object[]} rows matching the conditions
     */
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
                        createFilter(row, condition)
                    )
                }

                return rowsToReturn.concat(filtered)
            },
            []
        )

        return filteredRows
    }

    /**
     * Handles creating result rows for createAdvancedRows().
     * @param {object} queries command.fields of SELECT command
     * @param {object[]} existingRows array of row objects
     */
    createQueriedRows(queries, existingRows) {
        const createdRows = existingRows.reduce((rowsToReturn, row) => {
            const newRow = {}

            for (let i = 0; i < queries.length; i++) {
                if (queries[i].type === 'column') {
                    const valueOfQueriedColumn = row[queries[i].value]
                    if (valueOfQueriedColumn) {
                        newRow[queries[i].value] = row[queries[i].value]
                    } else {
                        newRow.error = `no such column ${queries[i].value}`
                    }
                } else if (
                    queries[i].type === 'expression' &&
                    containsAggregateFunction(queries[i].value)
                ) {
                    const evaluated = evaluateAggregateExpression(
                        queries[i].value,
                        existingRows
                    )
                    newRow[queries[i].stringValue] = evaluated
                    return [{ [queries[i].stringValue]: evaluated }]
                } else if (queries[i].type === 'expression') {
                    const expressionResult = evaluateExpression(
                        queries[i].value,
                        row,
                        existingRows
                    )

                    newRow[queries[i].stringValue] = expressionResult
                } else if (queries[i].type === 'stringFunction') {
                    const functionResult = executeStringFunction(
                        queries[i],
                        row
                    )

                    functionResult.error
                        ? (newRow.error = functionResult.error)
                        : (newRow[queries[i].value] = functionResult)
                } else if (queries[i].type === 'aggregateFunction') {
                    const functionResult = this.createAggregateFunctionRow(
                        queries[i],
                        existingRows
                    )

                    functionResult.error
                        ? (newRow.error = functionResult.error)
                        : (newRow[queries[i].value] =
                              functionResult[queries[i].value])
                }
            }

            rowsToReturn.push(newRow)

            return rowsToReturn
        }, [])

        const errorRows = createdRows.filter((r) => r.error)
        if (errorRows.length > 0) return errorRows[0]

        return createdRows
    }

    /**
     * Handles creating a result row from an aggragate function.
     * @param {object} functionField object containing the function information
     * @param {object[]} existingRows array of row objects
     */
    createAggregateFunctionRow(functionField, existingRows) {
        const executedFunction = executeAggregateFunction(
            functionField,
            existingRows
        )
        return executedFunction.error
            ? executedFunction
            : { [functionField.value]: executedFunction }
    }

    /**
     * Checks whether a table exists with the table name given in the command.
     * Also checks if there are duplicate column names.
     * Returns either an error message or nothing.
     * @param {object} command CREATE TABLE command object
     */
    checkCreateTableErrors(command) {
        if (this.state.tableExists(command.tableName)) {
            return `Table ${command.tableName} already exists`
        }

        const nameArray = command.columns.map((e) => e.name)

        const duplicates = this.findDuplicates(nameArray)

        if (duplicates.length !== 0)
            return duplicates.map((e) => `duplicate column ${e}: ${e}`)
    }

    /**
     * Checks whether a table by the given name already exists.
     * Returns either an error message or nothing.
     * @param {String} tableName name to be searched for
     */
    checkIfTableExists(tableName) {
        if (!this.state.tableExists(tableName))
            return `No such table ${tableName}`
    }

    /**
     * Checks an array for duplicate values and returns an array of them.
     * @param {Array} arr array to be checked
     */
    findDuplicates(arr) {
        return arr.filter((item, index) => arr.indexOf(item) !== index)
    }

    /**
     * Retrieves and returns a table object with a name matching the given one.
     * Returns undefined if no such table exists.
     * @param {String} tableName the name of the wanted table
     */
    findTable(tableName) {
        return this.state.getTableByName(tableName)
    }
}

module.exports = StateService
