const _ = require('lodash')
const {
    executeStringFunction,
    executeAggregateFunction,
} = require('./components/functions')
const {
    evaluateExpression,
    evaluateAggregateExpression,
    containsAggregateFunction,
} = require('./components/expressionTools')
const { createFilter } = require('./components/filterTools')
const groupByMultipleProps = require('../utils/groupByMultipleProps')

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
        const existingRows = table.rows

        let selectedRows = this.selectRows(command, existingRows)

        if (selectedRows.error) return { error: selectedRows.error }

        if (command.limit) {
            selectedRows = this.limitRows(command.limit, selectedRows)
            if (selectedRows.error) return { error: selectedRows.error }
        }

        const result = `SELECT ${command.fields
            .map((c) => c.value)
            .join(', ')} FROM ${command.tableName} -query executed successfully`

        return {
            result,
            rows: selectedRows,
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
         * to update only the rows that are spesified in command.fields[i]
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

    limitRows(limitObject, rows) {
        let limitedRows = rows

        if (limitObject.offset) {
            const offset =
                limitObject.offset.field.type === 'expression'
                    ? evaluateExpression(
                          limitObject.offset.field.expressionParts,
                          {},
                          limitedRows
                      )
                    : limitObject.offset.field.value

            limitedRows = _.drop(limitedRows, offset)
        }

        const limit =
            limitObject.field.type === 'expression'
                ? evaluateExpression(
                      limitObject.field.expressionParts,
                      {},
                      limitedRows
                  )
                : limitObject.field.value

        if (limit >= 0) limitedRows = _.take(limitedRows, limit)

        if (limitedRows.length === 0) {
            return {
                error:
                    'No rows left to return. Try changing value given to LIMIT or OFFSET.',
            }
        }

        return limitedRows
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

    createFunctionRows(command, existingRows) {
        const createdRows = existingRows.reduce((rowsToReturn, row) => {
            for (let i = 0; i < command.fields.length; i++) {
                if (command.fields[i].type === 'stringFunction') {
                    const functionResult = executeStringFunction(
                        command.fields[i],
                        row
                    )

                    functionResult.error
                        ? (row.error = functionResult.error)
                        : (row[command.fields[i].value] = functionResult)
                } else if (command.fields[i].type === 'aggregateFunction') {
                    const functionResult = this.createAggregateFunctionRow(
                        command.fields[i],
                        existingRows
                    )

                    functionResult.error
                        ? (row.error = functionResult.error)
                        : (row[command.fields[i].value] =
                              functionResult[command.fields[i].value])
                }
            }

            return rowsToReturn
        }, existingRows)

        const errorRows = createdRows.filter((r) => r.error)
        if (errorRows.length > 0) return errorRows[0]

        return createdRows
    }

    selectRows(command, existingRows) {
        const filteredRows = command.where
            ? this.filterRows(command.where.conditions, existingRows)
            : existingRows

        if (command.fields[0].type === 'all') {
            return command.orderBy
                ? this.orderRowsBy(command.orderBy.fields, filteredRows)
                : filteredRows
        }

        if (
            command.fields[0].type === 'aggregateFunction' &&
            command.fields.length === 1
        ) {
            const functionResult = this.createAggregateFunctionRow(
                command.fields[0],
                filteredRows
            )
            return functionResult.error
                ? functionResult
                : [
                      this.createAggregateFunctionRow(
                          command.fields[0],
                          filteredRows
                      ),
                  ]
        }

        const fieldsToReturn = command.fields.map((f) => f.value)

        const rowsWithNewFields = this.createRowsWithNewFields(
            command,
            filteredRows
        )

        const initialGroupedRows = command.groupBy
            ? this.initialGroupRowsBy(rowsWithNewFields, command.groupBy.fields)
            : rowsWithNewFields

        const rowsWithNewFieldsAndFunctions = command.groupBy
            ? _.flatten(
                  initialGroupedRows.map((rowGroup) =>
                      this.createFunctionRows(command, rowGroup)
                  )
              )
            : initialGroupedRows.length > 1
            ? this.createFunctionRows(command, initialGroupedRows)
            : initialGroupedRows

        if (rowsWithNewFieldsAndFunctions.error)
            return rowsWithNewFieldsAndFunctions

        const orderedRows = command.orderBy
            ? this.orderRowsBy(
                  command.orderBy.fields,
                  rowsWithNewFieldsAndFunctions
              )
            : rowsWithNewFieldsAndFunctions

        const selectedRows = orderedRows.map((row) =>
            _.pick(row, fieldsToReturn)
        )

        return command.groupBy
            ? command.orderBy
                ? this.orderRowsBy(
                      command.orderBy.fields,
                      this.groupRowsBy(selectedRows, command.groupBy.fields)
                  )
                : this.groupRowsBy(selectedRows, command.groupBy.fields)
            : selectedRows
    }

    /**
     * Groups rows for individual aggregate function evaluation
     * @param {Array} rows
     * @param {Array} fields
     */
    initialGroupRowsBy(rows, fields) {
        return _.flattenDepth(
            groupByMultipleProps(
                rows,
                fields.map((f) => f.value)
            ),
            fields.length - 1
        )
    }

    groupRowsBy(rows, fields) {
        return _.chain(rows)
            .flattenDepth(
                groupByMultipleProps(
                    rows,
                    fields.map((f) => f.value)
                ),
                fields.length - 1
            )
            .uniqWith(_.isEqual)
            .orderBy(fields.map((f) => f.value))
            .value()
    }

    /**
     * Handles creating result rows for createRowsWithNewFields().
     * @param {object} command command of SELECT command
     * @param {object[]} existingRows array of row objects
     */
    createRowsWithNewFields(command, existingRows) {
        const createdRows = existingRows.reduce((rowsToReturn, row) => {
            for (let i = 0; i < command.fields.length; i++) {
                if (command.fields[i].type === 'column') {
                    const valueOfQueriedColumn = row[command.fields[i].value]
                    if (!valueOfQueriedColumn) {
                        row.error = `no such column ${command.fields[i].value}`
                    }
                } else if (
                    command.fields[i].type === 'expression' &&
                    containsAggregateFunction(command.fields[i].expressionParts)
                ) {
                    const evaluated = evaluateAggregateExpression(
                        command.fields[i].value,
                        existingRows
                    )
                    row[command.fields[i].value] = evaluated
                    return [{ [command.fields[i].value]: evaluated }]
                } else if (command.fields[i].type === 'expression') {
                    const expressionResult = evaluateExpression(
                        command.fields[i].expressionParts,
                        row,
                        existingRows
                    )

                    row[command.fields[i].value] = expressionResult
                }
            }

            return rowsToReturn
        }, existingRows)

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
