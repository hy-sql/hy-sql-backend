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
const SQLError = require('../models/SQLError')

class StateService {
    constructor(state) {
        this.state = state
        this._id = 0
    }

    /**
     * Handles updating state based on the given command.
     * Returns:
     *   - if the update was successful either a result object of form { result: result } or
     *      { result: result, rows: [] } for SELECT commands
     *   - if the command is not executed successfully an error is thrown
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
     * Throws an error if the table name given in command already exists or
     * if some of the columns to be created have duplicate names.
     * Returns result object if the table was added to existing tables successfully.
     * @param {object} command CREATE TABLE command object
     */
    createTable(command) {
        this.checkCreateTableErrors(command)

        const newTable = {
            name: command.tableName,
            columns: command.columns,
            rows: [],
        }

        this.validatePrimaryKey(newTable)

        this.state.createTable(newTable)

        return { result: `Table ${newTable.name} created successfully` }
    }

    validatePrimaryKey(table) {
        const columnsContainingPrimaryKeyConstraint = table.columns.filter(
            (c) => c.constraints.includes('PRIMARY KEY')
        )

        if (columnsContainingPrimaryKeyConstraint.length > 1)
            throw new SQLError(
                `table ${table.name} has more than one primary key`
            )

        return
    }

    /**
     * Handles updating the state according to a INSERT INTO command.
     * Throws an error if the table name given in command does not exist
     * or when attempting to add values of wrong data type into columns.
     * Returns result object if the row was added to the table successfully.
     * @param {object} command INSERT INTO command object
     */
    insertIntoTable(command) {
        this.checkIfTableExists(command.tableName)

        const table = this.findTable(command.tableName)

        const newRow = command.columns.reduce(
            (rowToReturn, column, i) => {
                const columnName = column.value
                const value = command.values[i].value
                const valueType = command.values[i].type.toUpperCase()

                const existingColumn = _.find(
                    table.columns,
                    (column) => column.name === columnName
                )

                if (!existingColumn) {
                    throw new SQLError(`No such column: ${columnName}`)
                }

                const columnType = existingColumn.type

                if (columnType !== valueType) {
                    throw new SQLError(
                        `Wrong datatype: expected ${columnType} but was ${valueType}`
                    )
                }

                rowToReturn[columnName] = value

                return rowToReturn
            },
            {
                id: this.generateId(),
            }
        )

        this.state.insertIntoTable(command.tableName, newRow)

        return {
            result: `INSERT INTO ${command.tableName} -query was executed successfully`,
        }
    }

    /**
     * Handles SELECT commands. Throws an error if the table name given in command
     * does not exist or the command is not executable (for example MAX is called
     * for a nonexitent column).
     * Returns result object of form { result: result, rows: [] } if successful.
     * @param {object} command SELECT command object
     */
    selectFrom(command) {
        this.checkIfTableExists(command.tableName)

        const table = this.findTable(command.tableName)

        let selectedRows = this.selectRows(command, table)

        if (command.limit) {
            selectedRows = this.limitRows(command.limit, selectedRows)
        }

        let result

        if (command.fields[0].type === 'distinct') {
            result = `SELECT DISTINCT ${command.fields[0].value
                .map((c) => c.value)
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
            rows: selectedRows,
        }
    }

    /**
     * Handles updating the state according to a UPDATE command.
     * Throws an error if the table name given in command does not exist
     * or when attempting to add values of wrong data type into columns.
     * Returns result object if the rows of the table were updated successfully.
     * @param {object} command UPDATE command object
     */
    updateTable(command) {
        this.checkIfTableExists(command.tableName)

        const table = this.findTable(command.tableName)

        /* Check if trying to update wrong type of data to column */
        command.columns.forEach((pair) => {
            const column = _.find(table.columns, { name: pair.columnName })
            if (column.type !== pair.valueType)
                throw new SQLError(
                    `Wrong datatype: expected ${column.type} but was ${pair.valueType}`
                )
        })

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
            const notChangedRows = _.difference(table.rows, rowsToUpdate)
            newRows = newRows.concat(notChangedRows)
        }

        const updatedRows = rowsToUpdate.map((row) =>
            this.updateRow(command.columns, row)
        )

        newRows = newRows.concat(updatedRows)

        newRows = _.sortBy(newRows, 'id')
        this.state.replaceRows(command.tableName, newRows)

        const result = `Rows in table ${command.tableName} updated`

        return { result }
    }

    /**
     * Help function for updateTable(). Updates wanted columns in given row.
     * @param {*} row Row object
     * @param {*} columnsToUpdate Object that contains columnName and value which will be updated
     */
    updateRow(columnsToUpdate, row) {
        const updatedRow = { ...row }

        columnsToUpdate.forEach((column) => {
            const columnName = column.columnName
            const value = column.value
            updatedRow[columnName] = value
        })

        return updatedRow
    }

    /**
     * Handles updating the state according to a DELETE command.
     * Throws an error if the table name given in command does not exist.
     * Returns result object if the required rows were successfully deleted from table.
     * @param {object} command DELETE command object
     */
    deleteFromTable(command) {
        this.checkIfTableExists(command.tableName)

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
     * Goes through all possible select command options and returns selected fields
     * @param {Object} command
     * @param {Array} existingRows
     */
    selectRows(command, table) {
        const selectedFields =
            command.fields[0].type === 'distinct'
                ? command.fields[0].value
                : command.fields

        const columnsToReturn = this.getcolumnsToReturn(selectedFields, table)

        const existingRows = table.rows

        const filteredRows = command.where
            ? this.filterRows(command.where.conditions, existingRows)
            : existingRows

        const rowsWithNewFields = this.createRowsWithNewFields(
            selectedFields,
            filteredRows
        )

        const initialGroupedRows = command.groupBy
            ? this.initialGroupRowsBy(command.groupBy.fields, rowsWithNewFields)
            : rowsWithNewFields

        const havingRows = command.having
            ? initialGroupedRows.map((group) =>
                  this.filterRows(command.having.conditions, group)
              )
            : initialGroupedRows

        if (command.groupBy) {
            this.checkExistanceOfColumnsAsked(
                command.groupBy.fields,
                table,
                columnsToReturn
            )
        }

        const groupedRowsWithNewFieldsAndFunctions = command.groupBy
            ? havingRows
                  .map((rowGroup) =>
                      this.createRowsWithFunctionResults(
                          selectedFields,
                          rowGroup
                      )
                  )
                  .filter((row) => row.length > 0)
            : this.createRowsWithFunctionResults(selectedFields, havingRows)

        if (command.orderBy) {
            this.checkExistanceOfColumnsAsked(
                command.orderBy.fields,
                table,
                columnsToReturn
            )
        }

        const aggregateFunctionRows = command.groupBy
            ? this.groupRowsBy(
                  command.groupBy.fields,
                  _.flatten(
                      _.compact(
                          groupedRowsWithNewFieldsAndFunctions.map((rowGroup) =>
                              this.pickAggregateFunctionRow(
                                  selectedFields,
                                  rowGroup
                              )
                          )
                      )
                  )
              )
            : this.pickAggregateFunctionRow(
                  selectedFields,
                  groupedRowsWithNewFieldsAndFunctions
              )

        const orderedAggregateFunctionRows = command.orderBy
            ? this.orderRowsBy(command.orderBy.fields, aggregateFunctionRows)
            : aggregateFunctionRows

        if (!_.isEmpty(orderedAggregateFunctionRows)) {
            const aggregateFunctionRowsWithcolumnsToReturn = orderedAggregateFunctionRows.map(
                (row) => _.pick(row, columnsToReturn)
            )

            return aggregateFunctionRowsWithcolumnsToReturn
        }

        const orderedRows = command.orderBy
            ? this.orderRowsBy(
                  command.orderBy.fields,
                  _.flatten(groupedRowsWithNewFieldsAndFunctions)
              )
            : _.flatten(groupedRowsWithNewFieldsAndFunctions)

        const rowsWithInitiallycolumnsToReturn = command.groupBy
            ? orderedRows.map((row) => _.pick(row, columnsToReturn))
            : orderedRows

        const groupedRows = command.groupBy
            ? this.groupRowsBy(
                  command.groupBy.fields,
                  rowsWithInitiallycolumnsToReturn
              )
            : rowsWithInitiallycolumnsToReturn

        const rowsWithcolumnsToReturn = groupedRows.map((row) =>
            _.pick(row, columnsToReturn)
        )

        const groupedOrderedRows =
            command.groupBy && command.orderBy
                ? this.orderRowsBy(
                      command.orderBy.fields,
                      rowsWithcolumnsToReturn
                  )
                : rowsWithcolumnsToReturn

        return command.fields[0].type === 'distinct'
            ? this.selectDistinct(groupedOrderedRows)
            : groupedOrderedRows
    }

    /**
     * Creates an array of columns to return by select command
     * @param {Array} fields
     * @param {Object} table
     * @returns {Array} string array of columns
     */
    getcolumnsToReturn(fields, table) {
        const listOfAllTableColumns = table.columns.map((column) => column.name)

        const fieldValues = fields.map((f) => {
            if (f.type === 'all') {
                return listOfAllTableColumns
            }

            return f.value
        })

        return _.uniq(_.flatten(fieldValues))
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

                if (
                    condition.left.type === 'aggregateFunction' ||
                    condition.right.type === 'aggregateFunction' ||
                    (condition.left.type === 'expression' &&
                        containsAggregateFunction(
                            condition.left.expressionParts
                        )) ||
                    (condition.right === 'expression' &&
                        containsAggregateFunction(
                            condition.right.expressionParts
                        ))
                ) {
                    const aggregateFunctionFilter = createFilter(
                        rowsToReturn,
                        condition
                    )
                    return _.filter(rowsToReturn, () => aggregateFunctionFilter)
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
                    if (
                        condition.left.type === 'aggregateFunction' ||
                        condition.right.type === 'aggregateFunction' ||
                        (condition.left.type === 'expression' &&
                            containsAggregateFunction(
                                condition.left.expressionParts
                            )) ||
                        (condition.right === 'expression' &&
                            containsAggregateFunction(
                                condition.right.expressionParts
                            ))
                    ) {
                        const aggregateFunctionFilter = createFilter(
                            existingRows,
                            condition
                        )

                        filtered = _.filter(
                            existingRows,
                            () => aggregateFunctionFilter
                        )
                    } else {
                        filtered = _.filter(existingRows, (row) =>
                            createFilter(row, condition)
                        )
                    }
                }

                return rowsToReturn.concat(filtered)
            },
            []
        )

        return filteredRows
    }

    /**
     * Executes SELECT DISTINCT command for given rows. Expects rows containing only
     * queried columns as input and filters out possible duplicate data. Returns only
     * unique rows.
     * @param {object[]} rows Rows containing queried columns.
     */
    selectDistinct(rows) {
        return _.uniqWith(rows, _.isEqual)
    }

    /**
     * Handles creating arithmetic expressions result rows for selectRows()
     * and checks the existance of queried columns, throws error if not found
     * @param {object} command command of SELECT command
     * @param {object[]} existingRows array of row objects
     */
    createRowsWithNewFields(fields, existingRows) {
        const createdRows = existingRows.reduce((rowsToReturn, row) => {
            let newRow = { ...row }

            fields.forEach((field) => {
                if (field.type === 'column') {
                    const valueOfQueriedColumn = row[field.value]
                    // eslint-disable-next-line eqeqeq
                    if (valueOfQueriedColumn == null) {
                        throw new SQLError(`no such column ${field.value}`)
                    }

                    newRow[field.value] = valueOfQueriedColumn
                } else if (
                    field.type === 'expression' &&
                    containsAggregateFunction(field.expressionParts)
                ) {
                    const evaluated = evaluateAggregateExpression(
                        field.expressionParts,
                        existingRows
                    )

                    newRow[field.value] = evaluated
                } else if (field.type === 'expression') {
                    const expressionResult = evaluateExpression(
                        field.expressionParts,
                        row,
                        existingRows
                    )

                    newRow[field.value] = expressionResult
                }
            })

            return rowsToReturn.concat(newRow)
        }, [])

        return createdRows
    }

    /**
     * Handles creating string and aggregate function result rows for selectRows().
     * @param {object} command command of SELECT command
     * @param {object[]} existingRows array of row objects
     */
    createRowsWithFunctionResults(fields, existingRows) {
        const createdRows = existingRows.reduce((rowsToReturn, row) => {
            fields.forEach((field) => {
                if (field.type === 'stringFunction') {
                    const functionResult = executeStringFunction(field, row)

                    row[field.value] = functionResult
                } else if (field.type === 'aggregateFunction') {
                    const functionResult = this.createRowWithAggregateFunctionResult(
                        field,
                        existingRows
                    )

                    row[field.value] = functionResult[field.value]
                }
            })

            return rowsToReturn
        }, existingRows)

        return createdRows
    }

    /**
     * Picks last executed aggregate function result from rows
     * @param {Array} rows
     * @param {Array} fields
     */
    pickAggregateFunctionRow(fields, rows) {
        const lastMinMaxFunction = _.findLast(
            fields,
            (field) =>
                field.type === 'aggregateFunction' &&
                (field.name === 'MIN' || field.name === 'MAX')
        )

        if (lastMinMaxFunction) {
            const filteredRows = _.filter(rows, {
                [lastMinMaxFunction.param.value]: executeAggregateFunction(
                    lastMinMaxFunction,
                    rows
                ),
            })

            return [filteredRows[0]]
        }

        const lastExpressionIncludingMinMaxFunction = _.findLast(
            fields,
            (field) => {
                if (field.type === 'expression') {
                    return _.findLast(
                        field.expressionParts,
                        (part) =>
                            part.type === 'aggregateFunction' &&
                            (part.name === 'MIN' || part.name === 'MAX')
                    )
                }
            }
        )

        if (lastExpressionIncludingMinMaxFunction) {
            const lastMinMaxFunctionInExpression = _.findLast(
                lastExpressionIncludingMinMaxFunction.expressionParts,
                (field) =>
                    field.type === 'aggregateFunction' &&
                    (field.name === 'MIN' || field.name === 'MAX')
            )

            if (lastMinMaxFunctionInExpression) {
                const filteredRows = _.filter(rows, {
                    [lastMinMaxFunctionInExpression.param
                        .value]: executeAggregateFunction(
                        lastMinMaxFunctionInExpression,
                        rows
                    ),
                })

                return [filteredRows[0]]
            }
        }

        const lastOtherAggregateFunction = _.findLast(
            fields,
            (field) => field.type === 'aggregateFunction'
        )

        const lastExpressionIncludingOtherAggregateFunction = _.findLast(
            fields,
            (field) => {
                if (field.type === 'expression') {
                    return _.findLast(
                        field.expressionParts,
                        (part) => part.type === 'aggregateFunction'
                    )
                }
            }
        )

        if (
            lastOtherAggregateFunction ||
            lastExpressionIncludingOtherAggregateFunction
        ) {
            return [rows[0]]
        }

        return null
    }

    /**
     * Helper function, groups rows for aggregate function evaluation
     * @param {Array} rows
     * @param {Array} fields
     */
    initialGroupRowsBy(fields, rows) {
        return _.flattenDepth(
            groupByMultipleProps(
                rows,
                fields.map((f) => f.value)
            ),
            fields.length - 1
        )
    }

    /**
     * Groups rows for select command
     * @param {Array} rows
     * @param {Array} fields
     */
    groupRowsBy(fields, rows) {
        const grouped = _.flattenDepth(
            groupByMultipleProps(
                rows,
                fields.map((f) => f.value)
            ),
            fields.length - 1
        ).map((group) => group.slice(0, 1))

        return _.orderBy(
            _.flatten(grouped),
            fields.map((f) => f.value)
        )
    }

    /**
     * Handles creating a result row from an aggregate function.
     * @param {object} functionField object containing the function information
     * @param {object[]} existingRows array of row objects
     */
    createRowWithAggregateFunctionResult(functionField, existingRows) {
        const executedFunction = executeAggregateFunction(
            functionField,
            existingRows
        )

        return { [functionField.value]: executedFunction }
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

    checkExistanceOfColumnsAsked(fields, table, selectedColumns) {
        const existingColumns = table.columns.map((c) => c.name)

        const allColumns = _.uniq(existingColumns.concat(selectedColumns))

        fields.forEach((field) => {
            if (
                (field.type === 'stringFunction' ||
                    field.type === 'aggregateFunction' ||
                    field.type === 'expression') &&
                !_.includes(allColumns, field.value)
            ) {
                throw new SQLError(
                    'functions and expressions in order by not implemented yet'
                )
            } else if (!_.includes(allColumns, field.value)) {
                throw new SQLError(`no such column: ${field.value}`)
            }
        })

        return
    }

    /**
     * Handles narrowing down the amount of rows to be returned according to the values
     * given to LIMIT ... OFFSET in a query.
     * @param {object} limitObject .limit of a command object
     * @param {object[]} rows rows to limit
     */
    limitRows(limitObject, rows) {
        let limitedRows = rows

        let offset = undefined
        if (limitObject.offset) {
            offset =
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

        if (limit < 0 || offset < 0) {
            throw new SQLError('Value given to LIMIT or OFFSET is negative.')
        }

        if (limitedRows.length === 0) {
            throw new SQLError(
                'No rows left to return. Try changing value given to LIMIT or OFFSET.'
            )
        }

        return limitedRows
    }

    /**
     * Generates id
     */
    generateId() {
        return ++this._id
    }

    /**
     * Retrieves and returns a table object with a name matching the given one.
     * Returns undefined if no such table exists.
     * @param {String} tableName the name of the wanted table
     */
    findTable(tableName) {
        return this.state.getTableByName(tableName)
    }

    /**
     * Checks whether a table by the given name already exists.
     * Throws an error message or returns nothing.
     * @param {String} tableName name to be searched for
     */
    checkIfTableExists(tableName) {
        if (!this.state.tableExists(tableName))
            throw new SQLError(`No such table ${tableName}`)
    }

    /**
     * Checks whether a table exists with the table name given in the command.
     * Also checks if there are duplicate column names.
     * Returns either an error message or nothing.
     * @param {object} command CREATE TABLE command object
     */
    checkCreateTableErrors(command) {
        if (this.state.tableExists(command.tableName))
            throw new SQLError(`Table ${command.tableName} already exists`)

        const nameArray = command.columns.map((e) => e.name)

        const duplicates = this.findDuplicates(nameArray)

        if (duplicates.length !== 0)
            throw new SQLError(
                `duplicate column ${duplicates[0]}: ${duplicates[0]}`
            )
    }

    /**
     * Checks an array for duplicate values and returns an array of them.
     * @param {Array} arr array to be checked
     */
    findDuplicates(arr) {
        return arr.filter((item, index) => arr.indexOf(item) !== index)
    }
}

module.exports = StateService
