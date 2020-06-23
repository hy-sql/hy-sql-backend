const _ = require('lodash')
const SQLError = require('../../models/SQLError')

/**
 * Handles string functions. Expected input format of functionFields:
 *    { type: type, name: name, value: value, param: { paramDetails } }
 *
 * Returns:
 *   - functionFields.name === 'LENGTH': length of param.value or length of value
 *         in param.value column in input row. If the parameter is a column that
 *         does not exist an error is thrown.
 * @param {object} functionFields object containing function details
 * @param {object} row table row object
 */
const executeStringFunction = (functionFields, row) => {
    switch (functionFields.name) {
        case 'LENGTH':
            if (functionFields.param.type === 'column') {
                const columnValue = row[functionFields.param.value]
                if (!columnValue && columnValue !== 0)
                    throw new SQLError(
                        'Column name given to LENGTH as parameter does not match any existing column'
                    )

                return columnValue.toString().length
            }
            return functionFields.param.value.toString().length
        case 'CONCAT':
            return 'function not implemented yet'
        case 'SUBSTRING':
            return 'function not implemented yet'
    }
}

/**
 * Handles sql aggregate functions. Expected input format of functionFields:
 * { type: type, name: name, value: value, param: { paramDetails } }
 *
 * Return value depends on the fields of functionFields:
 *   - name === 'AVG': If param.value does not match any existing columns an error is thrown.
 *         If it matches a column of type TEXT, 0 is returned.
 *         Otherwise average of the values in the matched column is returned.
 *   - name === 'COUNT': returns rows.length.
 *   - name === 'MAX': If param.value does not match any existing columns an error is thrown.
 *         Otherwise the highest of the values in the matched column is returned.
 *   - name === 'MIN': If param.value does not match any existing columns an error is thrown.
 *         Otherwise the lowest of the values in the matched column is returned.
 *   - name === 'SUM': If param.value does not match any existing columns an error is thrown.
 *         If it matches a column of type TEXT, 0 is returned.
 *         Otherwise sum of the values in the matched column is returned.
 * @param {object} functionFields object containing function details
 * @param {object[]} rows array of table rows
 */
const executeAggregateFunction = (functionFields, rows) => {
    const paramValue = functionFields.param.value

    switch (functionFields.name) {
        case 'AVG': {
            if (_.isString(_.get(rows[0], paramValue))) {
                return 0
            }

            const result = _.meanBy(rows, paramValue)

            if (!result && result !== 0)
                throw new SQLError(
                    'Parameter given to AVG does not match any existing column'
                )

            return result
        }
        case 'COUNT':
            return functionFields.param.type === 'all'
                ? rows.length
                : _.filter(rows, paramValue).filter(Boolean).length
        case 'MAX': {
            const result = _.get(_.maxBy(rows, paramValue), paramValue)

            if (!result && result !== 0)
                throw new SQLError(
                    'Parameter given to MAX does not match any existing column'
                )

            return result
        }
        case 'MIN': {
            const result = _.get(_.minBy(rows, paramValue), paramValue)
            if (!result && result !== 0)
                throw new SQLError(
                    'Parameter given to MIN does not match any existing column'
                )

            return result
        }
        case 'SUM': {
            if (_.isString(_.sumBy(rows, paramValue))) {
                return 0
            }

            const result = _.sumBy(rows, paramValue)

            if (!result && result !== 0)
                throw new SQLError(
                    'Parameter given to SUM does not match any existing column'
                )

            return result
        }
    }
}

/**
 * Executes SELECT DISTINCT command for given rows. Expects rows containing only
 * queried columns as input and filters out possible duplicate data. Returns only
 * unique rows.
 * @param {*} rows Rows containing queried columns.
 */
const executeSelectDistinct = (rows) => {
    return _.uniqWith(rows, _.isEqual)
}

module.exports = {
    executeStringFunction,
    executeAggregateFunction,
    executeSelectDistinct,
}
