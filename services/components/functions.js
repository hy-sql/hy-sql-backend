const _ = require('lodash')

/**
 * Handles string functions. Expected input format of functionDetails:
 *    { type: type, name: name, value: value, param: { paramDetails } }
 *
 * Returns:
 *    - functionDetails.name === 'LENGTH': length of param.value or length of value
 *          in param.value column in input row. If the parameter is a column that
 *          does not exist an error object is returned.
 * @param {Object} functionDetails object containing function details
 * @param {Object} row table row object
 */
const executeStringFunction = (functionDetails, row) => {
    switch (functionDetails.name) {
        case 'LENGTH':
            if (functionDetails.param.type === 'column') {
                return row[functionDetails.param.value]
                    ? row[functionDetails.param.value].toString().length
                    : {
                          error:
                              'Column name given to LENGTH as parameter does not match any existing column',
                      }
            }
            return functionDetails.param.value.toString().length
        case 'CONCAT':
            return 'function not implemented yet'
        case 'SUBSTRING':
            return 'function not implemented yet'
    }
}

/** Handles sql aggregate functions. Expected input format of functionDetails:
 * { type: type, name: name, value: value, param: { paramDetails } }
 *
 * Return value depends on the fields of functionDetails:
 *    - name === 'AVG': If param.value does not match any existing columns returns an error object.
 *         If it matches a column of type TEXT, 0 is returned.
 *         Otherwise average of the values in the matched column is returned.
 *    - name === 'COUNT': returns rows.length.
 *    - name === 'MAX': If param.value does not match any existing columns returns an error object.
 *         Otherwise the highest of the values in the matched column is returned.
 *    - name === 'MIN': If param.value does not match any existing columns returns an error object.
 *         Otherwise the lowest of the values in the matched column is returned.
 *    - name === 'SUM': If param.value does not match any existing columns returns an error object.
 *         If it matches a column of type TEXT, 0 is returned.
 *         Otherwise sum of the values in the matched column is returned.
 * @param {Object} functionDetails object containing function details
 * @param {Array} rows array of table rows
 */
const executeAggregateFunction = (functionDetails, rows) => {
    const paramValue = functionDetails.param.value

    switch (functionDetails.name) {
        case 'AVG':
            if (_.isString(_.get(rows[0], paramValue))) {
                return 0
            }

            return _.meanBy(rows, paramValue)
                ? _.meanBy(rows, paramValue)
                : {
                      error:
                          'Parameter given to AVG does not match any existing column',
                  }
        case 'COUNT':
            return functionDetails.param.type === 'all'
                ? rows.length
                : _.filter(rows, paramValue).filter(Boolean).length
        case 'MAX':
            return _.get(_.maxBy(rows, paramValue), paramValue, {
                error:
                    'Parameter given to MAX does not match any existing column',
            })
        case 'MIN':
            return _.get(_.minBy(rows, paramValue), paramValue, {
                error:
                    'Parameter given to MIN does not match any existing column',
            })
        case 'SUM':
            if (_.isString(_.sumBy(rows, paramValue))) {
                return 0
            }

            return _.sumBy(rows, paramValue)
                ? _.sumBy(rows, paramValue)
                : {
                      error:
                          'Parameter given to SUM does not match any existing column',
                  }
    }
}

module.exports = { executeStringFunction, executeAggregateFunction }
