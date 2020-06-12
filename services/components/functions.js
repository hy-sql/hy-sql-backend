const _ = require('lodash')

const executeStringFunction = (functionDetails, row) => {
    switch (functionDetails.name) {
        case 'LENGTH':
            return functionDetails.param.type === 'column'
                ? row[functionDetails.param.value].toString().length
                : functionDetails.param.value.toString().length
        case 'CONCAT':
            return 'function not implemented yet'
        case 'SUBSTRING':
            return 'function not implemented yet'
    }
}

const executeAggregateFunction = (functionDetails, rows) => {
    const paramValue = functionDetails.param.value

    switch (functionDetails.name) {
        case 'AVG':
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
            return _.sumBy(rows, paramValue)
                ? _.sumBy(rows, paramValue)
                : {
                      error:
                          'Parameter given to SUM does not match any existing column',
                  }
    }
}

module.exports = { executeStringFunction, executeAggregateFunction }
